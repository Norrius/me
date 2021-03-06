{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
module Handler.Consensus where

import Import
import qualified Data.List
import qualified Data.HashMap.Strict as Map
import Database.Persist.Sql (rawSql, fromSqlKey)
import Database.Persist.Class (toPersistValue)
import Data.Aeson.Types (withObject, parseMaybe)

getConsensusAllR :: Handler Html
getConsensusAllR = do
    maybeUserId <- maybeAuthId
    polls <- runDB $ selectList [] [Desc PollId]
    defaultLayout $ do
        setTitle "Consensus"
        $(widgetFile "consensus-all")

-- | Add a new poll.
postConsensusAllR :: Handler Value
postConsensusAllR = do
    request <- requireJsonBody
    let parser = withObject "request" $ \o -> o .: "name"
    let maybeName = parseMaybe parser request :: Maybe Text
    name <- maybe (invalidArgs ["`name` required"]) return maybeName

    _ <- runDB $ insert $ Poll name Ongoing
    return $ object ["success" .= True]

-- Helpers for poll state
isActionable :: Poll -> Bool
isActionable poll = pollState poll /= Finished

isOpen :: Poll -> Bool
isOpen poll = pollState poll /= Ongoing

getActionable :: PollId -> Handler Poll
getActionable pollId = do
    poll <- runDB $ get404 pollId
    if isActionable poll -- yay imperative style
    then return poll
    else invalidArgs ["poll has ended"]

-- | Produces a poll view.
getConsensusR :: PollId -> Handler Html
getConsensusR pollId = do
    maybePoll <- runDB $ selectFirst [PollId ==. pollId] []
    poll <- case maybePoll of
        Just (Entity _ record) -> return record
        _ -> notFound
    maybeUserId <- maybeAuthId

    choices' <- runDB $ selectList [ChoicePollId ==. pollId] [] -- unsorted
    allVotes' <- runDB $ case isOpen poll of
        True -> rawSql
          "select ?? \
          \from vote join choice \
            \on vote.choice_id = choice.id \
          \where choice.poll_id = ?" [toPersistValue pollId]
        False -> maybe (return []) (\userId -> rawSql -- hide votes from other users
          "select ?? \
          \from vote join choice \
            \on vote.choice_id = choice.id \
          \where choice.poll_id = ? and vote.user_id = ?" [toPersistValue pollId, toPersistValue userId]) maybeUserId
    let allVotes = map entityVal allVotes' :: [Vote]
        (myVotes, otherVotes) = partition ((maybeUserId ==) . Just . voteUserId) allVotes
        userIds = Data.List.nub $ map voteUserId otherVotes -- O(n^2)

        -- These maps are queried from the template to fill in cell values:
        myVotesMap = Map.fromList $
            map (\v -> (voteChoiceId v, voteValue v)) myVotes
        otherVotesMap = Map.fromList $
            map (\v -> ((voteChoiceId v, voteUserId v), voteValue v)) otherVotes
        totalsMap = Map.fromListWith (+) $
            map (\v -> (voteChoiceId v, weight . voteValue $ v)) allVotes

        choices = sortBy (\a b -> flip compare (total a) (total b)) choices' :: [Entity Choice]
            where
                total c = Map.lookupDefault 0 (entityKey c) totalsMap

    $logDebug . pack . show $ myVotesMap
    $logDebug . pack . show $ otherVotesMap

    users <- runDB $ selectList [UserId <-. userIds] [Asc UserId]
    defaultLayout $ do
        setTitle "Poll"
        $(widgetFile "consensus-poll")

-- | Negative votes are more important, as defined by these equations.
weight :: Int -> Int
weight 1 = 1
weight (-1) = -3
weight _ = 0

-- | Vote in a poll. Accepts body in the form {"choice_id": 1, "value": -1}.
postConsensusR :: PollId -> Handler Value
postConsensusR pollId = do
    (Entity userId _) <- requireAuth
    _ <- getActionable pollId
    request <- requireJsonBody
    let parser = withObject "request" $ \o -> (,) <$> o .: "choice_id" <*> o .: "value"
    let maybeArgs = parseMaybe parser request :: Maybe (ChoiceId, Int)
    (choiceId, value) <- case maybeArgs of
        Just x@(_, -1) -> return x
        Just x@(_, 0) -> return x
        Just x@(_, 1) -> return x
        _ -> invalidArgs []
    maybeChoice <- runDB $ selectFirst [ChoiceId ==. choiceId, ChoicePollId ==. pollId] []
    _ <- maybe notFound return maybeChoice

    let vote = Vote choiceId userId value
    _ <- runDB $ upsert vote [VoteValue =. value]
    return $ object ["success" .= True]

-- | Add a new option. Accepts body in the form {"name": "Option Name"}.
putConsensusR :: PollId -> Handler Value
putConsensusR pollId = do
    _ <- getActionable pollId
    request <- requireJsonBody
    let parser = withObject "request" $ \o -> o .: "name"
    let maybeName = parseMaybe parser request :: Maybe Text
    name <- maybe (invalidArgs ["`name` required"]) return maybeName

    _ <- runDB $ insert $ Choice name pollId
    return $ object ["success" .= True]

-- | Finalise a poll.
patchConsensusR :: PollId -> Handler Value
patchConsensusR pollId = do
    _ <- getActionable pollId
    _ <- runDB $ update pollId [PollState =. Finished]
    return $ object ["success" .= True]
