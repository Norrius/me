{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
module Handler.Consensus where

import Import
import qualified Data.List
import qualified Data.HashMap.Strict as Map
import Database.Persist.Sql (rawSql, toSqlKey, fromSqlKey)
import Database.Persist.Class (toPersistValue)
import Data.Aeson.Types (withObject, parseMaybe)

getConsensusAllR :: Handler Html
getConsensusAllR = do
    polls <- runDB $ selectList [] [Asc PollName]
    defaultLayout $ do
        setTitle "Polls"
        $(widgetFile "consensus-all")

-- | Produces a poll view.
getConsensusR :: PollId -> Handler Html
getConsensusR pollId = do
    maybePoll <- runDB $ selectFirst [PollId ==. pollId] []
    poll <- case maybePoll of
        Just (Entity _ record) -> return record
        _ -> notFound
    maybeUserId <- maybeAuthId
    let userId = case maybeUserId of
            Just x -> x
            _ -> (toSqlKey (-1))  -- dummy key that should not exist

    choices <- runDB $ selectList [ChoicePollId ==. pollId] [Asc ChoiceId]
    allVotes' <- runDB $ rawSql
        "select ?? \
        \from vote join choice \
          \on vote.choice_id = choice.id \
        \where choice.poll_id = ?" [toPersistValue pollId]
    let allVotes = map entityVal allVotes' :: [Vote]
        (myVotes, otherVotes) = partition ((userId ==) . voteUserId) allVotes
        userIds = Data.List.nub $ map voteUserId otherVotes -- O(n^2)

        -- These maps are queried from the template to fill in cell values:
        myVotesMap = Map.fromList $
            map (\v -> (voteChoiceId v, voteValue v)) myVotes
        otherVotesMap = Map.fromList $
            map (\v -> ((voteChoiceId v, voteUserId v), voteValue v)) otherVotes
        totalsMap = Map.fromListWith (+) $
            map (\v -> (voteChoiceId v, weight . voteValue $ v)) allVotes

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

-- | Vote in a poll.
postConsensusR :: PollId -> Handler Value
postConsensusR pollId = do
    (Entity userId _) <- requireAuth
    request <- requireJsonBody
    let parser = withObject "request" $ \o -> (,) <$> o .: "choice_id" <*> o .: "value"
    let maybeArgs = parseMaybe parser request :: Maybe (ChoiceId, Int)
    (choiceId, value) <- case maybeArgs of
        Just x@(_, -1) -> return x
        Just x@(_, 0) -> return x
        Just x@(_, 1) -> return x
        _ -> invalidArgs []
    -- TODO: verify poll_id = choice.poll_id

    let vote = Vote choiceId userId value
    _ <- runDB $ upsert vote [VoteValue =. value]
    return $ object ["success" .= True]

-- | Add a new option.
putConsensusR :: PollId -> Handler Value
putConsensusR pollId = do
    (Entity userId _) <- requireAuth
    request <- requireJsonBody
    let parser = withObject "request" $ \o -> o .: "name"
    let maybeName = parseMaybe parser request :: Maybe Text
    name <- case maybeName of
        Just x -> return x
        _ -> invalidArgs[]

    _ <- runDB $ insert $ Choice name pollId
    return $ object ["success" .= True]
