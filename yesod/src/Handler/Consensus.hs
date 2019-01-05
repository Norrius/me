{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
module Handler.Consensus where

import Import
import qualified Data.List
import Database.Persist.Sql (rawSql, Single, unSingle, toSqlKey, fromSqlKey)
import Database.Persist.Class (toPersistValue)
import Data.Aeson.Types (withObject, parseMaybe)

getConsensusAllR :: Handler Html
getConsensusAllR = do
    polls <- runDB $ selectList [] [Asc PollName]
    defaultLayout $ do
        setTitle "Polls"
        $(widgetFile "consensus-all")

-- | Produces a poll view. The crazy selects could be simplified (making them more efficient),
-- but at the cost of dealing with more logic Haskell-side, which I'd rather not.
getConsensusR :: PollId -> Handler Html
getConsensusR pollId = do
    maybeUserId <- maybeAuthId
    let userId = case maybeUserId of
            Just x -> x
            _ -> (toSqlKey 0)

    maybePoll <- runDB $ selectFirst [PollId ==. pollId] []
    poll <- case maybePoll of
        Just (Entity _ record) -> return record
        _ -> notFound

    choices <- runDB $ selectList [ChoicePollId ==. pollId] [Asc ChoiceId]
    -- user IDs are themselves unique, so the addition of name doesn't change anything:
    users' <- runDB $ rawSql
        "select distinct vote.user_id, \"user\".name \
        \from vote \
        \join choice on vote.choice_id = choice.id \
        \join \"user\" on vote.user_id = \"user\".id \
        \where choice.poll_id = ? and \"user\".id != ? \
        \order by vote.user_id" [toPersistValue pollId, toPersistValue userId]
    let users = map (unSingle . snd) (users' :: [(UserId, Single Text)])

    myVotes' <- runDB $ rawSql
        "select coalesce(vote.value, 0) \
        \from choice \
          \left outer join vote on vote.choice_id = choice.id \
        \where choice.poll_id = ? and (vote.user_id = ? or vote.user_id isnull) \
        \order by choice.id" [toPersistValue pollId, toPersistValue userId]
    let myVotes = map unSingle (myVotes' :: [Single Int])

    values' <- runDB $ rawSql
        "select coalesce(vote.value, 0) \
         \from choice \
           \cross join ( \
             \select distinct \"user\".* \
             \from \"user\" \
               \join vote on \"user\".id = vote.user_id \
               \join choice on vote.choice_id = choice.id \
             \where choice.poll_id = ? and \"user\".id != ? \
           \) as \"user\" \
           \left outer join vote on \
             \vote.choice_id = choice.id and \
             \vote.user_id = \"user\".id \
         \where choice.poll_id = ? \
         \order by choice.id asc, \"user\".id " [toPersistValue pollId, toPersistValue userId, toPersistValue pollId]
    let values = map unSingle (values' :: [Single Int])
    let votes = chunks (length users) $ map voteClass values
    let table = zip3 choices myVotes votes
    $logDebug (pack . show $ table)

    defaultLayout $ do
        setTitle "Poll"
        $(widgetFile "consensus-poll")

voteClass :: Int -> Text
voteClass (-1) = "neg"
voteClass 1 = "pos"
voteClass _ = "neut"

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs = chunk : chunks n rest
    where
        (chunk, rest) = Data.List.splitAt n xs

-- | Vote in a poll.
postConsensusR :: PollId -> Handler Value
postConsensusR pollId = do
    (Entity userId _) <- requireAuth
    request <- requireJsonBody
    let parser = withObject "request" $ \o -> (,) <$> o .: "choice_id" <*> o .: "value"
    let maybeArgs = parseMaybe parser request :: Maybe (ChoiceId, Int)
    (choiceId, value) <- case maybeArgs of
        Just x -> return x
        _ -> invalidArgs []
    -- TODO: verify poll_id = choice.poll_id

    let vote = Vote choiceId userId value
    _ <- runDB $ upsert vote [VoteValue =. value]
    return $ object ["success" .= True]
