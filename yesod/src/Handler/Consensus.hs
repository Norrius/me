{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
module Handler.Consensus where

import Import
import qualified Data.List
import Database.Persist.Sql (rawSql, Single, unSingle)
import Database.Persist.Class (toPersistValue)

getConsensusAllR :: Handler Html
getConsensusAllR = do
    polls <- runDB $ selectList [] [Asc PollName]
    defaultLayout $ do
        setTitle "Polls"
        $(widgetFile "consensus-all")

getConsensusR :: PollId -> Handler Html
getConsensusR pollId = do
    maybePoll <- runDB $ selectFirst [PollId ==. pollId] []
    poll <- case maybePoll of
        Just (Entity _ record) -> return record
        _ -> notFound

    choices <- runDB $ selectList [ChoicePollId ==. pollId] [Desc ChoiceId]
    -- user IDs are themselves unique, so the addition of name doesn't change anything:
    users' <- runDB $ rawSql
        "select distinct vote.user_id, user.name \
        \from vote \
        \join choice on vote.choice_id = choice.id \
        \join user on vote.user_id = user.id \
        \where choice.poll_id = ? \
        \order by vote.user_id" [toPersistValue pollId]
    let users = map (unSingle . snd) (users' :: [(UserId, Single Text)])

    votes' <- runDB $ rawSql
        "select vote.value \
        \from vote \
        \join choice on vote.choice_id = choice.id \
        \where choice.poll_id = ? \
        \order by vote.choice_id, vote.user_id " [toPersistValue pollId]
    let values = chunks (length users) $ map unSingle (votes' :: [Single Int])
    let table = zip choices values
    $logDebug (pack . show $ values)

    defaultLayout $ do
        setTitle "Poll"
        $(widgetFile "consensus-poll")

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs = chunk : chunks n rest
    where
        (chunk, rest) = Data.List.splitAt n xs
