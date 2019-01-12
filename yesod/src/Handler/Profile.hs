{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Handler.Profile where

import Import
import Data.Aeson.Types (withObject, parseMaybe)

getProfileR :: Handler Html
getProfileR = do
    (_, user) <- requireAuthPair
    defaultLayout $ do
        setTitle . toHtml $ ("User Profile" :: Text)
        $(widgetFile "profile")

-- | Change user settings.
postProfileR :: Handler Value
postProfileR = do
    (Entity userId _) <- requireAuth
    request <- requireJsonBody
    let parser = withObject "request" $ \o -> o .: "name"
    let maybeName = parseMaybe parser request :: Maybe Text
    name <- maybe (invalidArgs ["`name` required"]) return maybeName

    _ <- runDB $ update userId [UserName =. name]
    return $ object ["success" .= True]
