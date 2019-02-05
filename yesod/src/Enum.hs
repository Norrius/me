{-# LANGUAGE TemplateHaskell #-}
module Enum where

import Database.Persist.TH
import Prelude

data PollState = Open | Ongoing | Finished
    deriving (Show, Read, Eq)
derivePersistField "PollState"
