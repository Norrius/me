{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE UndecidableInstances       #-}
module Model where

import ClassyPrelude.Yesod
import Database.Persist.Quasi
import Database.Persist.Sql (toSqlKey, fromSqlKey)

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

-- | Key for the demo calendar, available to unauthenticated users.
demoCalendar :: Key Calendar
demoCalendar = toSqlKey 0

-- | Enables putting SQL keys in maps. Requires two pragmas and produces
-- a warning, so probably is a hack, although I cannot articulate why.
instance (ToBackendKey SqlBackend a) => Hashable (Key a) where
  hashWithSalt salt k = hashWithSalt salt (fromSqlKey k)
