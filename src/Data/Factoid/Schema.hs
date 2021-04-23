{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Data.Factoid.Schema where


import           Data.Text (Text)

import qualified Database.Persist


import           Database.Persist.TH         ( AtLeastOneUniqueKey(..)
                                             , OnlyOneUniqueKey(..)
                                             , mkDeleteCascade
                                             , mkMigrate
                                             , mkPersist
                                             , persistLowerCase
                                             , share
                                             , sqlSettings
                                             )

import Data.Time (UTCTime)

share [ mkPersist sqlSettings
      , mkDeleteCascade sqlSettings
      , mkMigrate "migrateAll" ] [persistLowerCase|
  Factoid
    name      Text
    value     Text
    locked    Bool
    UniqueName name
    deriving Eq Show Ord

  FactoidHistory
    ref       FactoidId
    value     Text
    by        Text
    when      UTCTime
    deriving Eq Show Ord
|]
