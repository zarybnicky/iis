{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module App.Bug.Model
  ( BugId
  , Bug(..)
  , EntityField(..)
  , migrateBug
  ) where

import ClassyPrelude.Yesod
import App.Module.Model (ModuleId)
import App.Patch.Model (PatchId)
import App.User.Model (UserId)

share [mkPersist sqlSettings, mkMigrate "migrateBug"] [persistLowerCase|
Bug
    name Text
    description Text
    severity Int
    vulnerability Int Maybe
    module ModuleId
    isRepairedBy PatchId Maybe
    author UserId
|]
