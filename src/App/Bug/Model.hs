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
import App.Patch.Model (PatchId)
import App.Vulnerability.Model (VulnerabilityId)
import App.Module.Model (ModuleId)

share [mkPersist sqlSettings, mkMigrate "migrateBug"] [persistLowerCase|
Bug
    name Text
    description Text
    severity Int
    module ModuleId
    isRepairedBy PatchId Maybe
    causes VulnerabilityId Maybe
|]
