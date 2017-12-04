{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module App.Module.Model
  ( ModuleId
  , Module(..)
  , EntityField(..)
  , migrateModule
  ) where

import ClassyPrelude.Yesod
import App.User.Model (ProgrammerId)
import App.Language.Model (LanguageId)

share [mkPersist sqlSettings, mkMigrate "migrateModule"] [persistLowerCase|
Module
    name Text
    description Text
    repository Text
    supervisor ProgrammerId
    languages [LanguageId]
|]
