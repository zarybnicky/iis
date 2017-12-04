{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module App.Language.Model
  ( LanguageId
  , Language(..)
  , Unique(..)
  , EntityField(..)
  , migrateLanguage
  ) where

import ClassyPrelude.Yesod

share [mkPersist sqlSettings, mkMigrate "migrateLanguage"] [persistLowerCase|
Language
    name Text
    UniqueLanguage name
|]
