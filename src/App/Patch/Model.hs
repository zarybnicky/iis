{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module App.Patch.Model
  ( PatchId
  , Patch(..)
  , EntityField(..)
  , migratePatch
  , patchName
  ) where

import App.User.Model (ProgrammerId, UserId)
import ClassyPrelude.Yesod
import qualified Data.Text as T
import Data.Time (Day)

share [mkPersist sqlSettings, mkMigrate "migratePatch"] [persistLowerCase|
Patch
    content Text
    author UserId
    creationDate Day
    approved ProgrammerId Maybe
    approvalDate Day Maybe
|]

patchName :: Patch -> Text
patchName = (<> "...") . T.take 20 . patchContent
