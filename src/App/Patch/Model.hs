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
  , PatchCommentId
  , PatchComment(..)
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
    deploymentDate Day Maybe

PatchComment
    parent PatchId
    line Int
    author UserId
    content Text
|]

patchName :: Patch -> Text
patchName = (<> "...") . T.take 20 . patchContent
