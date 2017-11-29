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
  ) where

import ClassyPrelude.Yesod
import App.User.Model (ProgrammerId, UserId)
import Data.Time (Day)

share [mkPersist sqlSettings, mkMigrate "migratePatch"] [persistLowerCase|
Patch
    content Text
    author UserId
    creationDate Day
    approved ProgrammerId Maybe
    approvalDate Day Maybe
|]
