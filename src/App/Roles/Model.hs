{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module App.Roles.Model
  ( UserRoleId
  , UserRole(..)
  , EntityField(..)
  , Unique(..)
  , migrateRoles
  ) where

import App.Roles.Types (RoleName)
import App.User.Model (UserId)
import ClassyPrelude.Yesod

share [mkPersist sqlSettings, mkMigrate "migrateRoles"] [persistLowerCase|
UserRole
    userId UserId
    roleName RoleName
    UniqueUserRole userId roleName
    deriving Show
|]
