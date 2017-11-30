{-# LANGUAGE TemplateHaskell       #-}

module App.Roles.Types
  ( RoleName(..)
  ) where

import ClassyPrelude.Yesod

data RoleName
  = RoleProgrammer
  | RoleAdmin
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Typeable)

derivePersistField "RoleName"
