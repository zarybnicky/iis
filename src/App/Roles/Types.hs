{-# LANGUAGE TemplateHaskell       #-}

module App.Roles.Types
  ( RoleName(..)
  ) where

import ClassyPrelude.Yesod

data RoleName
  = User
  | Programmer
  | Admin
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Typeable)

derivePersistField "RoleName"
