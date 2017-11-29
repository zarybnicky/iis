{-# LANGUAGE TemplateHaskell       #-}

module App.Ticket.Types
  ( TicketStatus(..)
  ) where

import ClassyPrelude.Yesod

data TicketStatus
  = New
  | InProgress
  | Complete
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Typeable)

derivePersistField "TicketStatus"

