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

module App.Ticket.Model
  ( TicketId
  , Ticket(..)
  , AnnouncesId
  , Announces(..)
  , EntityField(..)
  , migrateTicket
  ) where

import ClassyPrelude.Yesod
import App.Bug.Model (BugId)
import App.User.Model (ProgrammerId, UserId)
import App.Ticket.Types (TicketStatus)

share [mkPersist sqlSettings, mkMigrate "migrateTicket"] [persistLowerCase|
Ticket
    status TicketStatus
    title Text
    text Text
    author UserId
    assignedTo ProgrammerId Maybe

Announces
    bug BugId
    ticket TicketId
    Primary bug ticket
|]
