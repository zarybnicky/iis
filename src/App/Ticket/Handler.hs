{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module App.Ticket.Handler where

import App.User.Model (User(..), Programmer(..), EntityField(..))
import App.Ticket.Model (Ticket(..))
import App.Module.Model (ModuleId)
import ClassyPrelude.Yesod hiding (Request, FormMessage(..))
import Cms.Crud (SimplerCrud(..), CrudMessages(..), encodeClickableTable, simplerCrudToHandler)
import Cms.Crud.Route (CrudRoute, handleCrud)
import Colonnade (headed)
import qualified Data.Text as T
import Foundation
import Message (AppMessage(..))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as H
import Yesod.Form.Bootstrap3

handleTicketR :: CrudRoute ModuleId Ticket -> Handler Html
handleTicketR = error "Requires 'SimpleCrud'"
  -- handleCrud . flip simplerCrudToHandler TicketR $
  -- SimplerCrud
  -- { crudSimplerMsg = ticketMessages
  -- , crudSimplerDb = defaultCrudDb
  -- , crudSimplerForm = ticketForm
  -- , crudSimplerTable =
  --     encodeClickableTable $
  --     mconcat
  --       [ headed "Name" $ \(e, mr) ->
  --           case mr of
  --             Nothing -> toHtml . ticketName $ entityVal e
  --             Just r ->
  --               H.a (toHtml . ticketName $ entityVal e) H.! H.href (H.toValue r)
  --       ]
  -- }

ticketForm :: Maybe Ticket -> UTCTime -> Form Ticket
ticketForm m _ =
  renderBootstrap3 BootstrapBasicForm $
  Ticket <$> areq textField (bfs MsgName) (ticketName <$> m) <*>
  areq textField (bfs MsgDescription) (ticketDescription <$> m) <*>
  areq (selectField optionsEnum) (bfs MsgStatus) (ticketStatus <$> m) <*>
  areq (selectField users) (bfs MsgAuthor) (ticketAuthor <$> m) <*>
  aopt (selectField programmers) (bfs MsgAssignedTo) (ticketAssignedTo <$> m) <*
  bootstrapSubmit (BootstrapSubmit MsgSave " btn-success " [])
  where
    users =
      optionsPersistKey
        []
        [Asc UserId]
        (mconcat $ ($) <$> [userFirstName, const " ", userLastName])
    programmers =
      optionsPersistKey
        []
        [Asc ProgrammerContractNum]
        (T.pack . show . programmerContractNum)


ticketMessages :: CrudMessages App Ticket
ticketMessages = CrudMessages
  { crudMsgBack = SomeMessage MsgBack
  , crudMsgDelete = SomeMessage MsgDelete
  , crudMsgIndex = SomeMessage MsgTicketAdminIndex
  , crudMsgNew = SomeMessage MsgTicketAdminNew
  , crudMsgEdit = SomeMessage MsgTicketAdminEdit
  , crudMsgNoEntities = SomeMessage MsgNoTicketFound
  , crudMsgCreated = SomeMessage . MsgLogTicketCreated . ticketName
  , crudMsgUpdated = SomeMessage . MsgLogTicketUpdated . ticketName
  , crudMsgDeleted = SomeMessage . MsgLogTicketDeleted . ticketName
  }
