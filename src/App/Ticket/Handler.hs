{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module App.Ticket.Handler where

import App.User.Model (User(..), Programmer(..), EntityField(..))
import App.Bug.Model (EntityField(..))
import App.Ticket.Model (Ticket(..), TicketId, EntityField(..))
import App.Module.Model (Module(..), ModuleId)
import ClassyPrelude.Yesod hiding (Request, FormMessage(..), (==.))
import Cms.Crud (CrudMessages(..), encodeClickableTable)
import Cms.Crud.Simple
import Cms.Crud.Route (CrudRoute, handleCrud)
import Colonnade (headed)
import qualified Data.Text as T
import qualified Database.Esqueleto as E
import Foundation
import Message (AppMessage(..))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as H
import Yesod.Form.Bootstrap3

handleTicketR :: CrudRoute ModuleId Ticket -> Handler Html
handleTicketR =
  handleCrud . toCrudHandler $
  (basicChildSimpleCrud getParent TicketR)
  { scCrudMsg = ticketMessages
  , scForm = ticketForm . either (const Nothing) Just
  , scView = return . toHtml . T.pack . show
  }
  --     encodeClickableTable $
  --     mconcat
  --       [ headed "Name" $ \(e, mr) ->
  --           case mr of
  --             Nothing -> toHtml . ticketName $ entityVal e
  --             Just r ->
  --               H.a (toHtml . ticketName $ entityVal e) H.! H.href (H.toValue r)
  --       ]
  where
    getParent :: TicketId -> YesodDB App ModuleId
    getParent k = do
      p :: [Entity Module] <-
        E.select $
        E.from $ \(b `E.InnerJoin` a `E.InnerJoin` m) -> do
          E.on (b E.^. BugId E.==. a E.^. AnnouncesBug)
          E.on (a E.^. AnnouncesTicket E.==. E.val k)
          return m
      case p of
        [] -> notFound
        p':_ -> return $ entityKey p'

ticketForm :: Maybe Ticket -> Form Ticket
ticketForm m =
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
