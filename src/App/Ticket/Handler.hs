{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module App.Ticket.Handler where

import App.Bug.Model (EntityField(..))
import App.Module.Model (Module(..), ModuleId)
import App.Ticket.Model (Ticket(..), TicketId, TicketStatus(..), EntityField(..))
import App.User.Model (UserId)
import App.Utils (optionsUsers, optionsProgrammers)
import ClassyPrelude.Yesod hiding (Request, FormMessage(..), (==.), on)
import Cms.Crud (CrudMessages(..), encodeClickableTable)
import Cms.Crud.Simple
import Cms.Crud.Route (CrudRoute(..), handleCrud)
import Cms.Roles.Class (getCan)
import Colonnade (headed)
import qualified Data.Text as T
import Database.Esqueleto
import Foundation
import Message (AppMessage(..))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as H
import Yesod.Auth (requireAuthId)
import Yesod.Form.Bootstrap3

getTicketR :: Handler Html
getTicketR = postTicketR

postTicketR :: Handler Html
postTicketR = do
  uid <- requireAuthId
  ((res, form), enctype) <- runFormPost (ticketInsertForm uid)
  case res of
    FormSuccess t -> runDB (insert t) >> redirect TicketR
    _ -> defaultLayout [whamlet|<form enctype=#{enctype}>^{form}|]

handleTicketCrudR :: CrudRoute ModuleId Ticket -> Handler Html
handleTicketCrudR =
  handleCrud . toCrudHandler $
  (basicChildSimpleCrud getParent TicketCrudR)
  { scCrudMsg = ticketMessages
  , scForm = ticketForm . either (const Nothing) Just
  , scView = return . toHtml . T.pack . show
  , scIndex = \mid -> do
      can <- getCan
      render <- getUrlRenderParams
      xs :: [Entity Ticket] <-
        runDB . select . from $ \(t `InnerJoin` b `InnerJoin` a) -> do
          on (b ^. BugId ==. a ^. AnnouncesBug)
          on (a ^. AnnouncesTicket ==. t ^. TicketId)
          where_ (b ^. BugModule ==. val mid)
          return t
      let entities =
            (id &&& fmap (`render` []) . flip can "GET" . TicketCrudR . EditR . entityKey) <$> xs
      return $
        encodeClickableTable
        (mconcat
          [ headed "Name" $ \(e, mr) ->
              case mr of
                Nothing -> toHtml . ticketName $ entityVal e
                Just r ->
                  H.a (toHtml . ticketName $ entityVal e) H.! H.href (H.toValue r)
          ])
        entities
  }
  where
    getParent :: TicketId -> YesodDB App ModuleId
    getParent k = do
      p :: [Entity Module] <-
        select $
        from $ \(b `InnerJoin` a `InnerJoin` m) -> do
          on (b ^. BugId ==. a ^. AnnouncesBug)
          on (a ^. AnnouncesTicket ==. val k)
          return m
      case p of
        [] -> notFound
        p':_ -> return $ entityKey p'

ticketInsertForm :: UserId -> Form Ticket
ticketInsertForm uid =
  renderBootstrap3 BootstrapBasicForm $
  Ticket <$> areq textField (bfs MsgTitle) Nothing <*>
  fmap unTextarea (areq textareaField (bfs MsgDescription) Nothing) <*>
  pure StatusNew <*>
  pure uid <*>
  pure Nothing <*
  bootstrapSubmit (BootstrapSubmit MsgSave " btn-success " [])

ticketForm :: Maybe Ticket -> Form Ticket
ticketForm m =
  renderBootstrap3 BootstrapBasicForm $
  Ticket <$> areq textField (bfs MsgName) (ticketName <$> m) <*>
  areq textField (bfs MsgDescription) (ticketDescription <$> m) <*>
  areq (selectField optionsEnum) (bfs MsgStatus) (ticketStatus <$> m) <*>
  areq (selectField optionsUsers) (bfs MsgAuthor) (ticketAuthor <$> m) <*>
  aopt (selectField optionsProgrammers) (bfs MsgAssignedTo) (ticketAssignedTo <$> m) <*
  bootstrapSubmit (BootstrapSubmit MsgSave " btn-success " [])

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
