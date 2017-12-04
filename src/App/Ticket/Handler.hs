{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module App.Ticket.Handler where

import App.Bug.Model
import App.Ticket.Model
import App.User.Model (UserId)
import App.Utils (optionsUsers, optionsProgrammers)
import ClassyPrelude.Yesod hiding (Request, FormMessage(..), (==.), on)
import Cms.Crud
import Cms.Crud.Route (CrudRoute(..), handleCrud)
import Colonnade (headed)
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

handleTicketCrudR :: CrudRoute () Ticket -> Handler Html
handleTicketCrudR =
  handleCrud . flip simplerCrudToHandler TicketCrudR $
  SimplerCrud
  { crudSimplerMsg = ticketMessages
  , crudSimplerDb = defaultCrudDb
  , crudSimplerForm = ticketForm
  , crudSimplerTable =
      encodeClickableTable
      (mconcat
        [ headed "Name" $ \(e, mr) ->
            case mr of
              Nothing -> toHtml . ticketName $ entityVal e
              Just r ->
                H.a (toHtml . ticketName $ entityVal e) H.! H.href (H.toValue r)
        ])
  }

announcesName :: Announces -> Text
announcesName a =
  toPathPiece (announcesBug a) <> " " <> toPathPiece (announcesTicket a)

handleAnnouncesCrudR :: CrudRoute () Announces -> Handler Html
handleAnnouncesCrudR =
  handleCrud . flip simplerCrudToHandler AnnouncesCrudR $
  SimplerCrud
  { crudSimplerMsg = announcesMessages
  , crudSimplerDb = defaultCrudDb
  , crudSimplerForm = announcesForm
  , crudSimplerTable =
      encodeClickableTable
        (mconcat
           [ headed "Name" $ \(e, mr) ->
               case mr of
                 Nothing -> toHtml . announcesName $ entityVal e
                 Just r -> H.a (toHtml . announcesName $ entityVal e) H.! H.href (H.toValue r)
           ])
  }

ticketInsertForm :: UserId -> Form Ticket
ticketInsertForm uid =
  renderBootstrap3 BootstrapBasicForm $
  Ticket <$> areq textField (bfs MsgTitle) Nothing <*>
  fmap unTextarea (areq textareaField (bfs MsgDescription) Nothing) <*>
  pure StatusNew <*>
  pure uid <*>
  pure Nothing <*
  bootstrapSubmit (BootstrapSubmit MsgSave " btn-success " [])

ticketForm :: Maybe Ticket -> UTCTime -> Form Ticket
ticketForm m _ =
  renderBootstrap3 BootstrapBasicForm $
  Ticket <$> areq textField (bfs MsgName) (ticketName <$> m) <*>
  areq textField (bfs MsgDescription) (ticketDescription <$> m) <*>
  areq (selectField optionsEnum) (bfs MsgStatus) (ticketStatus <$> m) <*>
  areq (selectField optionsUsers) (bfs MsgAuthor) (ticketAuthor <$> m) <*>
  aopt (selectField optionsProgrammers) (bfs MsgAssignedTo) (ticketAssignedTo <$> m) <*
  bootstrapSubmit (BootstrapSubmit MsgSave " btn-success " [])

announcesForm :: Maybe Announces -> UTCTime -> Form Announces
announcesForm m _ =
  renderBootstrap3 BootstrapBasicForm $
  Announces <$>
  areq (selectField optionsBugs) (bfs MsgBug) (announcesBug <$> m) <*>
  areq (selectField optionsTickets) (bfs MsgTicket) (announcesTicket <$> m) <*
  bootstrapSubmit (BootstrapSubmit MsgSave " btn-success " [])
  where
    optionsBugs = optionsPersistKey [] [] bugName
    optionsTickets = optionsPersistKey [] [] ticketName

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

announcesMessages :: CrudMessages App Announces
announcesMessages = CrudMessages
  { crudMsgBack = SomeMessage MsgBack
  , crudMsgDelete = SomeMessage MsgDelete
  , crudMsgIndex = SomeMessage MsgAnnouncesAdminIndex
  , crudMsgNew = SomeMessage MsgAnnouncesAdminNew
  , crudMsgEdit = SomeMessage MsgAnnouncesAdminEdit
  , crudMsgNoEntities = SomeMessage MsgNoAnnouncesFound
  , crudMsgCreated = SomeMessage . MsgLogAnnouncesCreated . announcesName
  , crudMsgUpdated = SomeMessage . MsgLogAnnouncesUpdated . announcesName
  , crudMsgDeleted = SomeMessage . MsgLogAnnouncesDeleted . announcesName
  }
