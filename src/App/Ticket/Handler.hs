{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module App.Ticket.Handler where

import App.Bug.Model
import App.Ticket.Model
import App.User.Model
import App.Utils (optionsUsers, optionsProgrammers)
import ClassyPrelude.Yesod hiding (Request, FormMessage(..), (==.), on)
import Cms.Crud
import Cms.Crud.Route (CrudRoute(..), handleCrud)
import Colonnade (headed)
import qualified Database.Esqueleto as E
import qualified Database.Persist as P
import Foundation
import Message (AppMessage(..))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as H
import Yesod.Auth (requireAuthId)
import Yesod.Form.Bootstrap3

getMyTicketsR :: Handler Html
getMyTicketsR = do
  uid <- requireAuthId
  ps :: [(Entity Ticket, Maybe (Entity User))] <-
    runDB $ E.select $ E.from $ \(t `E.LeftOuterJoin` p `E.InnerJoin` u) -> do
    E.on (t E.^. TicketAssignedTo E.==. p E.?. ProgrammerId)
    E.on (p E.?. ProgrammerUser E.==. u E.?. UserId)
    E.orderBy [E.desc $ t E.^. TicketId]
    E.where_ $ t E.^. TicketAuthor E.==. E.val uid
    return (t, u)
  defaultLayout $ [whamlet|
    <h1>My patches
    <a href=@{AddTicketR}><b>+ New ticket
    <table .table>
      <thead>
        <th>Name
        <th>Status
        <th>Assigned to
        <th>
        <th>
      <tbody>
        $forall (Entity k p, m) <- ps
          <tr>
            <td>
              <a href=@{ViewTicketR k}>
                #{ticketName p}
            <td>#{tshow $ ticketStatus p}
            $maybe Entity _ u <- m
              <td>#{userFullName u}
            $nothing
              <td>-
            <td>
              <a href=@{EditTicketR k}>Edit
            <td>
              <a href=@{DeleteTicketR k}>Delete
  |]

getAddTicketR :: Handler Html
getAddTicketR = postAddTicketR

postAddTicketR :: Handler Html
postAddTicketR = do
  uid <- requireAuthId
  ((res, form), enctype) <- runFormPost (ticketUserForm Nothing uid)
  case res of
    FormSuccess (t, bugs) -> do
      _ <- runDB $ do
        tid <- insert t
        insertMany $ flip Announces tid <$> bugs
      setMessage "Ticket added"
      redirect MyTicketsR
    _ -> defaultLayout [whamlet|<form method="post" enctype=#{enctype}>^{form}|]

getEditTicketR :: TicketId -> Handler Html
getEditTicketR = postEditTicketR

postEditTicketR :: TicketId -> Handler Html
postEditTicketR pid = do
  uid <- requireAuthId
  p <- runDB $ get404 pid
  when (ticketAuthor p /= uid) (permissionDenied "You can edit only your tickets")
  bugs <- fmap (fmap entityKey) . runDB $ E.select $ E.from $ \(a `E.InnerJoin` b) -> do
    E.on (a E.^. AnnouncesBug E.==. b E.^. BugId)
    E.where_ $ a E.^. AnnouncesTicket E.==. E.val pid
    return b
  ((res, form), enctype) <- runFormPost (ticketUserForm (Just (p, bugs)) uid)
  case res of
    FormSuccess (t, bugs') -> do
      _ <- runDB $ do
        replace pid t
        deleteWhere [AnnouncesTicket P.==. pid]
        insertMany $ flip Announces pid <$> bugs'
      setMessage "Ticket added"
      redirect MyTicketsR
    _ -> defaultLayout [whamlet|<form method="post" enctype=#{enctype}>^{form}|]

getDeleteTicketR :: TicketId -> Handler Html
getDeleteTicketR pid = do
  uid <- requireAuthId
  p <- runDB $ get404 pid
  when (ticketAuthor p /= uid) (permissionDenied "You can edit only your tickets")
  runDB $ delete pid
  redirect MyTicketsR

getViewTicketR :: TicketId -> Handler Html
getViewTicketR pid = do
  p <- runDB $ get404 pid
  defaultLayout $ [whamlet|
    <h1>#{ticketName p}
    <p>
      #{ticketDescription p}
  |]

handleTicketCrudR :: CrudRoute () Ticket -> Handler Html
handleTicketCrudR =
  handleCrud . flip simplerCrudToHandler TicketCrudR $
  SimplerCrud
  { crudSimplerMsg = ticketMessages
  , crudSimplerDb = defaultCrudDb
  , crudSimplerForm = ticketForm
  , crudSimplerTable = encodeClickableTable $ mconcat
    [ headed "Name" $ \(e, mr) ->
        case mr of
          Nothing -> toHtml . ticketName $ entityVal e
          Just r ->
            H.a (toHtml . ticketName $ entityVal e) H.! H.href (H.toValue r)
    , headed "Status" $ \(e, _) -> toHtml . show . ticketStatus $ entityVal e
    ]
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
      encodeClickableTable $ mconcat
      [ headed "Name" $ \(e, mr) ->
          case mr of
            Nothing -> toHtml . announcesName $ entityVal e
            Just r -> H.a (toHtml . announcesName $ entityVal e) H.! H.href (H.toValue r)
      ]
  }

ticketUserForm :: Maybe (Ticket, [BugId]) -> UserId -> Form (Ticket, [BugId])
ticketUserForm m uid =
  renderBootstrap3 BootstrapBasicForm $
  (,) <$>
  (Ticket <$>
  areq textField (bfs ("Title(*)" :: Text)) (ticketName . fst <$> m) <*>
  fmap unTextarea (areq textareaField (bfs ("Description(*)" :: Text)) (Textarea . ticketDescription . fst <$> m)) <*>
  pure (fromMaybe StatusNew (ticketStatus . fst <$> m)) <*>
  pure (fromMaybe uid (ticketAuthor . fst <$> m)) <*>
  pure (fromMaybe Nothing (ticketAssignedTo . fst <$> m))) <*>
  areq (multiSelectField optionsBugs) (bfs ("Related bugs" :: Text)) (snd <$> m) <*
  bootstrapSubmit (BootstrapSubmit MsgSave " btn-success " [])
  where
    optionsBugs = optionsPersistKey [] [Asc BugName] bugName

ticketForm :: Maybe Ticket -> UTCTime -> Form Ticket
ticketForm m _ =
  renderBootstrap3 BootstrapBasicForm $
  Ticket <$> areq textField (bfs ("Name(*)" :: Text)) (ticketName <$> m) <*>
  areq textField (bfs ("Description(*)" :: Text)) (ticketDescription <$> m) <*>
  areq (selectField optionsEnum) (bfs ("Status(*)" :: Text)) (ticketStatus <$> m) <*>
  areq (selectField optionsUsers) (bfs ("Author(*)" :: Text)) (ticketAuthor <$> m) <*>
  aopt (selectField optionsProgrammers) (bfs MsgAssignedTo) (ticketAssignedTo <$> m) <*
  bootstrapSubmit (BootstrapSubmit MsgSave " btn-success " [])

announcesForm :: Maybe Announces -> UTCTime -> Form Announces
announcesForm m _ =
  renderBootstrap3 BootstrapBasicForm $
  Announces <$>
  areq (selectField optionsBugs) (bfs ("Bug(*)" :: Text)) (announcesBug <$> m) <*>
  areq (selectField optionsTickets) (bfs ("Ticket(*)" :: Text)) (announcesTicket <$> m) <*
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
