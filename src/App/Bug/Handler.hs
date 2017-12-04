{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module App.Bug.Handler where

import App.Bug.Model
import App.Module.Model (Module(..), EntityField(..))
import App.Patch.Model
import App.User.Model
import App.Utils (optionsUsers)
import ClassyPrelude.Yesod hiding (Request, FormMessage(..))
import Cms.Crud
import Cms.Crud.Route (CrudRoute(..), handleCrud)
import Colonnade (headed)
import qualified Data.Text as T
import Foundation
import Message (AppMessage(..))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as H
import Yesod.Auth (requireAuthId)
import Yesod.Form.Bootstrap3

getMyBugsR :: Handler Html
getMyBugsR = do
  uid <- requireAuthId
  ps <- runDB $ selectList [BugAuthor ==. uid] [Desc BugId]
  defaultLayout $ [whamlet|
    <h1>My bugs
    <a href=@{AddBugR}><b>+ New bug
    <table .table>
      <thead>
        <th>Name
        <th>
        <th>
      <tbody>
        $forall Entity k p <- ps
          <tr>
            <td>
              <a href=@{ViewBugR k}>
                #{bugName p}
            <td>
              <a href=@{EditBugR k}>Edit
            <td>
              <a href=@{DeleteBugR k}>Delete
  |]

getAddBugR :: Handler Html
getAddBugR = postAddBugR

postAddBugR :: Handler Html
postAddBugR = do
  uid <- requireAuthId
  ((res, form), enctype) <- runFormPost (bugUserForm Nothing uid)
  case res of
    FormSuccess t -> do
      _ <- runDB (insert t)
      setMessage "Bug added"
      redirect MyBugsR
    _ -> defaultLayout [whamlet|<form method="post" enctype=#{enctype}>^{form}|]

getEditBugR :: BugId -> Handler Html
getEditBugR = postEditBugR

postEditBugR :: BugId -> Handler Html
postEditBugR pid = do
  uid <- requireAuthId
  p <- runDB $ get404 pid
  when (bugAuthor p /= uid) (permissionDenied "You can edit only your bugs")
  ((res, form), enctype) <- runFormPost (bugUserForm (Just p) uid)
  case res of
    FormSuccess t -> do
      _ <- runDB (replace pid t)
      setMessage "Bug added"
      redirect MyBugsR
    _ -> defaultLayout [whamlet|<form method="post" enctype=#{enctype}>^{form}|]

getDeleteBugR :: BugId -> Handler Html
getDeleteBugR pid = do
  uid <- requireAuthId
  p <- runDB $ get404 pid
  when (bugAuthor p /= uid) (permissionDenied "You can edit only your bugs")
  runDB $ delete pid
  redirect MyBugsR

getViewBugR :: BugId -> Handler Html
getViewBugR pid = do
  p <- runDB $ get404 pid
  defaultLayout $ [whamlet|
    <h1>#{bugName p}
    <p>
      #{bugDescription p}
  |]


handleBugCrudR :: CrudRoute () Bug -> Handler Html
handleBugCrudR =
  handleCrud . flip simplerCrudToHandler BugCrudR $
  SimplerCrud
  { crudSimplerMsg = bugMessages
  , crudSimplerDb = defaultCrudDb
  , crudSimplerForm = bugForm
  , crudSimplerTable =
      encodeClickableTable
        (mconcat
           [ headed "Name" $ \(e, mr) ->
               case mr of
                 Nothing -> toHtml . bugName $ entityVal e
                 Just r -> H.a (toHtml . bugName $ entityVal e) H.! H.href (H.toValue r)
           ])
  }

bugUserForm :: Maybe Bug -> UserId -> Form Bug
bugUserForm m uid =
  renderBootstrap3 BootstrapBasicForm $
  Bug <$> areq textField (bfs ("Name(*)" :: Text)) (bugName <$> m) <*>
  areq textField (bfs ("Description(*)" :: Text)) (bugDescription <$> m) <*>
  areq intField (bfs ("Severity(*)" :: Text)) (bugSeverity <$> m) <*>
  aopt intField (bfs MsgVulnerability) (bugVulnerability <$> m) <*>
  areq (selectField optionsModules) (bfs ("Module(*)" :: Text)) (bugModule <$> m) <*>
  pure (fromMaybe Nothing (bugIsRepairedBy <$> m)) <*>
  pure (fromMaybe uid (bugAuthor <$> m)) <*
  bootstrapSubmit (BootstrapSubmit MsgSave " btn-success " [])
  where
    optionsModules = optionsPersistKey [] [Asc ModuleName] moduleName

bugForm :: Maybe Bug -> UTCTime -> Form Bug
bugForm m _ =
  renderBootstrap3 BootstrapBasicForm $
  Bug <$> areq textField (bfs ("Name(*)" :: Text)) (bugName <$> m) <*>
  areq textField (bfs ("Description(*)" :: Text)) (bugDescription <$> m) <*>
  areq intField (bfs ("Severity(*)" :: Text)) (bugSeverity <$> m) <*>
  aopt intField (bfs MsgVulnerability) (bugVulnerability <$> m) <*>
  areq (selectField optionsModules) (bfs ("Module(*)" :: Text)) (bugModule <$> m) <*>
  aopt (selectField optionsPatches) (bfs ("Is repaired by patch" :: Text)) (bugIsRepairedBy <$> m) <*>
  areq (selectField optionsUsers) (bfs ("Author(*)" :: Text)) (bugAuthor <$> m) <*
  bootstrapSubmit (BootstrapSubmit MsgSave " btn-success " [])
  where
    optionsModules = optionsPersistKey [] [Asc ModuleName] moduleName
    optionsPatches = optionsPersistKey [] [] (T.take 20 . patchContent)

bugMessages :: CrudMessages App Bug
bugMessages = CrudMessages
  { crudMsgBack = SomeMessage MsgBack
  , crudMsgDelete = SomeMessage MsgDelete
  , crudMsgIndex = SomeMessage MsgBugAdminIndex
  , crudMsgNew = SomeMessage MsgBugAdminNew
  , crudMsgEdit = SomeMessage MsgBugAdminEdit
  , crudMsgNoEntities = SomeMessage MsgNoBugFound
  , crudMsgCreated = SomeMessage . MsgLogBugCreated . bugName
  , crudMsgUpdated = SomeMessage . MsgLogBugUpdated . bugName
  , crudMsgDeleted = SomeMessage . MsgLogBugDeleted . bugName
  }
