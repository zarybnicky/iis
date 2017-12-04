{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module App.Patch.Handler where

import App.User.Model
import App.Patch.Model
import App.Utils (optionsUsers, optionsProgrammers)
import ClassyPrelude.Yesod hiding (Request, FormMessage(..))
import Cms.Crud
import Cms.Crud.Route
import Cms.Roles.Class (getCan)
import Colonnade (headed)
import qualified Data.Text as T
import qualified Database.Esqueleto as E
import Foundation
import Message (AppMessage(..))
import Settings (widgetFile)
import Settings.StaticFiles
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as H
import Yesod.Auth (requireAuthId)
import Yesod.Form.Bootstrap3

getMyPatchesR :: Handler Html
getMyPatchesR = do
  uid <- requireAuthId
  ps <- runDB $ selectList [PatchAuthor ==. uid] [Desc PatchCreationDate]
  defaultLayout $ [whamlet|
    <h1>My patches
    <a href=@{AddPatchR}><b>+ New patch
    <table .table>
      <thead>
        <th>Snippet
        <th>Created
        <th>
        <th>
      <tbody>
        $forall Entity k p <- ps
          <tr>
            <td>#{patchName p}
            <td>#{tshow $ patchCreationDate p}
            <td>
              <a href=@{EditPatchR k}>Edit
            <td>
              <a href=@{DeletePatchR k}>Delete
  |]

getAddPatchR :: Handler Html
getAddPatchR = postAddPatchR

postAddPatchR :: Handler Html
postAddPatchR = do
  uid <- requireAuthId
  now <- liftIO $ fmap utctDay getCurrentTime
  ((res, form), enctype) <- runFormPost (patchUserForm Nothing uid now)
  case res of
    FormSuccess t -> do
      _ <- runDB (insert t)
      setMessage "Patch added"
      redirect MyPatchesR
    _ -> defaultLayout [whamlet|<form method="post" enctype=#{enctype}>^{form}|]

getEditPatchR :: PatchId -> Handler Html
getEditPatchR = postEditPatchR

postEditPatchR :: PatchId -> Handler Html
postEditPatchR pid = do
  uid <- requireAuthId
  p <- runDB $ get404 pid
  when (patchAuthor p /= uid) (permissionDenied "You can edit only your patches")
  now <- liftIO $ fmap utctDay getCurrentTime
  ((res, form), enctype) <- runFormPost (patchUserForm (Just p) uid now)
  case res of
    FormSuccess t -> do
      _ <- runDB (replace pid t)
      setMessage "Patch added"
      redirect MyPatchesR
    _ -> defaultLayout [whamlet|<form method="post" enctype=#{enctype}>^{form}|]

getDeletePatchR :: PatchId -> Handler Html
getDeletePatchR pid = do
  uid <- requireAuthId
  p <- runDB $ get404 pid
  when (patchAuthor p /= uid) (permissionDenied "You can edit only your patches")
  runDB $ delete pid
  redirect MyPatchesR

getViewPatchR :: PatchId -> Handler Html
getViewPatchR pid = do
  p <- runDB $ get404 pid
  defaultLayout $ [whamlet|
    <h1>Patch view
    Created: #{tshow $ patchCreationDate p}
    <pre>
      #{patchContent p}
  |]

getPatchGridR :: Handler Html
getPatchGridR = do
  can <- getCan
  xs :: [(Entity Patch, Entity User)] <-
    runDB . E.select . E.from $ \(p `E.InnerJoin` u) -> do
      E.on (p E.^. PatchAuthor E.==. u E.^. UserId)
      E.orderBy [E.desc $ p E.^. PatchId]
      return (p, u)
  defaultLayout $ $(widgetFile "patch-grid")

postPatchApproveR :: PatchId -> Bool -> Handler TypedContent
postPatchApproveR pid True = do
  uid <- requireAuthId
  prog <- runDB $ getBy404 $ UniqueProgrammer uid
  now <- liftIO $ getCurrentTime
  runDB $ update pid [PatchApproved =. Just (entityKey prog), PatchApprovalDate =. Just (utctDay now)]
  return $ TypedContent typePlain ""
postPatchApproveR pid False = do
  runDB $ update pid [PatchApproved =. Nothing, PatchApprovalDate =. Nothing]
  return $ TypedContent typePlain ""

postPatchDeployR :: PatchId -> Bool -> Handler TypedContent
postPatchDeployR pid True = do
  now <- liftIO $ getCurrentTime
  runDB $ update pid [PatchDeploymentDate =. Just (utctDay now)]
  return $ TypedContent typePlain ""
postPatchDeployR pid False = do
  runDB $ update pid [PatchDeploymentDate =. Nothing]
  return $ TypedContent typePlain ""

handlePatchCrudR :: CrudRoute () Patch -> Handler Html
handlePatchCrudR =
  handleCrud . flip simplerCrudToHandler PatchCrudR $
  SimplerCrud
  { crudSimplerMsg = patchMessages
  , crudSimplerDb = defaultCrudDb
  , crudSimplerForm = patchForm
  , crudSimplerTable =
      encodeClickableTable $
      mconcat
        [ headed "Name" $ \(e, mr) ->
            case mr of
              Nothing ->
                toHtml . patchName $ entityVal e
              Just r ->
                H.a
                  (toHtml . patchName $ entityVal e) H.!
                H.href (H.toValue r)
        ]
  }

handlePatchCommentCrudR :: CrudRoute () PatchComment -> Handler Html
handlePatchCommentCrudR =
  handleCrud . flip simplerCrudToHandler PatchCommentCrudR $
  SimplerCrud
  { crudSimplerMsg = patchCommentMessages
  , crudSimplerDb = defaultCrudDb
  , crudSimplerForm = patchCommentForm
  , crudSimplerTable =
      encodeClickableTable $
      mconcat
        [ headed "Name" $ \(e, mr) ->
            case mr of
              Nothing ->
                toHtml . T.take 20 . patchCommentContent $ entityVal e
              Just r ->
                H.a
                  (toHtml . T.take 20 . patchCommentContent $ entityVal e) H.!
                H.href (H.toValue r)
        ]
  }

patchUserForm :: Maybe Patch -> UserId -> Day -> Form Patch
patchUserForm m uid now =
  renderBootstrap3 BootstrapBasicForm $
  Patch <$>
  fmap unTextarea (areq textareaField (bfs ("Content(*)" :: Text)) (Textarea . patchContent <$> m)) <*>
  pure (fromMaybe uid (patchAuthor <$> m)) <*>
  pure (fromMaybe now (patchCreationDate <$> m)) <*>
  pure (fromMaybe Nothing (patchApproved <$> m)) <*>
  pure (fromMaybe Nothing (patchApprovalDate <$> m)) <*>
  pure (fromMaybe Nothing (patchDeploymentDate <$> m)) <*
  bootstrapSubmit (BootstrapSubmit MsgSave " btn-success " [])

patchForm :: Maybe Patch -> UTCTime -> Form Patch
patchForm m now =
  renderBootstrap3 BootstrapBasicForm $
  Patch <$>
  fmap unTextarea (areq textareaField (bfs ("Content(*)" :: Text)) (Textarea . patchContent <$> m)) <*>
  areq (selectField optionsUsers) (bfs ("Author(*)" :: Text)) (patchAuthor <$> m) <*>
  pure (fromMaybe (utctDay now) (patchCreationDate <$> m)) <*>
  aopt (selectField optionsProgrammers) (bfs MsgApproved) (patchApproved <$> m) <*>
  aopt dayField (bfs MsgApprovedDate) (patchApprovalDate <$> m) <*>
  aopt dayField (bfs MsgDeploymentDate) (patchDeploymentDate <$> m) <*
  bootstrapSubmit (BootstrapSubmit MsgSave " btn-success " [])

patchCommentForm :: Maybe PatchComment -> UTCTime -> Form PatchComment
patchCommentForm m _ =
  renderBootstrap3 BootstrapBasicForm $
  PatchComment <$>
  areq (selectField optionsPatches) (bfs ("Patch(*)" :: Text)) (patchCommentParent <$> m) <*>
  areq intField (bfs ("Line(*)" :: Text)) (patchCommentLine <$> m) <*>
  areq (selectField optionsUsers) (bfs ("Author(*)" :: Text)) (patchCommentAuthor <$> m) <*>
  fmap unTextarea (areq textareaField (bfs ("Description(*)" :: Text)) Nothing) <*
  bootstrapSubmit (BootstrapSubmit MsgSave " btn-success " [])
  where
    optionsPatches = optionsPersistKey [] [] (T.take 20 . patchContent)

patchMessages :: CrudMessages App Patch
patchMessages = CrudMessages
  { crudMsgBack = SomeMessage MsgBack
  , crudMsgDelete = SomeMessage MsgDelete
  , crudMsgIndex = SomeMessage MsgPatchAdminIndex
  , crudMsgNew = SomeMessage MsgPatchAdminNew
  , crudMsgEdit = SomeMessage MsgPatchAdminEdit
  , crudMsgNoEntities = SomeMessage MsgNoPatchFound
  , crudMsgCreated = SomeMessage . MsgLogPatchCreated . patchName
  , crudMsgUpdated = SomeMessage . MsgLogPatchUpdated . patchName
  , crudMsgDeleted = SomeMessage . MsgLogPatchDeleted . patchName
  }

patchCommentMessages :: CrudMessages App PatchComment
patchCommentMessages = CrudMessages
  { crudMsgBack = SomeMessage MsgBack
  , crudMsgDelete = SomeMessage MsgDelete
  , crudMsgIndex = SomeMessage MsgPatchCommentAdminIndex
  , crudMsgNew = SomeMessage MsgPatchCommentAdminNew
  , crudMsgEdit = SomeMessage MsgPatchCommentAdminEdit
  , crudMsgNoEntities = SomeMessage MsgNoPatchCommentFound
  , crudMsgCreated = SomeMessage . MsgLogPatchCommentCreated . patchCommentContent
  , crudMsgUpdated = SomeMessage . MsgLogPatchCommentUpdated . patchCommentContent
  , crudMsgDeleted = SomeMessage . MsgLogPatchCommentDeleted . patchCommentContent
  }
