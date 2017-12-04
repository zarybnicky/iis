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

getPatchR :: Handler Html
getPatchR = postPatchR

postPatchR :: Handler Html
postPatchR = do
  uid <- requireAuthId
  now <- liftIO $ fmap utctDay getCurrentTime
  ((res, form), enctype) <- runFormPost (patchInsertForm uid now)
  case res of
    FormSuccess t -> runDB (insert t) >> redirect PatchR
    _ -> defaultLayout [whamlet|<form enctype=#{enctype}>^{form}|]

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

getPatchViewR:: PatchId -> Handler Html
getPatchViewR = pid True do 
  patch <- runDB $ selectList [] [Asc pid]
    defaultLayout 
      [whamlet|
          <ul>
              $forall Entity content patchData <- patch
                  <li>
                      <a href=@{PatchR content}>#{patchDataContent patchData}
      |]

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

patchInsertForm :: UserId -> Day -> Form Patch
patchInsertForm uid now =
  renderBootstrap3 BootstrapBasicForm $
  Patch <$>
  fmap unTextarea (areq textareaField (bfs MsgContent) Nothing) <*>
  pure uid <*>
  pure now <*>
  pure Nothing <*>
  pure Nothing <*>
  pure Nothing <*
  bootstrapSubmit (BootstrapSubmit MsgSave " btn-success " [])

patchForm :: Maybe Patch -> UTCTime -> Form Patch
patchForm m now =
  renderBootstrap3 BootstrapBasicForm $
  Patch <$>
  fmap unTextarea (areq textareaField (bfs MsgContent) (Textarea . patchContent <$> m)) <*>
  areq (selectField optionsUsers) (bfs MsgAuthor) (patchAuthor <$> m) <*>
  pure (fromMaybe (utctDay now) (patchCreationDate <$> m)) <*>
  aopt (selectField optionsProgrammers) (bfs MsgApproved) (patchApproved <$> m) <*>
  aopt dayField (bfs MsgApprovedDate) (patchApprovalDate <$> m) <*>
  aopt dayField (bfs MsgDeploymentDate) (patchDeploymentDate <$> m) <*
  bootstrapSubmit (BootstrapSubmit MsgSave " btn-success " [])

patchCommentForm :: Maybe PatchComment -> UTCTime -> Form PatchComment
patchCommentForm m _ =
  renderBootstrap3 BootstrapBasicForm $
  PatchComment <$>
  areq (selectField optionsPatches) (bfs MsgPatch) (patchCommentParent <$> m) <*>
  areq intField (bfs MsgLine) (patchCommentLine <$> m) <*>
  areq (selectField optionsUsers) (bfs MsgAuthor) (patchCommentAuthor <$> m) <*>
  fmap unTextarea (areq textareaField (bfs MsgDescription) Nothing) <*
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
