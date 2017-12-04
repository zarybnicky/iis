{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module App.Patch.Handler where

import App.User.Model (UserId)
import App.Patch.Model (Patch(..), patchName)
import App.Utils (optionsUsers, optionsProgrammers)
import ClassyPrelude.Yesod hiding (Request, FormMessage(..))
import Cms.Crud
import Cms.Crud.Route
import Colonnade (headed)
import Foundation
import Message (AppMessage(..))
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
    _ -> defaultLayout [whamlet|
           <form enctype=#{enctype}>
             ^{form}
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

patchInsertForm :: UserId -> Day -> Form Patch
patchInsertForm uid now =
  renderBootstrap3 BootstrapBasicForm $
  Patch <$> 
  fmap unTextarea (areq textareaField (bfs MsgDescription) Nothing) <*>
  pure uid <*>
  pure now <*>
  pure Nothing <*>
  pure Nothing <*>
  pure Nothing <*
  bootstrapSubmit (BootstrapSubmit MsgSave " btn-success " [])

patchForm :: Maybe Patch -> UTCTime -> Form Patch
patchForm m _ =
  renderBootstrap3 BootstrapBasicForm $
  Patch <$>
  areq textField (bfs MsgContent) (patchContent <$> m) <*>
  areq (selectField optionsUsers) (bfs MsgAuthor) (patchAuthor <$> m) <*>
  areq dayField (bfs MsgCreationDate) (patchCreationDate <$> m) <*>
  aopt (selectField optionsProgrammers) (bfs MsgApproved) (patchApproved <$> m) <*>
  aopt dayField (bfs MsgApprovedDate) (patchApprovalDate <$> m) <*>
  aopt dayField (bfs MsgDeploymentDate) (patchDeploymentDate <$> m) <*
  bootstrapSubmit (BootstrapSubmit MsgSave " btn-success " [])

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
