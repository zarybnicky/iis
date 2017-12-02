{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module App.Patch.Handler where

import App.User.Model (User(..), Programmer(..), EntityField(..))
import App.Patch.Model (Patch(..), patchName)
import ClassyPrelude.Yesod hiding (Request, FormMessage(..))
import Cms.Crud
import Cms.Crud.Route
import Colonnade (headed)
import qualified Data.Text as T
import Foundation
import Message (AppMessage(..))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as H
import Yesod.Form.Bootstrap3

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

patchForm :: Maybe Patch -> UTCTime -> Form Patch
patchForm m _ =
  renderBootstrap3 BootstrapBasicForm $
  Patch <$>
  areq textField (bfs MsgContent) (patchContent <$> m) <*>
  areq (selectField users) (bfs MsgAuthor) (patchAuthor <$> m) <*>
  areq dayField (bfs MsgCreationDate) (patchCreationDate <$> m) <*>
  aopt (selectField programmers) (bfs MsgApproved) (patchApproved <$> m) <*>
  aopt dayField (bfs MsgApprovedDate) (patchApprovalDate <$> m) <*
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
