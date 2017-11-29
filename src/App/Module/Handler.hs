{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module App.Module.Handler where

import App.User.Model (Programmer(..), EntityField(..))
import App.Module.Model (Module(..))
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

handleModuleR :: CrudRoute () Module -> Handler Html
handleModuleR =
  handleCrud . flip simplerCrudToHandler ModuleR $
  SimplerCrud
  { crudSimplerMsg = moduleMessages
  , crudSimplerDb = defaultCrudDb
  , crudSimplerForm = moduleForm
  , crudSimplerTable =
      encodeClickableTable $
      mconcat
        [ headed "Name" $ \(e, mr) ->
            case mr of
              Nothing -> toHtml . moduleName $ entityVal e
              Just r ->
                H.a (toHtml . moduleName $ entityVal e) H.! H.href (H.toValue r)
        ]
  }

moduleForm :: Maybe Module -> UTCTime -> Form Module
moduleForm m _ =
  renderBootstrap3 BootstrapBasicForm $
  Module <$>
  areq textField (bfs MsgName) (moduleName <$> m) <*>
  areq textField (bfs MsgDescription) (moduleDescription <$> m) <*>
  areq textField (bfs MsgRepository) (moduleRepository <$> m) <*>
  areq (selectField programmers) (bfs MsgSupervisor) (moduleSupervisor <$> m) <*
  bootstrapSubmit (BootstrapSubmit MsgSave " btn-success " [])
  where
    programmers =
      optionsPersistKey
        []
        [Asc ProgrammerContractNum]
        (T.pack . show . programmerContractNum)

moduleMessages :: CrudMessages App Module
moduleMessages = CrudMessages
  { crudMsgBack = SomeMessage MsgBack
  , crudMsgDelete = SomeMessage MsgDelete
  , crudMsgIndex = SomeMessage MsgModuleAdminIndex
  , crudMsgNew = SomeMessage MsgModuleAdminNew
  , crudMsgEdit = SomeMessage MsgModuleAdminEdit
  , crudMsgNoEntities = SomeMessage MsgNoModuleFound
  , crudMsgCreated = SomeMessage . MsgLogModuleCreated . moduleName
  , crudMsgUpdated = SomeMessage . MsgLogModuleUpdated . moduleName
  , crudMsgDeleted = SomeMessage . MsgLogModuleDeleted . moduleName
  }
