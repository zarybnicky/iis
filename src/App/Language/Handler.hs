{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module App.Language.Handler where

import App.Language.Model (Language(..))
import ClassyPrelude.Yesod hiding (Request, FormMessage(..))
import Cms.Crud
import Cms.Crud.Route
import Colonnade (headed)
import Foundation
import Message (AppMessage(..))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as H
import Yesod.Form.Bootstrap3

handleLanguageR :: CrudRoute () Language -> Handler Html
handleLanguageR =
  handleCrud . flip simplerCrudToHandler LanguageR $
  SimplerCrud
  { crudSimplerMsg = languageMessages
  , crudSimplerDb = defaultCrudDb
  , crudSimplerForm = languageForm
  , crudSimplerTable =
      encodeClickableTable $
      mconcat
        [ headed "Name" $ \(e, mr) ->
            case mr of
              Nothing -> toHtml . languageName $ entityVal e
              Just r ->
                H.a (toHtml . languageName $ entityVal e) H.! H.href (H.toValue r)
        ]
  }

languageForm :: Maybe Language -> UTCTime -> Form Language
languageForm m _ =
  renderBootstrap3 BootstrapBasicForm $
  Language <$>
  areq textField (bfs MsgName) (languageName <$> m) <*
  bootstrapSubmit (BootstrapSubmit MsgSave " btn-success " [])

languageMessages :: CrudMessages App Language
languageMessages = CrudMessages
  { crudMsgBack = SomeMessage MsgBack
  , crudMsgDelete = SomeMessage MsgDelete
  , crudMsgIndex = SomeMessage MsgLanguageAdminIndex
  , crudMsgNew = SomeMessage MsgLanguageAdminNew
  , crudMsgEdit = SomeMessage MsgLanguageAdminEdit
  , crudMsgNoEntities = SomeMessage MsgNoLanguageFound
  , crudMsgCreated = SomeMessage . MsgLogLanguageCreated . languageName
  , crudMsgUpdated = SomeMessage . MsgLogLanguageUpdated . languageName
  , crudMsgDeleted = SomeMessage . MsgLogLanguageDeleted . languageName
  }
