{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module App.Module.Handler where

import App.Bug.Model (Bug(..), EntityField(..))
import App.Language.Model
import App.Module.Model
import App.Ticket.Model (Ticket(..), EntityField(..))
import App.Utils (optionsProgrammers)
import ClassyPrelude.Yesod hiding (Request, FormMessage(..), on, (==.))
import Cms.Crud
import Cms.Crud.Route
import Colonnade (headed)
import Database.Esqueleto
import Foundation
import Message (AppMessage(..))
import Settings (widgetFile)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as H
import Yesod.Auth (maybeAuthId)
import Yesod.Form.Bootstrap3

getHomeR :: Handler Html
getHomeR = do
  mauth <- maybeAuthId
  case mauth of
    Nothing -> defaultLayout "Please log in or create an account to access this site"
    Just _ -> do
      modules <- runDB $ selectList [] [Asc ModuleName]
      defaultLayout $(widgetFile "home")

getModuleOverviewR :: ModuleId -> Handler Html
getModuleOverviewR mid = do
  m <- runDB $ get404 mid
  xs :: [(Maybe (Entity Ticket), Entity Bug)] <- runDB $ select $ from $ \(b `LeftOuterJoin` a `LeftOuterJoin` t) -> do
    on (a ?. AnnouncesTicket ==. t ?. TicketId)
    on (just (b ^. BugId) ==. a ?. AnnouncesBug)
    where_ (b ^. BugModule ==. val mid)
    orderBy [desc (b ^. BugId)]
    return (t, b)
  let entities = ClassyPrelude.Yesod.groupBy (\(_, a) (_, b) -> entityKey a == entityKey b) xs
  defaultLayout $(widgetFile "module-overview")

handleModuleCrudR :: CrudRoute () Module -> Handler Html
handleModuleCrudR =
  handleCrud . flip simplerCrudToHandler ModuleCrudR $
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
        , headed "Repository" $ \(e, _) -> toHtml . moduleRepository $ entityVal e
        ]
  }

moduleForm :: Maybe Module -> UTCTime -> Form Module
moduleForm m _ =
  renderBootstrap3 (BootstrapHorizontalForm (ColMd 0) (ColMd 2) (ColMd 0) (ColMd 10)) $
  Module <$>
  areq textField (bfs MsgName) (moduleName <$> m) <*>
  fmap unTextarea (areq textareaField (bfs MsgDescription) (Textarea . moduleDescription <$> m)) <*>
  areq textField (bfs MsgRepository) (moduleRepository <$> m) <*>
  areq (selectField optionsProgrammers) (bfs MsgSupervisor) (moduleSupervisor <$> m) <*>
  areq (multiSelectField optionsLanguages) (bfs MsgLanguage) Nothing <*
  bootstrapSubmit (BootstrapSubmit MsgSave " btn-success " [])
  where
    optionsLanguages = optionsPersistKey [] [Asc LanguageName] languageName

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
