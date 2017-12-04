{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module App.Module.Handler where

import App.Bug.Model (Bug(..), EntityField(..))
import App.Module.Model (ModuleId, Module(..), EntityField(..))
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
  xs :: [(Entity Ticket, Entity Bug)] <- runDB $ select $ from $ \(a `InnerJoin` b `InnerJoin` t) -> do
    on (b ^. BugId ==. a ^. AnnouncesBug)
    on (a ^. AnnouncesTicket ==. t ^. TicketId)
    where_ (b ^. BugModule ==. val mid)
    orderBy [asc (t ^. TicketId)]
    return (t, b)
  let entities = ClassyPrelude.Yesod.groupBy (\(a, _) (b, _) -> entityKey a == entityKey b) xs
  defaultLayout $(widgetFile "module-overview")

getModuleR :: Handler Html
getModuleR = postModuleR

postModuleR :: Handler Html
postModuleR = do
  ((res, form), enctype) <- runFormPost (moduleInsertForm)
  case res of
    FormSuccess t -> runDB (insert t) >> redirect ModuleR
    _ -> defaultLayout [whamlet|
           <form enctype=#{enctype}>
             ^{form}
         |]

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
        ]
  }

moduleInsertForm :: Form Module
moduleInsertForm =
  renderBootstrap3 BootstrapBasicForm $
  Module <$> 
  areq textField (bfs MsgName) Nothing <*>
  fmap unTextarea (areq textareaField (bfs MsgDescription) Nothing) <*>
  areq textField (bfs MsgRepository) Nothing <*>
  areq (selectField optionsProgrammers) (bfs MsgSupervisor) Nothing <*
  bootstrapSubmit (BootstrapSubmit MsgSave " btn-success " [])

moduleForm :: Maybe Module -> UTCTime -> Form Module
moduleForm m _ =
  renderBootstrap3 (BootstrapHorizontalForm (ColMd 0) (ColMd 2) (ColMd 0) (ColMd 10)) $
  Module <$>
  areq textField (bfs MsgName) (moduleName <$> m) <*>
  areq textField (bfs MsgDescription) (moduleDescription <$> m) <*>
  areq textField (bfs MsgRepository) (moduleRepository <$> m) <*>
  areq (selectField optionsProgrammers) (bfs MsgSupervisor) (moduleSupervisor <$> m) <*
  bootstrapSubmit (BootstrapSubmit MsgSave " btn-success " [])

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
