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
import ClassyPrelude.Yesod hiding (Request, FormMessage(..))
import Cms.Crud
import Cms.Crud.Route (CrudRoute(..), handleCrud)
import Colonnade (headed)
import qualified Data.Text as T
import Foundation
import Message (AppMessage(..))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as H
import Yesod.Form.Bootstrap3

-- handleBugCrudR :: CrudRoute ModuleId Bug -> Handler Html
-- handleBugCrudR =
--   handleCrud . toCrudHandler $
--   (basicChildSimpleCrud getParent BugCrudR)
--   { scCrudMsg = bugMessages
--   , scForm = bugForm . either (const Nothing) Just
--   , scView = return . toHtml . T.pack . show
--   , scIndex = \mid -> do
--       can <- getCan
--       render <- getUrlRenderParams
--       xs <- runDB $ selectList [BugModule ==. mid] []
--       let entities =
--             (id &&& fmap (`render` []) . flip can "GET" . BugCrudR . EditR . entityKey) <$> xs
--       let table =
--             encodeClickableTable
--             (mconcat
--              [ headed "Name" $ \(e, mr) ->
--                  case mr of
--                    Nothing -> toHtml . bugName $ entityVal e
--                    Just r ->
--                      H.a (toHtml . bugName $ entityVal e) H.! H.href (H.toValue r)
--              ])
--             entities
--       defaultLayout $ [whamlet|$newline never
--         <h1>Index
--         <p>
--           <a href="@{BugCrudR (AddR mid)}">Add
--         #{table}
--       |]
--   }
--   where
--     getParent :: BugId -> YesodDB App ModuleId
--     getParent k = bugModule <$> get404 k

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

bugForm :: Maybe Bug -> UTCTime -> Form Bug
bugForm m _ =
  renderBootstrap3 BootstrapBasicForm $
  Bug <$> areq textField (bfs MsgName) (bugName <$> m) <*>
  areq textField (bfs MsgDescription) (bugDescription <$> m) <*>
  areq intField (bfs MsgSeverity) (bugSeverity <$> m) <*>
  aopt intField (bfs MsgVulnerability) (bugVulnerability <$> m) <*>
  areq (selectField optionsModules) (bfs MsgModule) (bugModule <$> m) <*>
  aopt (selectField optionsPatches) (bfs ("Is repaired by patch" :: Text)) (bugIsRepairedBy <$> m) <*
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
