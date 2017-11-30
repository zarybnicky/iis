{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Cms.Crud.Simple where

import Cms.ActionLog.Class (CmsActionLog(..))
import Cms.Crud (CrudDb(..), CrudMessages(..), defaultCrudDb)
import Cms.Crud.Route
       (CrudHandler(..), CrudRoute(..), EditParent(..), PersistCrudEntity,
        ViewParent(..))
import Data.Monoid ((<>))
import qualified Data.Text as T
import Data.Typeable
import Yesod.Core
import Yesod.Form
import Yesod.Persist

data SimpleCrud site p c = SimpleCrud
  { scAdd          :: WidgetT site IO () -> HandlerT site IO Html
  , scIndex        :: p -> HandlerT site IO Html
  , scView         :: Key c -> HandlerT site IO Html
  , scEdit         :: WidgetT site IO () -> HandlerT site IO Html
  , scDelete       :: WidgetT site IO () -> HandlerT site IO Html
  , scDeleteForm   :: WidgetT site IO ()
  , scForm         :: Either p c -> Html -> MForm (HandlerT site IO) (FormResult c, WidgetT site IO ())
  , scFormWrap     :: Enctype -> Route site -> WidgetT site IO () -> WidgetT site IO ()
  , scCrudDb       :: CrudDb site p c (Entity c)
  , scCrudMsg      :: CrudMessages site c
  , scEditParent   :: EditParent
  , scViewParent   :: ViewParent site p
  , scPromoteRoute :: CrudRoute p c -> Route site
  }

emptyParentlessSimpleCrud ::
     (PersistCrudEntity site c, Typeable c)
  => (CrudRoute () c -> Route site)
  -> SimpleCrud site () c
emptyParentlessSimpleCrud =
  SimpleCrud
    (const $ return mempty) -- add
    (const $ return mempty) -- index
    (const $ return mempty) -- view
    (const $ return mempty) -- edit
    (const $ return mempty) -- delete
    mempty -- delete form
    (const $ const $ return (FormMissing, mempty))
    (const $ const $ const mempty) -- form wrapper
    defaultCrudDb
    defaultCrudMessages
    EditParentIndex
    ViewParentIndex

emptyChildSimpleCrud ::
     (PersistCrudEntity site c, Typeable c)
  => (Key c -> YesodDB site p)
  -> (CrudRoute p c -> Route site)
  -> SimpleCrud site p c
emptyChildSimpleCrud getParent =
  SimpleCrud
    (const $ return mempty) -- add
    (const $ return mempty) -- index
    (const $ return mempty) -- view
    (const $ return mempty) -- edit
    (const $ return mempty) -- delete
    mempty -- delete form
    (const $ const $ return (FormMissing, mempty))
    (const $ const $ const mempty) -- form wrapper
    db
    defaultCrudMessages
    EditParentIndex
    ViewParentIndex
  where
    db =
      CrudDb
        (const $ selectList [] [])
        entityKey
        (const insert)
        (\k v -> do
           replace k v
           getParent k)
        (\k -> do
           p <- getParent k
           delete k
           return p)

applyBasicLayoutsAndForms ::
     PersistCrudEntity site a => SimpleCrud site p a -> SimpleCrud site p a
applyBasicLayoutsAndForms initial =
  initial
  { scIndex =
      basicSimpleCrudIndex
        (scPromoteRoute initial)
        (toWidget . toHtml . toPathPiece . entityKey)
  , scAdd = defaultLayout
  , scEdit = defaultLayout
  , scDelete = defaultLayout
  , scDeleteForm = [whamlet|<button type="submit">Delete|]
  , scFormWrap = formWrap
  }
  where
    formWrap enctype route inner =
      [whamlet|$newline never
          <form action="@{route}" enctype="#{enctype}" method="post">
            ^{inner}
        |]

basicSimpleCrudIndex ::
     (PersistCrudEntity site c)
  => (CrudRoute p c -> Route site)
  -> (Entity c -> WidgetT site IO ())
  -> p
  -> HandlerT site IO Html
basicSimpleCrudIndex tp nameFunc p = do
  cs <- runDB $ selectList [] []
  defaultLayout $ [whamlet|$newline never
    <h1>Index
    <p>
      <a href="@{tp (AddR p)}">Add
    <table.table>
      <thead>
        <tr>
          <th>ID
          <th>Edit
          <th>Delete
      <tbody>
        $forall c <- cs
          <tr>
            <td>^{nameFunc c}
            <td>
              <a href="@{tp (EditR (entityKey c))}">Edit
            <td>
              <a href="@{tp (DeleteR (entityKey c))}">Delete
  |]

basicSimpleCrud ::
     (PersistCrudEntity site c, Typeable c)
  => (CrudRoute () c -> Route site)
  -> SimpleCrud site () c
basicSimpleCrud = applyBasicLayoutsAndForms . emptyParentlessSimpleCrud

basicChildSimpleCrud ::
     (PersistCrudEntity site c, Typeable c)
  => (Key c -> YesodDB site p)
  -> (CrudRoute p c -> Route site)
  -> SimpleCrud site p c
basicChildSimpleCrud = (applyBasicLayoutsAndForms .) . emptyChildSimpleCrud

toCrudHandler ::
     (PersistCrudEntity site c, RenderMessage site FormMessage)
  => SimpleCrud site p c
  -> CrudHandler site p c
toCrudHandler SimpleCrud {..} = CrudHandler {..}
  where
    CrudDb {..} = scCrudDb
    CrudMessages {..} = scCrudMsg
    chIndex = scIndex
    chView = scView
    chDelete eid = do
      res <- runInputPostResult $ ireq textField "fake"
      case res of
        FormSuccess _ -> do
          old <- runDB (get404 eid)
          p <- runDB (crudDbDelete eid)
          logMsg (crudMsgDeleted old)
          setMessageI (crudMsgDeleted old)
          redirect (scPromoteRoute $ IndexR p)
        _ -> return ()
      scDelete $ scFormWrap
        UrlEncoded
        (scPromoteRoute $ DeleteR eid)
        ([whamlet|<input type="hidden" value="a" name="fake">|] <> scDeleteForm)
    chAdd p = do
      ((res, w), enctype) <- runFormPost . scForm $ Left p
      case res of
        FormSuccess a -> do
          _ <- runDB (crudDbAdd p a)
          logMsg (crudMsgCreated a)
          setMessageI (crudMsgCreated a)
          redirect $
            case scViewParent of
              ViewParentIndex -> scPromoteRoute $ IndexR p
              ViewParentOther f -> f p
        _ -> scAdd $ scFormWrap enctype (scPromoteRoute $ AddR p) w
    chEdit eid = do
      old <- runDB $ get404 eid
      ((res, w), enctype) <- runFormPost . scForm $ Right old
      case res of
        FormSuccess new -> do
          p <- runDB (crudDbEdit eid new)
          logMsg (crudMsgUpdated new)
          setMessageI (crudMsgUpdated new)
          redirect . scPromoteRoute $
            case scEditParent of
              EditParentView -> ViewR eid
              EditParentIndex -> IndexR p
        _ -> scEdit $ scFormWrap enctype (scPromoteRoute $ EditR eid) w

defaultCrudMessages :: Typeable e => CrudMessages site e
defaultCrudMessages = CrudMessages
  (SomeMessage ("Index" :: T.Text))
  (SomeMessage ("Add" :: T.Text))
  (SomeMessage ("Edit" :: T.Text))
  (SomeMessage ("Back" :: T.Text))
  (SomeMessage ("Delete" :: T.Text))
  (SomeMessage ("No entities found" :: T.Text))
  (\x -> SomeMessage ("Entity " <> ctrName x <> " created" :: T.Text))
  (\x -> SomeMessage ("Entity " <> ctrName x <> " updated" :: T.Text))
  (\x -> SomeMessage ("Entity " <> ctrName x <> " deleted" :: T.Text))
  where
    ctrName = T.pack . tyConName . typeRepTyCon . typeOf
