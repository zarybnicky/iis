{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module App.ActionLog.Model where
  -- ( ActionLogId
  -- , ActionLog(..)
  -- , EntityField(..)
  -- , Unique(..)
  -- , migrateActionLog
  -- , logMsgImpl
  -- ) where

import App.User.Model
import ClassyPrelude.Yesod
import Cms.Class
import Settings
import Yesod.Auth (AuthId, requireAuthId)

share [mkPersist sqlSettings, mkMigrate "migrateActionLog"] [persistLowerCase|
ActionLog
    ident        Text
    userId       UserId
    message      Text
    lang         Text
    createdAt    UTCTime
    UniqueLog    ident lang
    deriving     Typeable Show

Module
    x Text
    deriving     Eq Show Read Typeable
Ticket
    x Text
    deriving     Eq Show Read Typeable
Patch
    x Text
    deriving     Eq Show Read Typeable
Language
    x Text
    deriving     Eq Show Read Typeable
|]

logMsgImpl
  :: ( RenderMessage app m
     , Cms app
     , PersistRecordBackend ActionLog SqlBackend
     , AuthId app ~ UserId
     )
  => m -> HandlerT app IO ()
logMsgImpl msg = do
  y <- getYesod
  let msgs = map (id &&& (\l -> renderMessage y [l] msg)) (renderLanguages y)
  actionLogUserId    <- requireAuthId
  actionLogCreatedAt <- liftIO getCurrentTime
  actionLogIdent     <- liftIO generateUUID
  mapM_ (\(actionLogLang, actionLogMessage) -> runDB . insert_ $ ActionLog {..}) msgs
