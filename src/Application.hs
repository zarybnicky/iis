{-# LANGUAGE CPP                   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE QuasiQuotes	       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}

module Application
  ( getApplicationDev
  , appMain
  , makeFoundation
  , makeLogWare
  , handler
  , db
  ) where

#ifdef DEVELOPMENT
import Database.Persist.Sqlite
       (createSqlitePool, sqlDatabase, sqlPoolSize, runSqlPool)
#else
import Database.Persist.Postgresql
       (createPostgresqlPool, pgConnStr, pgPoolSize, runSqlPool)
#endif

import ClassyPrelude.Yesod hiding (Request)
import Cms.Crud.Route (CrudRoute)
import Control.Monad.Logger (liftLoc, runLoggingT)
import Foundation
import Language.Haskell.TH.Syntax (qLocation)
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp
       (Settings, defaultSettings, defaultShouldDisplayException,
        runSettings, setHost, setOnException, setPort)
import Network.Wai.Middleware.MethodOverridePost (methodOverridePost)
import Network.Wai.Middleware.RequestLogger
       (Destination(Logger), IPAddrSource(..), OutputFormat(..),
        destination, mkRequestLogger, outputFormat)
import Settings
import System.Log.FastLogger
       (defaultBufSize, newStdoutLoggerSet, toLogStr)
import Yesod.Auth (getAuth, requireAuthId)
import Yesod.Core.Types (loggerSet)
import Yesod.Default.Config2

import App.Common
import App.Migrations (migrateAll, migrateCustom)

import App.ActionLog.Model (ActionLog)
import App.ActionLog.Handler
import App.Module.Model (Module, ModuleId)
import App.Module.Handler (handleModuleR)
import App.Ticket.Model (Ticket, TicketId)
import App.Ticket.Handler (handleTicketR)
import App.Patch.Model (Patch)
import App.Patch.Handler (handlePatchR)
import App.Language.Model (Language)
import App.Language.Handler (handleLanguageR)
import App.User.Handler

mkYesodDispatch "App" resourcesApp

-- | This function allocates resources (such as a database connection pool),
-- performs initialization and returns a foundation datatype value. This is also
-- the place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeFoundation :: AppSettings -> IO App
makeFoundation appSettings = do
  appHttpManager <- newManager
  appLogger <- newStdoutLoggerSet defaultBufSize >>= makeYesodLogger
  appStatic <-
    (if appMutableStatic appSettings
       then staticDevel
       else static)
      (appStaticDir appSettings)
  let mkFoundation  appConnPool = App {..}
      tempFoundation = mkFoundation $ error "connPool forced in tempFoundation"
      logFunc = messageLoggerSource tempFoundation appLogger
  pool <-
#ifdef DEVELOPMENT
    flip runLoggingT logFunc $ createSqlitePool
         (sqlDatabase $ appDatabaseConf appSettings)
         (sqlPoolSize $ appDatabaseConf appSettings)
#else
    flip runLoggingT logFunc $ createPostgresqlPool
         (pgConnStr  $ appDatabaseConf appSettings)
         (pgPoolSize $ appDatabaseConf appSettings)
#endif
  let theFoundation = mkFoundation pool
  runLoggingT
    (flip runSqlPool pool $ do
      runMigration migrateAll
      migrateCustom appSettings)
Ticket    (messageLoggerSource theFoundation appLogger)
  return theFoundation

-- | Convert our foundation to a WAI Application by calling @toWaiAppPlain@ and
-- applying some additional middlewares.
makeApplication :: App -> IO Application
makeApplication foundation = do
  logWare <- makeLogWare foundation
  logWare . methodOverridePost <$> toWaiAppPlain foundation

makeLogWare :: App -> IO Middleware
makeLogWare foundation =
  mkRequestLogger
    def
    { outputFormat =
        if appDetailedRequestLogging $ appSettings foundation
          then Detailed True
          else Apache
                 (if appIpFromHeader $ appSettings foundation
                    then FromFallback
                    else FromSocket)
    , destination = Logger $ loggerSet $ appLogger foundation
    }

warpSettings :: App -> Settings
warpSettings foundation =
  setPort (appPort $ appSettings foundation) $
  setHost (appHost $ appSettings foundation) $
  setOnException
    (\_req e ->
       when (defaultShouldDisplayException e) $
       messageLoggerSource
         foundation
         (appLogger foundation)
         $(qLocation >>= liftLoc)
         "yesod"
         LevelError
         (toLogStr $ "Exception from Warp: " ++ show e))
    defaultSettings

-- | For yesod devel, return the Warp settings and WAI Application.
getApplicationDev :: IO (Settings, Application)
getApplicationDev = do
    settings <- getAppSettings
    foundation <- makeFoundation settings
    wsettings <- getDevSettings $ warpSettings foundation
    app <- makeApplication foundation
    return (wsettings, app)

getAppSettings :: IO AppSettings
getAppSettings = loadYamlSettings [configSettingsYml] [] useEnv

-- | The @main@ function for an executable running this site.
appMain :: IO ()
appMain = do
  settings <- loadYamlSettingsArgs [configSettingsYmlValue] useEnv
  foundation <- makeFoundation settings
  app <- makeApplication foundation
  runSettings (warpSettings foundation) app


---------------------------------------------
-- Functions for use in development with GHCi
---------------------------------------------

-- | Run a handler
handler :: Handler a -> IO a
handler h = getAppSettings >>= makeFoundation >>= flip unsafeHandler h

-- | Run DB queries
db :: ReaderT SqlBackend (HandlerT App IO) a -> IO a
db = handler . runDB
