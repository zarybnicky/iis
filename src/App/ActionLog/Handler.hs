{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module App.ActionLog.Handler
       ( getActionLogAdminIndexR
       , getActionLogAdminUserR
       ) where

import App.ActionLog.Model (ActionLog(..), EntityField(..))
import App.User.Model
import ClassyPrelude.Yesod hiding (Request, intersect)
import Cms.Class (renderLanguages)
import Cms.Roles.Class (getCan)
import Data.Int (Int64)
import Data.List (intersect)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Time.Format.Human (humanReadableTimeI18N')
import Database.Esqueleto ((^.))
import qualified Database.Esqueleto as E
import Foundation
import Message
import Network.Wai (Request)
import Text.Read (readEither)
import Yesod.Core.Types (ResolvedApproot)


-- | REST/JSON endpoint for fetching the recent logs.
getActionLogAdminIndexR :: Handler TypedContent
getActionLogAdminIndexR = getActionLogAdminJson Nothing

-- | REST/JSON endpoint for fetching the recent logs of a particular user.
getActionLogAdminUserR :: UserId -> Handler TypedContent
getActionLogAdminUserR userId = getActionLogAdminJson (Just userId)

data JsonLog = JsonLog
  { message :: Text
  , username :: Text
  , userUrl :: Maybe Text
  , timeAgo :: String
  }

-- TODO: The ToJSON instance could move directly to the ActionLog model.
instance ToJSON JsonLog where
  toJSON JsonLog {..} =
    object
      [ "message" .= message
      , "username" .= username
      , "userUrl" .= userUrl
      , "timeAgo" .= timeAgo
      ]

getActionLogAdminJson :: Maybe UserId -> Handler TypedContent
getActionLogAdminJson mUserId =
  selectRep . provideRep $ do
    (limit, offset) <- getFilters
    lang <- getCurrentLang
    can <- getCan
    y <- getYesod
    req <- waiRequest
    timeNow <- liftIO getCurrentTime
    hrtLocale <- getHumanTimeLocale
    let renderUrl = flip (yesodRender y (resolveApproot y req)) []
        toAgo = humanReadableTimeI18N' hrtLocale timeNow
    logs <- getActionLogs mUserId limit offset lang
    jsonLogs <- mapM (logToJsonLog can renderUrl toAgo) logs
    returnJson jsonLogs

getActionLogs
  :: Maybe UserId
  -> Int64
  -> Int64
  -> Text
  -> Handler [(Entity ActionLog, Entity User)]
getActionLogs mUserId limit offset lang =
  runDB . E.select $
  E.from $ \(log' `E.InnerJoin` user) -> do
    E.on $ log' ^. ActionLogUserId E.==. user ^. UserId
    E.where_ $ log' ^. ActionLogLang E.==. E.val lang
    maybe (return ()) (E.where_ . (E.==.) (user ^. UserId) . E.val) mUserId
    E.limit limit
    E.offset offset
    E.orderBy [E.desc (log' ^. ActionLogCreatedAt)]
    return (log', user)

logToJsonLog
  :: (IsString method, Monad m)
  => (Route App -> method -> Maybe r)
  -> (r -> Text)
  -> (UTCTime -> String)
  -> (Entity ActionLog, Entity User)
  -> m JsonLog
logToJsonLog can renderUrl toAgo (Entity _ log', Entity userId user) =
  return
    JsonLog
    { message = actionLogMessage log'
    , username = userName user
    , userUrl =
        renderUrl <$> can (UserAdminR $ UserAdminEditR userId) "GET"
    , timeAgo = toAgo $ actionLogCreatedAt log'
    }

resolveApproot :: Yesod master => master -> Request -> ResolvedApproot
resolveApproot master req =
  case approot of
    ApprootRelative -> ""
    ApprootStatic t -> t
    ApprootMaster f -> f master
    ApprootRequest f -> f master req

getCurrentLang :: Handler Text
getCurrentLang = do
  langs <- languages
  y <- getYesod
  return . fromMaybe "en" . listToMaybe $ langs `intersect` renderLanguages y

getFilters :: Handler (Int64, Int64)
getFilters = do
  mLimitText <- lookupGetParam "limit"
  mOffsetText <- lookupGetParam "offset"
  case (defaultTo 10 mLimitText, defaultTo 0 mOffsetText) of
    (Left _, Left _) -> invalidArgsI [MsgInvalidLimit, MsgInvalidOffset]
    (Left _, _) -> invalidArgsI [MsgInvalidLimit]
    (_, Left _) -> invalidArgsI [MsgInvalidOffset]
    (Right limit, Right offset) -> return (limit, offset)
  where
    defaultTo d mText = fromMaybe (Right d) (readEither . unpack <$> mText)
