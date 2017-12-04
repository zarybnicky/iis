{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE TypeFamilies               #-}

module App.Migrations
  ( migrateAll
  , migrateCustom
  ) where

import App.ActionLog.Model
import App.Bug.Model
import App.Language.Model
import App.Module.Model
import App.Patch.Model
import App.Roles.Model
import App.Ticket.Model
import App.User.Model
import App.Vulnerability.Model
import ClassyPrelude.Yesod
import qualified Data.Text.IO as T
import Database.Persist.Sql (Migration)
import Settings (AppSettings(..), generateUUID)
import Yesod.Auth.HashDB (setPassword)

migrateAll :: Migration
migrateAll = sequence_
  [ migrateActionLog
  , migrateBug
  , migrateLanguage
  , migrateModule
  , migratePatch
  , migrateRoles
  , migrateTicket
  , migrateUser
  , migrateVulnerability
  ]

migrateCustom
  :: ( BaseBackend backend ~ SqlBackend
     , PersistStoreWrite backend
     , PersistUniqueRead backend
     , MonadIO m
     )
  => AppSettings -> ReaderT backend m ()
migrateCustom s =
  sequence_ . fmap ($ s) $
  [ insertUsers
  , insertLanguages
  ]

insertUsers
  :: ( BaseBackend backend ~ SqlBackend
     , PersistStoreWrite backend
     , PersistUniqueRead backend
     , MonadIO m
     )
  => AppSettings -> ReaderT backend m ()
insertUsers _ = do
  now <- liftIO getCurrentTime
  admin <- liftIO $ mkUser now "Admin" "Text" "admin@iis.zarybnicky.com" "admin"
  prog <- liftIO $ mkUser now "Programmer" "Text" "programmer@iis.zarybnicky.com" "programmer"
  user <- liftIO $ mkUser now "User" "Text" "user@iis.zarybnicky.com" "user"
  let users =
        [ (admin, Just (\u -> Programmer u 1 100 500), [RoleProgrammer, RoleAdmin])
        , (prog, Just (\u -> Programmer u 2 100 150), [RoleProgrammer])
        , (user, Nothing, [])
        ]
  forM_ users $ \(u, p, rs) -> do
    mu <- getBy (UniqueEmail $ userEmail u)
    case mu of
      Just _ -> liftIO . T.putStrLn $ "User " <> userEmail u <> " exists."
      Nothing -> do
        liftIO . T.putStrLn $ "User " <> userEmail u <> "found, creating them."
        uid <- insert u
        maybe (return ()) (void . insert . ($ uid)) p
        mapM_ (insert_ . UserRole uid) rs

insertLanguages
  :: ( BaseBackend backend ~ SqlBackend
     , PersistStoreWrite backend
     , PersistUniqueRead backend
     , MonadIO m
     )
  => AppSettings -> ReaderT backend m ()
insertLanguages _ = do
  let langs = [Language "C++", Language "PHP", Language "Haskell"]
  forM_ langs $ \l -> do
    m <- getBy (UniqueLanguage $ languageName l)
    case m of
      Just _ -> return ()
      Nothing -> void $ insert l

mkUser :: UTCTime -> Text -> Text -> Text -> Text -> IO User
mkUser now name surname email pass = do
  uuid <- liftIO generateUUID
  setPassword pass $
    User
      uuid
      Nothing
      email
      name
      surname
      0
      Nothing
      Nothing
      Nothing
      True
      Nothing
      now
      Nothing
      Nothing
      []
