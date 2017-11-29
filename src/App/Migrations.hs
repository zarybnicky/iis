{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE TypeFamilies               #-}

module App.Migrations
  ( migrateAll
  , migrateCustom
  ) where

import App.ActionLog.Model
import App.Roles.Model
import App.User.Model

import ClassyPrelude.Yesod
import Database.Persist.Sql (Migration)
import Settings (AppSettings(..), generateUUID)
import Yesod.Auth.HashDB (setPassword)

migrateAll :: Migration
migrateAll = sequence_
  [ migrateActionLog
  , migrateRoles
  , migrateUser
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
  [ insertFirstAdmin
  ]

insertFirstAdmin
  :: ( BaseBackend backend ~ SqlBackend
     , PersistStoreWrite backend
     , PersistUniqueRead backend
     , MonadIO m
     )
  => AppSettings -> ReaderT backend m ()
insertFirstAdmin s = do
  let admin = appAdmin s
  madmin <- getBy (UniqueEmail admin)
  case madmin of
    Just _ -> liftIO $ putStrLn "Admin user exists."
    Nothing -> do
      liftIO . putStrLn $ "No admin found, creating one (" ++ admin ++ ")..."
      timeNow <- liftIO getCurrentTime
      uuid <- liftIO generateUUID
      u <- setPassword "admin"
        User
        { userIdent = uuid
        , userPassword = Nothing
        , userEmail = admin
        , userFirstName = "First"
        , userLastName = "Last"
        , userBirthNumber = 0
        , userAddress = Nothing
        , userCity = Nothing
        , userPostalCode = Nothing
        , userActive = True
        , userToken = Nothing
        , userCreatedAt = timeNow
        , userLastLogin = Nothing
        , userDeletedAt = Nothing
        }
      uid <- insert u
      mapM_ (insert_ . UserRole uid) [minBound .. maxBound]
