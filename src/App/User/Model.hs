{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module App.User.Model
  ( module App.User.Model
  , EntityField(..)
  , Unique(..)
  ) where
--     UserId
--   , User(..)
--   , ProgrammerId
--   , Programmer(..)
--   , KnowledgeId
--   , Knowledge(..)
--   , migrateUser
--   , sendMailToUser

import App.Language.Model (LanguageId)
import Control.Lens.TH ()
import ClassyPrelude.Yesod
import Cms.Mailer.Class (CmsMailer, sendMail)
import Network.Mail.Mime (Address(..), simpleMail')
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Yesod.Auth.HashDB (HashDBUser(..))

share [mkPersist sqlSettings, mkMigrate "migrateUser"] [persistLowerCase|
User json
    ident        Text
    password     Text Maybe
    email        Text
    firstName    Text
    lastName     Text
    birthNumber  Int
    address      Text Maybe
    city         Text Maybe
    postalCode   Int Maybe
    active       Bool
    token        Text Maybe
    createdAt    UTCTime
    lastLogin    UTCTime Maybe
    deletedAt    UTCTime Maybe
    UniqueAuth   email active
    UniqueUser   ident
    UniqueEmail  email
    deriving     Typeable Show

Programmer
    user UserId
    contractNum Int
    commitment Int
    hourlyWage Int
    UniqueProgrammer user

Knowledge
    user UserId
    language LanguageId
    Primary user language
|]

_userIdent :: Functor f => (Text -> f Text) -> User -> f User
_userIdent f u = (\a' -> u {userIdent = a'}) <$> f (userIdent u)
_userPassword :: Functor f => (Maybe Text -> f (Maybe Text)) -> User -> f User
_userPassword f u = (\a' -> u {userPassword = a'}) <$> f (userPassword u)
_userEmail :: Functor f => (Text -> f Text) -> User -> f User
_userEmail f u = (\a' -> u {userEmail = a'}) <$> f (userEmail u)
_userFirstName :: Functor f => (Text -> f Text) -> User -> f User
_userFirstName f u = (\a' -> u {userFirstName = a'}) <$> f (userFirstName u)
_userLastName :: Functor f => (Text -> f Text) -> User -> f User
_userLastName f u = (\a' -> u {userLastName = a'}) <$> f (userLastName u)
_userBirthNumber :: Functor f => (Int -> f Int) -> User -> f User
_userBirthNumber f u = (\a' -> u {userBirthNumber = a'}) <$> f (userBirthNumber u)
_userAddress :: Functor f => (Maybe Text -> f (Maybe Text)) -> User -> f User
_userAddress f u = (\a' -> u {userAddress = a'}) <$> f (userAddress u)
_userCity :: Functor f => (Maybe Text -> f (Maybe Text)) -> User -> f User
_userCity f u = (\a' -> u {userCity = a'}) <$> f (userCity u)
_userPostalCode :: Functor f => (Maybe Int -> f (Maybe Int)) -> User -> f User
_userPostalCode f u = (\a' -> u {userPostalCode = a'}) <$> f (userPostalCode u)
_userActive :: Functor f => (Bool -> f Bool) -> User -> f User
_userActive f u = (\a' -> u {userActive = a'}) <$> f (userActive u)
_userToken :: Functor f => (Maybe Text -> f (Maybe Text)) -> User -> f User
_userToken f u = (\a' -> u {userToken = a'}) <$> f (userToken u)
_userCreatedAt :: Functor f => (UTCTime -> f UTCTime) -> User -> f User
_userCreatedAt f u = (\a' -> u {userCreatedAt = a'}) <$> f (userCreatedAt u)
_userLastLogin :: Functor f => (Maybe UTCTime -> f (Maybe UTCTime)) -> User -> f User
_userLastLogin f u = (\a' -> u {userLastLogin = a'}) <$> f (userLastLogin u)
_userDeletedAt :: Functor f => (Maybe UTCTime -> f (Maybe UTCTime)) -> User -> f User
_userDeletedAt f u = (\a' -> u {userDeletedAt = a'}) <$> f (userDeletedAt u)

instance HashDBUser User where
    userPasswordHash = userPassword
    setPasswordHash h u = u { userPassword = Just h }

sendMailToUser
  :: (CmsMailer master)
  => User
  -> Text
  -> ((Route master -> [(Text, Text)] -> Text) -> Html)
  -> HandlerT master IO ()
sendMailToUser user subj ttemp = do
  text <- renderHtml <$> withUrlRenderer ttemp
  sendMail $
    simpleMail'
      (Address (Just $ userFirstName user <> userLastName user) (userEmail user))
      (Address (Just "Lobinuv-zavod.cz") "lobinuv-zavod@zarybnicky.com")
      subj
      text
