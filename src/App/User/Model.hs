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
  ( UserId
  , User(..)
  , ProgrammerId
  , Programmer(..)
  , KnowledgeId
  , Knowledge(..)
  , EntityField(..)
  , Unique(..)
  , migrateUser
  , sendMailToUser
  ) where

import App.Language.Model (LanguageId)
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
