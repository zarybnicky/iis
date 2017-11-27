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
  , EntityField(..)
  , Unique(..)
  , migrateUser
  , sendMailToUser
  ) where

import ClassyPrelude.Yesod
import Cms.Mailer.Class (CmsMailer, sendMail)
import Network.Mail.Mime (Address(..), simpleMail')
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Yesod.Auth.HashDB (HashDBUser(..))

share [mkPersist sqlSettings, mkMigrate "migrateUser"] [persistLowerCase|
User json
    ident        Text
    name         Text
    password     Text Maybe
    email        Text
    active       Bool
    token        Text Maybe
    createdAt    UTCTime
    lastLogin    UTCTime Maybe
    deletedAt    UTCTime Maybe
    UniqueUser   ident
    UniqueName   name
    UniqueEmail  email
    UniqueAuth   name active
    deriving     Typeable Show
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
      (Address (Just $ userName user) (userEmail user))
      (Address (Just "Lobinuv-zavod.cz") "lobinuv-zavod@zarybnicky.com")
      subj
      text
