{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}

module App.Common where

import ClassyPrelude.Yesod hiding (Request)
import Data.FileEmbed (embedFile)
import Foundation
import Settings (widgetFile)
import Yesod.Auth (requireAuthId)

getHomeR :: Handler Html
getHomeR = defaultLayout $ $(widgetFile "homepage")

getProfileR :: Handler Html
getProfileR = do
  uid <- requireAuthId
  _user <- runDB $ get404 uid
  defaultLayout [whamlet|<h1>text|]

postProfileR :: Handler Html
postProfileR = getProfileR

getRobotsR :: Handler TypedContent
getRobotsR = return $ TypedContent typePlain
                    $ toContent $(embedFile "config/robots.txt")

getRegistrationR :: Handler Html
getRegistrationR = do
    ((res, form), enctype) <- myForm
    defaultLayout $ $(widgetFile "registration")

myForm = runFormPost $ renderDivs $ pure (,,,,,,,,,)
    <*> pure "Register form"
    <*> areq textField "First Name" Nothing
    <*> areq textField "Last Name" Nothing
    <*> areq textField "Email" Nothing
    <*> areq textField "Password" Nothing
    <*> areq textField "Confirm Password" Nothing
    <*> areq intField "Birth Number" Nothing
    <*> aopt textField "Address" Nothing
    <*> aopt textField "City" Nothing
    <*> aopt intField "Postal Code" Nothing

postRegistrationR :: Handler Html
postRegistrationR = getRegistrationR