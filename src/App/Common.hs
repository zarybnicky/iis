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
