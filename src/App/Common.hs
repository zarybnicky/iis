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

getHomeR :: Handler Html
getHomeR = defaultLayout $ $(widgetFile "homepage")

getRobotsR :: Handler TypedContent
getRobotsR = return $ TypedContent typePlain
                    $ toContent $(embedFile "config/robots.txt")

getModuleR :: Handler Html
getModuleR = postModuleR

postModuleR :: Handler Html
postModuleR = defaultLayout [whamlet|<h1>text|]

