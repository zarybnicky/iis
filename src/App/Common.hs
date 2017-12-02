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

getPatchR :: Handler Html
getPatchR = postPatchR

postPatchR :: Handler Html
postPatchR = defaultLayout [whamlet|<h1>text|]

getModuleR :: Handler Html
getModuleR = postModuleR

postModuleR :: Handler Html
postModuleR = return mempty
