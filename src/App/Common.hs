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
import Cms.Class (adminLayout)
import Cms.Roles.Class (getCan)
import Data.FileEmbed (embedFile)
import Foundation
import Message (AppMessage(..))
import Settings (widgetFile)
import Yesod.Auth (requireAuthId)

getRobotsR :: Handler TypedContent
getRobotsR = return $ TypedContent typePlain
                    $ toContent $(embedFile "config/robots.txt")
