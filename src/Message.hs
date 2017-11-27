{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module Message
  ( AppMessage(..)
  , renderAppMessage
  ) where

import ClassyPrelude.Yesod hiding (FormMessage(..))

data App = App

mkMessage "App" "messages" "en"

renderAppMessage :: [Text] -> AppMessage -> Text
renderAppMessage = renderMessage App
