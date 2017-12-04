{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module App.Utils
  ( optionsProgrammers
  , optionsUsers
  ) where

import App.User.Model (User(..), Programmer(..), EntityField(..))
import ClassyPrelude.Yesod hiding (on, (==.))
import Database.Esqueleto
import Foundation

optionsUsers :: Handler (OptionList (Key User))
optionsUsers = do
  mr <- getMessageRender
  pairs <- runDB $ selectList [] [Asc UserLastName]
  return . mkOptionList . flip fmap pairs $ \(Entity k u) -> Option
        { optionDisplay = mr $ userLastName u <> ", " <> userFirstName u
        , optionInternalValue = k
        , optionExternalValue = toPathPiece k
        }

optionsProgrammers :: Handler (OptionList (Key Programmer))
optionsProgrammers = do
  mr <- getMessageRender
  pairs <- runDB $ select $ from $ \(u `InnerJoin` p) -> do
    on (u ^. UserId ==. p ^. ProgrammerUser)
    orderBy [asc (u ^. UserLastName)]
    return (u, p)
  pure . mkOptionList . flip fmap pairs $ \((entityVal -> u), (entityKey -> k)) -> Option
        { optionDisplay = mr $ userLastName u <> ", " <> userFirstName u
        , optionInternalValue = k
        , optionExternalValue = toPathPiece k
        }
