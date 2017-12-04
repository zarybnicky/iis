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
import Cms.Crud.Route (CrudRoute(..))
import Cms.Roles.Class (getCan)
import Data.FileEmbed (embedFile)
import Foundation

unit :: ()
unit = ()

getRobotsR :: Handler TypedContent
getRobotsR = return $ TypedContent typePlain
                    $ toContent $(embedFile "config/robots.txt")

getEntitiesR :: Handler Html
getEntitiesR = do
  can <- getCan
  defaultLayout $ [whamlet|
$maybe r <- can (LanguageCrudR (IndexR unit)) "GET"
  <p>
    <a href=@{r}>Languages
$maybe r <- can (ModuleCrudR (IndexR unit)) "GET"
  <p>
    <a href=@{r}>Modules
$maybe r <- can (TicketCrudR (IndexR unit)) "GET"
  <p>
    <a href=@{r}>Tickets
$maybe r <- can (BugCrudR (IndexR unit)) "GET"
  <p>
    <a href=@{r}>Bugs
$maybe r <- can (AnnouncesCrudR (IndexR unit)) "GET"
  <p>
    <a href=@{r}>Ticket/bug links
$maybe r <- can (KnowledgeCrudR (IndexR unit)) "GET"
  <p>
    <a href=@{r}>User/language links
$maybe r <- can (PatchCrudR (IndexR unit)) "GET"
  <p>
    <a href=@{r}>Patches
|]
