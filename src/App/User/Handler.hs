{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module App.User.Handler where

import App.Language.Model
import App.User.Model
import App.Utils (optionsUsers)
import ClassyPrelude.Yesod hiding (Request, FormMessage(..))
import Cms.Class (adminLayout)
import Cms.Crud
import Cms.Crud.Route
import Cms.ActionLog.Class (logMsg)
import Cms.Roles.Class (Roles, getCan, getUserRoles, setUserRoles, mayAssignRoles, defaultRoles)
import Colonnade (headed)
import Control.Arrow ((&&&))
import Control.Lens
import Data.Maybe (fromJust, fromMaybe, isJust)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time.Format.Human
import qualified Database.Esqueleto as E
import Foundation
import Message (AppMessage(..))
import Settings
import Text.Hamlet (hamletFile)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as H
import Yesod.Auth (Creds(..), Route(LoginR), requireAuthId, setCreds, authLayout)
import Yesod.Auth.Email (saltPass)
import Yesod.Auth.HashDB (setPassword)
import Yesod.Form.Bootstrap3

getRegistrationR :: Handler Html
getRegistrationR = postRegistrationR

postRegistrationR :: Handler Html
postRegistrationR = do
  now <- liftIO getCurrentTime
  ident <- liftIO generateUUID
  opw <- lookupPostParam "original-pw"
  ((res, form), enctype) <- runFormPost (registrationForm opw now ident)
  case res of
    FormSuccess (u, pwd) -> do
      u' <- setPassword (originalPassword pwd) u
      _ <- runDB $ insert u'
      redirect (AuthR LoginR)
    _ -> return ()
  defaultLayout $ $(widgetFile "registration")

registrationForm :: Maybe Text -> UTCTime -> Text -> Form (User, ComparePassword)
registrationForm original now ident =
  renderBootstrap3 BootstrapBasicForm $
  (,) <$>
  (User <$>
   pure ident <*>
   pure Nothing <*>
   areq textField (bfs MsgEmail) Nothing <*>
   areq textField (bfs MsgFirstName) Nothing <*>
   areq textField (bfs MsgLastName) Nothing <*>
   areq intField (bfs MsgBirthNumber) Nothing <*>
   aopt textField (bfs MsgAddress) Nothing <*>
   aopt textField (bfs MsgCity) Nothing <*>
   aopt intField (bfs MsgPostalCode) Nothing <*>
   pure True <*>
   pure Nothing <*>
   pure now <*>
   pure Nothing <*>
   pure Nothing) <*>
  (ComparePassword
    <$> areq passwordField (withName "original-pw" $ bfs MsgPassword) Nothing
    <*> areq comparePasswordField (bfs MsgConfirm) Nothing) <*
  bootstrapSubmit (BootstrapSubmit MsgSave " btn-success " [])
  where
    comparePasswordField = check comparePasswords passwordField
    comparePasswords pw
      | pw == fromMaybe "" original = Right pw
      | otherwise = Left MsgPasswordMismatch

-- | Data type used by the change password form.
data ComparePassword = ComparePassword
  { originalPassword :: Text
  , _confirmPassword :: Text
  } deriving (Show, Eq)

-- | Form by which account setting are changed.
accountSettingsForm
  :: User
  -> Maybe Programmer
  -> S.Set (Roles App)
  -> Maybe AppMessage
  -> Html
  -> MForm Handler (FormResult (User, Maybe (Key User -> Programmer), [Roles App]), Widget)
accountSettingsForm user mprog roles mlabel extra = do
  maRoles <- lift mayAssignRoles
  -- User fields
  (fnameR, fnameV) <- mreq textField (bfs MsgFirstName) (Just $ userFirstName user)
  (lnameR, lnameV) <- mreq textField (bfs MsgLastName) (Just $ userLastName user)
  (emailR, emailV) <- mreq emailField (bfs MsgEmailAddress) (Just $ userEmail user)
  (birthR, birthV) <- mreq intField (bfs MsgBirthNumber) (Just $ userBirthNumber user)
  (addrR, addrV) <- mopt textField (bfs MsgAddress) (Just $ userAddress user)
  (cityR, cityV) <- mopt textField (bfs MsgCity) (Just $ userCity user)
  (postR, postV) <- mopt intField (bfs MsgPostalCode) (Just $ userPostalCode user)

  (rolesR, mrolesV) <- if maRoles
    then second Just <$> mreq (checkboxesField roleList) "Not used" (Just $ S.toList roles)
    else return (FormSuccess $ S.toList roles, Nothing)

  let handleProg True a b c = Just $ \uid -> Programmer uid a b c
      handleProg False _ _ _ = Nothing
  (mProgR, mProgV) <- if maRoles
    then do
    (isProgR, isProgV) <- mreq checkBoxField "Is a programmer?" (Just $ isJust mprog)
    (contractR, contractV) <- mreq intField (bfs MsgContractNum) (programmerContractNum <$> mprog)
    (commitR, commitV) <- mreq intField (bfs MsgCommitment) (programmerCommitment <$> mprog)
    (wageR, wageV) <- mreq intField (bfs MsgHourlyWage) (programmerHourlyWage <$> mprog)
    let progR = handleProg <$> isProgR <*> contractR <*> commitR <*> wageR
    return (progR, Just (isProgV, contractV, commitV, wageV))
    else return (FormSuccess $ const <$> mprog, Nothing)

  let userR = pure user
        <**> fmap (set _userFirstName) fnameR
        <**> fmap (set _userLastName) lnameR
        <**> fmap (set _userEmail) emailR
        <**> fmap (set _userBirthNumber) birthR
        <**> fmap (set _userAddress) addrR
        <**> fmap (set _userCity) cityR
        <**> fmap (set _userPostalCode) postR

  return ((,,) <$> userR <*> mProgR <*> rolesR, $(widgetFile "user/settings-form"))
  where
    roleList = optionsPairs $ map ((T.pack . show) &&& id) [minBound .. maxBound]

-- | Webform for changing a user's password.
userChangePasswordForm :: Maybe Text -> Maybe AppMessage -> Form ComparePassword
userChangePasswordForm original submit =
    renderBootstrap3 BootstrapBasicForm $ ComparePassword
    <$> areq validatePasswordField (withName "original-pw" $ bfs MsgPassword) Nothing
    <*> areq comparePasswordField  (bfs MsgConfirm) Nothing
    <*  bootstrapSubmit (BootstrapSubmit (fromMaybe MsgSubmit submit) " btn-success " [])
    where
        validatePasswordField = check validatePassword passwordField
        validatePassword pw
            | T.length pw >= 8 = Right pw
            | otherwise = Left MsgPasswordTooShort

        comparePasswordField = check comparePasswords passwordField
        comparePasswords pw
            | pw == fromMaybe "" original = Right pw
            | otherwise = Left MsgPasswordMismatch

-- | Helper to create a user with email address.
generateUserWithEmail :: Text -> IO User
generateUserWithEmail e = do
  uuid <- generateUUID
  token <- generateUUID
  timeNow <- getCurrentTime
  return
    User
    { userIdent = uuid
    , userFirstName = ""
    , userLastName = ""
    , userPassword = Nothing
    , userBirthNumber = 0
    , userAddress = Nothing
    , userCity = Nothing
    , userPostalCode = Nothing
    , userEmail = e
    , userActive = False
    , userToken = Just token
    , userCreatedAt = timeNow
    , userLastLogin = Nothing
    , userDeletedAt = Nothing
    }

-- | Helper to create an empty user.
emptyUser :: IO User
emptyUser = generateUserWithEmail ""

-- | Validate an activation token.
validateUserToken :: Text -> User -> Maybe Bool
validateUserToken token = fmap (== token) . userToken

-- | Send an email to the user with a link containing the activation token.
sendAccountActivationToken :: Entity User -> Handler ()
sendAccountActivationToken (Entity userId user) =
  case userToken user of
    Just token ->
      sendMailToUser
        user
        "Account activation"
        $(hamletFile "templates/mail/activation-text.hamlet")
    Nothing -> error "No activation token found"

-- | Send an email to the user with a link containing the reset token.
sendAccountResetToken :: Entity User -> Handler ()
sendAccountResetToken (Entity userId user) =
  case userToken user of
    Just token ->
      sendMailToUser
        user
        "Account password reset"
        $(hamletFile "templates/mail/reset-text.hamlet")
    Nothing -> error "No reset token found"

-- | User overview.
getUserAdminIndexR :: Handler Html
getUserAdminIndexR = do
  timeNow <- liftIO getCurrentTime
  can <- getCan
  (users' :: [(Entity User, Maybe (Entity Programmer))]) <-
    runDB $ E.select $ E.from $ \(u `E.LeftOuterJoin` p) -> do
    E.on (E.just (u E.^. UserId) E.==. p E.?. ProgrammerUser)
    return (u, p)
  users <-
    mapM
    (\user -> do
        ur <- getUserRoles $ entityKey $ fst user
        return (user, S.toList ur))
    users'
  hrtLocale <- getHumanTimeLocale
  adminLayout $ do
    setTitleI MsgUserIndex
    $(widgetFile "user/index")

-- | Create a new user, show the form.
getUserAdminNewR :: Handler Html
getUserAdminNewR = do
  eu <- liftIO emptyUser
  can <- getCan
  drs <- defaultRoles
  (formWidget, enctype) <-
    generateFormPost $ accountSettingsForm eu Nothing drs (Just MsgCreate)
  adminLayout $ do
    setTitleI MsgNewUser
    $(widgetFile "user/new")

-- | Create a new user, handle a posted form.
postUserAdminNewR :: Handler Html
postUserAdminNewR = do
  eu <- liftIO emptyUser
  drs <- defaultRoles
  ((formResult, formWidget), enctype) <-
    runFormPost $ accountSettingsForm eu Nothing drs (Just MsgCreate)
  case formResult of
    FormSuccess (user, mprog, roles) -> do
      userId <- runDB $ insert user
      case mprog of
        Nothing -> return ()
        Just prog -> void . runDB $ insert (prog userId)
      setUserRoles userId (S.fromList roles)
      sendAccountActivationToken (Entity userId user)
      logMsg $ MsgLogUserCreated (userEmail user)
      setMessageI MsgSuccessCreate
      redirectUltDest $ UserAdminR UserAdminIndexR
    _ -> do
      can <- getCan
      adminLayout $ do
        setTitleI MsgNewUser
        $(widgetFile "user/new")

-- | Show the forms to edit an existing user.
getUserAdminEditR :: UserId -> Handler Html
getUserAdminEditR userId = do
  timeNow <- liftIO getCurrentTime
  authId <- requireAuthId
  can <- getCan
  user <- runDB $ get404 userId
  mprog <- fmap (fmap entityVal) . runDB . getBy $ UniqueProgrammer userId
  urs <- getUserRoles userId
  hrtLocale <- getHumanTimeLocale
  (formWidget, enctype) <-
    generateFormPost $
    accountSettingsForm user mprog urs (Just MsgSave) -- user form
  (pwFormWidget, pwEnctype) <-
    generateFormPost $
    userChangePasswordForm Nothing (Just MsgChange) -- user password form
  adminLayout $ do
    setTitleI . MsgEditUser $ userEmail user
    $(widgetFile "user/edit")

-- | Change a user's main properties.
patchUserAdminEditR :: UserId -> Handler Html
patchUserAdminEditR userId = do
  (user, timeNow, hrtLocale, urs) <- updateHelper userId
  mProg <- runDB . getBy $ UniqueProgrammer userId
  (pwFormWidget, pwEnctype) <-
    generateFormPost $ userChangePasswordForm Nothing (Just MsgChange)
  ((formResult, formWidget), enctype) <-
    runFormPost $ accountSettingsForm user (entityVal <$> mProg) urs (Just MsgSave)
  case formResult of
    FormSuccess (updatedUser, updatedProg, updatedRoles) -> do
      _ <- runDB $ replace userId updatedUser
      case (mProg, updatedProg) of
        (Just prog, Just updated) -> runDB $ replace (entityKey prog) (updated userId)
        (Just prog, Nothing) -> runDB $ delete (entityKey prog)
        (Nothing, Just updated) -> void $ runDB $ insert (updated userId)
        (Nothing, Nothing) -> return ()
      setUserRoles userId (S.fromList updatedRoles)
      logMsg $ MsgLogUserUpdated (userEmail updatedUser)
      setMessageI MsgSuccessReplace
      redirect . UserAdminR $ UserAdminEditR userId
    _ -> do
      authId <- requireAuthId
      can <- getCan
      adminLayout $ do
        setTitleI . MsgEditUser $ userEmail user
        $(widgetFile "user/edit")

-- | Helper function to get data required for some DB updates operations in
-- handlers.  Removes code duplication.
updateHelper :: Key User
             -> Handler (User, UTCTime, HumanTimeLocale, S.Set (Roles App))
updateHelper userId = do
  user <- runDB $ get404 userId
  timeNow <- liftIO getCurrentTime
  hrtLocale <- getHumanTimeLocale
  roles <- getUserRoles userId
  return (user, timeNow, hrtLocale, roles)

chpassUserAdminEditR :: UserId -> Handler Html
chpassUserAdminEditR userId = do
  authId <- requireAuthId
  when (userId /= authId) (permissionDenied "You can change only your own password")

  (user, timeNow, hrtLocale, urs) <- updateHelper userId
  mProg <- runDB . getBy $ UniqueProgrammer userId
  (formWidget, enctype) <-
    generateFormPost $ accountSettingsForm user (entityVal <$> mProg) urs (Just MsgSave)
  opw <- lookupPostParam "original-pw"
  ((formResult, pwFormWidget), pwEnctype) <-
    runFormPost $ userChangePasswordForm opw (Just MsgChange)
  case formResult of
    FormSuccess f -> do
      saltedPassword <- liftIO . saltPass $ originalPassword f
      _ <- runDB $ update userId [UserPassword =. Just saltedPassword]
      logMsg $ MsgLogUserChangedPassword (userEmail user)
      setMessageI MsgSuccessChgPwd
      redirect . UserAdminR $ UserAdminEditR userId
    _ -> do
      can <- getCan
      adminLayout $ do
        setTitleI . MsgEditUser $ userEmail user
        $(widgetFile "user/edit")

-- | Request a user's password to be reset.
rqpassUserAdminEditR :: UserId -> Handler Html
rqpassUserAdminEditR userId = do
  user' <- runDB $ get404 userId
  token <- liftIO generateUUID
  let user =
        user'
        {userToken = Just token, userPassword = Nothing, userActive = False}
  _ <- runDB $ replace userId user
  _ <- sendAccountResetToken (Entity userId user)
  logMsg $ MsgLogUserRequestedPassword (userEmail user)
  setMessageI MsgPasswordResetTokenSend
  redirectUltDest . UserAdminR $ UserAdminEditR userId

-- | Deactivate a user.
deactivateUserAdminEditR :: UserId -> Handler Html
deactivateUserAdminEditR userId = do
  user' <- runDB $ get404 userId
  case userToken user' of
    Nothing -> do
      let user = user' {userActive = False}
      _ <- runDB $ replace userId user
      logMsg $ MsgLogUserDeactivated (userEmail user)
      setMessageI MsgUserDeactivated
    _ -> setMessageI MsgUserStillPending
  redirectUltDest . UserAdminR $ UserAdminEditR userId

-- | Activate a user.
activateUserAdminEditR :: UserId -> Handler Html
activateUserAdminEditR userId = do
  user' <- runDB $ get404 userId
  case userToken user' of
    Nothing -> do
      let user = user' {userActive = True}
      _ <- runDB $ replace userId user
      logMsg $ MsgLogUserActivated (userEmail user)
      setMessageI MsgUserActivated
    _ -> setMessageI MsgUserStillPending
  redirectUltDest . UserAdminR $ UserAdminEditR userId

-- | Delete an existing user.
-- TODO: Don\'t /actually/ delete the DB record!
deleteUserAdminEditR :: UserId -> Handler Html
deleteUserAdminEditR userId = do
  user' <- runDB $ get404 userId
  timeNow <- liftIO getCurrentTime
  uuid <- liftIO generateUUID
  let random = T.takeWhile (/= '-') uuid
  let user =
        user'
        { userEmail = random <> "@@@" <> userEmail user'
        , userToken = Nothing
        , userActive = False
        , userDeletedAt = Just timeNow
        }
  _ <- runDB $ replace userId user
  logMsg $ MsgLogUserDeleted (userEmail user)
  setMessageI MsgSuccessDelete
  redirectUltDest $ UserAdminR UserAdminIndexR

-- | Active an account by emailed activation link.
getUserAdminActivateR :: UserId -> Text -> Handler Html
getUserAdminActivateR userId token = do
  user <- runDB $ get404 userId
  case validateUserToken token user of
    Just True -> do
      (pwFormWidget, pwEnctype) <-
        generateFormPost $ userChangePasswordForm Nothing (Just MsgSave)
      authLayout $ do
        setTitle . toHtml $ userEmail user
        $(widgetFile "user/activate")
    Just False ->
      authLayout $ do
        setTitleI MsgTokenMismatch
        $(widgetFile "user/tokenmismatch")
    Nothing ->
      authLayout $ do
        setTitleI MsgAccountAlreadyActivated
        $(widgetFile "user/account-already-activated")

-- | Process a password change by password-reset-link email.
postUserAdminActivateR :: UserId -> Text -> Handler Html
postUserAdminActivateR userId token = do
  user <- runDB $ get404 userId
  case validateUserToken token user of
    Just True -> do
      opw <- lookupPostParam "original-pw"
      ((formResult, pwFormWidget), pwEnctype) <-
        runFormPost $ userChangePasswordForm opw (Just MsgSave)
      case formResult of
        FormSuccess f -> do
          _ <-
            runDB $
            update
              userId
              [ UserPassword =. Just (originalPassword f)
              , UserToken =. Nothing
              , UserActive =. True
              ]
          setMessageI MsgActivationSuccess
          setCreds False $ Creds "token-activation" (userEmail user) []
          redirect HomeR
        _ ->
          authLayout $ do
            setTitle . toHtml $ userEmail user
            $(widgetFile "user/activate")
    Just False ->
      authLayout $ do
        setTitleI MsgTokenMismatch
        $(widgetFile "user/tokenmismatch")
    Nothing ->
      authLayout $ do
        setTitleI MsgAccountAlreadyActivated
        $(widgetFile "user/account-already-activated")


knowledgeName :: Knowledge -> Text
knowledgeName a =
  toPathPiece (knowledgeUser a) <> " " <> toPathPiece (knowledgeLanguage a)

handleKnowledgeCrudR :: CrudRoute () Knowledge -> Handler Html
handleKnowledgeCrudR =
  handleCrud . flip simplerCrudToHandler KnowledgeCrudR $
  SimplerCrud
  { crudSimplerMsg = knowledgeMessages
  , crudSimplerDb = defaultCrudDb
  , crudSimplerForm = knowledgeForm
  , crudSimplerTable =
      encodeClickableTable $
      mconcat
        [ headed "Name" $ \(e, mr) ->
            case mr of
              Nothing -> toHtml . knowledgeName $ entityVal e
              Just r ->
                H.a (toHtml . knowledgeName $ entityVal e) H.! H.href (H.toValue r)
        ]
  }

knowledgeForm :: Maybe Knowledge -> UTCTime -> Form Knowledge
knowledgeForm m _ =
  renderBootstrap3 BootstrapBasicForm $
  Knowledge <$>
  areq (selectField optionsUsers) (bfs MsgUser) (knowledgeUser <$> m) <*>
  areq (selectField optionsLanguages) (bfs MsgLanguage) (knowledgeLanguage <$> m) <*
  bootstrapSubmit (BootstrapSubmit MsgSave " btn-success " [])
  where
    optionsLanguages = optionsPersistKey [] [] languageName

knowledgeMessages :: CrudMessages App Knowledge
knowledgeMessages = CrudMessages
  { crudMsgBack = SomeMessage MsgBack
  , crudMsgDelete = SomeMessage MsgDelete
  , crudMsgIndex = SomeMessage MsgKnowledgeAdminIndex
  , crudMsgNew = SomeMessage MsgKnowledgeAdminNew
  , crudMsgEdit = SomeMessage MsgKnowledgeAdminEdit
  , crudMsgNoEntities = SomeMessage MsgNoKnowledgeFound
  , crudMsgCreated = SomeMessage . MsgLogKnowledgeCreated . knowledgeName
  , crudMsgUpdated = SomeMessage . MsgLogKnowledgeUpdated . knowledgeName
  , crudMsgDeleted = SomeMessage . MsgLogKnowledgeDeleted . knowledgeName
  }
