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

module App.User.Handler
  ( getUserAdminIndexR
  , getUserAdminNewR
  , postUserAdminNewR
  , getUserAdminEditR
  , patchUserAdminEditR
  , deleteUserAdminEditR
  , chpassUserAdminEditR
  , rqpassUserAdminEditR
  , deactivateUserAdminEditR
  , activateUserAdminEditR
  , getUserAdminActivateR
  , postUserAdminActivateR
  , getProfileR
  , postProfileR
  , getRegistrationR
  , postRegistrationR
  , getAccountsR
  , postAccountsR
  ) where

import App.User.Model (UserId, User(..), EntityField(..), sendMailToUser)
import ClassyPrelude.Yesod hiding (Request)
import Cms.Class (adminLayout)
import Cms.ActionLog.Class (logMsg)
import Cms.Roles.Class (Roles, getCan, getUserRoles, setUserRoles, mayAssignRoles, defaultRoles)
import Control.Arrow ((&&&))
import Data.Maybe (fromJust, fromMaybe, isJust)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time.Format.Human
import Foundation
import Message (AppMessage(..))
import Settings
import Text.Hamlet (hamletFile)
import Yesod.Auth (Creds(..), Route(LoginR), requireAuthId, setCreds, authLayout)
import Yesod.Auth.Email (saltPass)
import Yesod.Auth.HashDB (setPassword)
import Yesod.Form.Bootstrap3


getProfileR :: Handler Html
getProfileR = do
  uid <- requireAuthId
  _user <- runDB $ get404 uid
  defaultLayout [whamlet|<h1>text|]

postProfileR :: Handler Html
postProfileR = getProfileR

getAccountsR :: Handler Html
getAccountsR = do
  users <- runDB $ selectList [] []
  defaultLayout $ do
    setTitle "Accounts"
    [whamlet|
      <h1>Accounts
        <table>
          <thead>
            <tr>
              <th>Email
              <th>First Name
              <th>Last Name
              <th>Birth Number
              <th>Address
              <th>City
              <th>Postal Code
          <tbody>
            $forall Entity _ userData <- users
              <tr>
                <td>#{userEmail userData}
                <td>#{userFirstName userData}
                <td>#{userLastName userData}
                <td>#{userBirthNumber userData}
                <td>#{fromMaybe "" (userAddress userData)}
                <td>#{fromMaybe "" (userCity userData)}
                <td>#{maybe "" show (userPostalCode userData)}
    |]

postAccountsR :: Handler Html
postAccountsR = getAccountsR

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
  -> S.Set (Roles App)
  -> Maybe AppMessage
  -> Html
  -> MForm Handler (FormResult (User, [Roles App]), Widget)
accountSettingsForm user roles mlabel extra = do
    maRoles <- lift mayAssignRoles
    -- User fields
    (fnameRes, fnameView) <- mreq textField (bfs MsgFirstName) (Just $ userFirstName user)
    (lnameRes, lnameView) <- mreq textField (bfs MsgLastName) (Just $ userLastName user)
    (emailRes, emailView) <- mreq emailField (bfs MsgEmailAddress) (Just $ userEmail user)
    (birthRes, birthView) <- mreq intField (bfs MsgBirthNumber) (Just $ userBirthNumber user)
    (addrRes, addrView) <- mopt textField (bfs MsgAddress) (Just $ userAddress user)
    (cityRes, cityView) <- mopt textField (bfs MsgCity) (Just $ userCity user)
    (postRes, postView) <- mopt intField (bfs MsgPostalCode) (Just $ userPostalCode user)

    -- Roles field
    (rolesRes, mrolesView) <- if maRoles
        then do
            (rolesRes', rolesView) <- mreq (checkboxesField roleList)
                                           "Not used"
                                           (Just $ S.toList roles)
            return (rolesRes', Just rolesView)
        else return (FormSuccess $ S.toList roles, Nothing)

    let userRes = (\a b c d e f g -> user { userFirstName = a
                                          , userLastName = b
                                          , userEmail = c
                                          , userBirthNumber = d
                                          , userAddress = e
                                          , userCity = f
                                          , userPostalCode = g
                                          })
                  <$> fnameRes <*> lnameRes <*> emailRes <*> birthRes
                  <*> addrRes <*> cityRes <*> postRes
        formRes = (,) <$> userRes <*> rolesRes
        widget = $(widgetFile "user/settings-form")

    return (formRes, widget)
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
  (users' :: [Entity User]) <-
    runDB $ selectList [UserDeletedAt ==. Nothing] []
  users <-
    mapM
    (\user -> do
        ur <- getUserRoles $ entityKey user
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
    generateFormPost $ accountSettingsForm eu drs (Just MsgCreate)
  adminLayout $ do
    setTitleI MsgNewUser
    $(widgetFile "user/new")

-- | Create a new user, handle a posted form.
postUserAdminNewR :: Handler Html
postUserAdminNewR = do
  eu <- liftIO emptyUser
  drs <- defaultRoles
  ((formResult, formWidget), enctype) <-
    runFormPost $ accountSettingsForm eu drs (Just MsgCreate)
  case formResult of
    FormSuccess (user, roles) -> do
      userId <- runDB $ insert user
      setUserRoles userId (S.fromList roles)
      sendAccountActivationToken (Entity userId user)
      logMsg $ MsgLogUserCreated (userEmail user)
      setMessageI MsgSuccessCreate
      redirectUltDest $ UserAdminR UserAdminIndexR
    _ ->
      do
        can <- getCan
        adminLayout $ do
          setTitleI MsgNewUser
          $(widgetFile "user/new")

-- | Show the forms to edit an existing user.
getUserAdminEditR :: UserId -> Handler Html
getUserAdminEditR userId = do
  timeNow <- liftIO getCurrentTime
  do
    authId <- requireAuthId
    can <- getCan
    user <- runDB $ get404 userId
    urs <- getUserRoles userId
    hrtLocale <- getHumanTimeLocale
    (formWidget, enctype) <-
      generateFormPost $
      accountSettingsForm user urs (Just MsgSave) -- user form
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
  (pwFormWidget, pwEnctype) <-
    generateFormPost $ userChangePasswordForm Nothing (Just MsgChange)
  ((formResult, formWidget), enctype) <-
    runFormPost $ accountSettingsForm user urs (Just MsgSave)
  case formResult of
    FormSuccess (updatedUser, updatedRoles) -> do
      do
        _ <- runDB $ replace userId updatedUser
        setUserRoles userId (S.fromList updatedRoles)
        logMsg $ MsgLogUserUpdated (userEmail updatedUser)
        setMessageI MsgSuccessReplace
      redirect . UserAdminR $ UserAdminEditR userId
    _ ->
      do
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
  if userId == authId
    then do
      (user, timeNow, hrtLocale, urs) <- updateHelper userId
      (formWidget, enctype) <-
        generateFormPost $ accountSettingsForm user urs (Just MsgSave)
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
        _ ->
          do
            can <- getCan
            adminLayout $ do
              setTitleI . MsgEditUser $ userEmail user
              $(widgetFile "user/edit")
    else error "Can't change this uses password"

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
  do
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
