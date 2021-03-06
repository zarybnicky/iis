{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}

module Foundation where

import App.ActionLog.Model
import App.Bug.Model
import App.Module.Model
import App.Ticket.Model
import App.Patch.Model
import App.Language.Model
import App.Roles.Model (UserRole(..), EntityField(..))
import App.Roles.Types (RoleName(..))
import App.User.Model
import ClassyPrelude.Yesod hiding (modifyMVar)
import Cms.ActionLog.Class (CmsActionLog(..))
import Cms.Class (Cms(..))
import Cms.Crud.Route (CrudRoute(..))
import Cms.Mailer.Class
import Cms.Roles.Class (CmsRoles(..), Allow(..), getCan)
import Control.Monad.Trans.Writer.Lazy (execWriter, tell)
import qualified Data.CaseInsensitive as CI
import Data.Maybe (isJust)
import Data.Ord (comparing)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (getCurrentTime)
import Data.Time.Format.Human
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import I18n
import Message
import Network.Mail.SMTP (sendMailWithLogin)
import qualified Network.Wai as W
import Settings
import Settings.StaticFiles
import Text.Hamlet (hamletFile)
import Text.Jasmine (minifym)
import Yesod.Auth
import Yesod.Auth.HashDB (authHashDB)
import Yesod.Auth.Message (AuthMessage(InvalidLogin))
import Yesod.Core.Types (Logger)
import qualified Yesod.Core.Unsafe as Unsafe
import Yesod.Default.Util (addStaticContentExternal)

data App = App
  { appSettings :: AppSettings
  , appStatic :: Static -- ^ Settings for static file serving.
  , appConnPool :: ConnectionPool -- ^ Database connection pool.
  , appHttpManager :: Manager
  , appLogger :: Logger
  }

type Unit = ()

mkYesodData "App" $(parseRoutesFile "config/routes")

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

instance Yesod App where
  approot = ApprootRequest $ \app req ->
    fromMaybe (getApprootText guessApproot app req) (appRoot $ appSettings app)

  makeSessionBackend _ = Just <$> defaultClientSessionBackend
    120    -- timeout in minutes
    "config/client_session_key.aes"

  maximumContentLength _ _ = Just (10 * 1024 * 1024)

  yesodMiddleware = defaultYesodMiddleware
  defaultLayout widget = do
    master <- getYesod
    mmsg <- getMessage
    can <- getCan

    muser <- maybeAuthPair
    mcurrentRoute <- getCurrentRoute

    (title, parents) <- breadcrumbs

    let mkMenu n r = when (isJust $ can r "GET") $ tell [(n, r)]
    let navbarLeft :: [(Text, Route App)] = execWriter $ do
          tell [("Home", HomeR)]
          mkMenu "Tickets" MyTicketsR
          mkMenu "Patches" MyPatchesR
          mkMenu "Bugs" MyBugsR
          mkMenu "Patch grid" PatchGridR
    let navbarRight = execWriter $ do
          mkMenu "Mgmt" EntitiesR
          when (isNothing muser) $ tell [("Register", RegistrationR)]
          when (isNothing muser) $ tell [("Login", (AuthR LoginR))]
          case muser of
            Nothing -> return ()
            Just (uid, u) -> do
              tell [(userFirstName u <> " " <> userLastName u, UserAdminR (UserAdminEditR uid))]
              tell [("Logout", AuthR LogoutR)]

    pc <- widgetToPageContent $ do
      addStylesheet $ StaticR fontawesome_font_awesome_min_css
      addStylesheet $ StaticR bootstrap_bootstrap_min_css
      addScript $ StaticR bootstrap_bootstrap_min_js
      $(widgetFile "default-layout")
    withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

  authRoute _ = Just $ AuthR LoginR

  isAuthorized (StaticR _)                   _ = return Authorized
  isAuthorized theRoute                      _ = do
    method' <- W.requestMethod <$> waiRequest
    y <- getYesod
    murs <- mapM getUserRoles =<< maybeAuthId
    return $ isAuthorizedTo y murs $ actionAllowedFor theRoute method'

  addStaticContent ext mime content = do
    master <- getYesod
    addStaticContentExternal
      minifym
      (("autogen-" ++) . base64md5)
      (appStaticDir $ appSettings master)
      (StaticR . flip StaticRoute [])
      ext
      mime
      content

  shouldLog app _source level =
    appShouldLogAll (appSettings app)
      || level == LevelWarn
      || level == LevelError

  makeLogger = return . appLogger

instance YesodBreadcrumbs App where
  breadcrumb HomeR = return ("Home", Nothing)
  breadcrumb (AuthR _) = return ("Login", Just HomeR)
  breadcrumb RegistrationR = return ("Registration", Just HomeR)
  breadcrumb (ModuleOverviewR _) = return ("Module overview", Just HomeR)
  breadcrumb (UserAdminR UserAdminIndexR) = return ("User management", Just HomeR)
  breadcrumb (UserAdminR (UserAdminEditR _)) = return ("Edit", Just (UserAdminR UserAdminIndexR))
  breadcrumb (UserAdminR UserAdminNewR) = return ("Add", Just (UserAdminR UserAdminIndexR))
  breadcrumb EntitiesR = return ("Entity management", Just HomeR)
  breadcrumb (ModuleCrudR _) = return ("Module administration", Just EntitiesR)
  breadcrumb (PatchCrudR _) = return ("Patch administration", Just EntitiesR)
  breadcrumb (PatchCommentCrudR _) = return ("Patch comment administration", Just EntitiesR)
  breadcrumb (LanguageCrudR _) = return ("Language administration", Just EntitiesR)
  breadcrumb (TicketCrudR _) = return ("Ticket administration", Just EntitiesR)
  breadcrumb (BugCrudR _) = return ("Bug administration", Just EntitiesR)
  breadcrumb (AnnouncesCrudR _) = return ("Ticket/bug link administration", Just EntitiesR)
  breadcrumb PatchGridR = return ("Patch grid", Just HomeR)
  breadcrumb MyPatchesR = return ("Patches", Just HomeR)
  breadcrumb AddPatchR = return ("Add patch", Just MyPatchesR)
  breadcrumb (EditPatchR _) = return ("Edit patch", Just MyPatchesR)
  breadcrumb (ViewPatchR _) = return ("View patch", Just MyPatchesR)
  breadcrumb MyTicketsR = return ("Tickets", Just HomeR)
  breadcrumb AddTicketR = return ("Add ticket", Just MyTicketsR)
  breadcrumb (EditTicketR _) = return ("Edit ticket", Just MyTicketsR)
  breadcrumb (ViewTicketR _) = return ("View ticket", Just MyTicketsR)
  breadcrumb MyBugsR = return ("Bugs", Just HomeR)
  breadcrumb AddBugR = return ("Add bug", Just MyBugsR)
  breadcrumb (EditBugR _) = return ("Edit bug", Just MyBugsR)
  breadcrumb (ViewBugR _) = return ("View ticket", Just MyBugsR)
  breadcrumb  _ = return ("unknown", Nothing)

instance CmsRoles App where
  type Roles App = RoleName

  actionAllowedFor RobotsR _ = AllowAll
  actionAllowedFor (StaticR _) _ = AllowAll
  actionAllowedFor HomeR _ = AllowAll
  actionAllowedFor (AuthR _) _ = AllowAll
  actionAllowedFor RegistrationR _ = AllowAll
  actionAllowedFor EntitiesR _ = AllowRoles $ S.fromList [RoleProgrammer, RoleAdmin]
  actionAllowedFor (ModuleOverviewR _) _ = AllowAuthenticated
  actionAllowedFor (UserAdminActivateR _ _) _ = AllowAuthenticated
  actionAllowedFor (UserAdminR UserAdminNewR) _ = AllowRoles $ S.fromList [RoleAdmin]
  actionAllowedFor (UserAdminR UserAdminIndexR) _ = AllowRoles $ S.fromList [RoleAdmin]
  actionAllowedFor (UserAdminR (UserAdminEditR _)) _ = AllowAuthenticated
  actionAllowedFor (LanguageCrudR _) _ = AllowRoles $ S.fromList [RoleAdmin]
  actionAllowedFor (ModuleCrudR _) _ = AllowRoles $ S.fromList [RoleAdmin]
  actionAllowedFor (TicketCrudR _) _ = AllowRoles $ S.fromList [RoleAdmin, RoleProgrammer]
  actionAllowedFor (BugCrudR _) _ = AllowRoles $ S.fromList [RoleAdmin, RoleProgrammer]
  actionAllowedFor (AnnouncesCrudR _) _ = AllowRoles $ S.fromList [RoleAdmin, RoleProgrammer]
  actionAllowedFor (PatchCrudR _) _ = AllowRoles $ S.fromList [RoleAdmin, RoleProgrammer]
  actionAllowedFor (PatchCommentCrudR _) _ = AllowRoles $ S.fromList [RoleAdmin, RoleProgrammer]
  actionAllowedFor MyTicketsR _ = AllowAuthenticated
  actionAllowedFor AddTicketR _ = AllowAuthenticated
  actionAllowedFor (EditTicketR _) _ = AllowAuthenticated
  actionAllowedFor (DeleteTicketR _) _ = AllowAuthenticated
  actionAllowedFor (ViewTicketR _) _ = AllowAuthenticated
  actionAllowedFor MyPatchesR _ = AllowAuthenticated
  actionAllowedFor AddPatchR _ = AllowAuthenticated
  actionAllowedFor (EditPatchR _) _ = AllowAuthenticated
  actionAllowedFor (DeletePatchR _) _ = AllowAuthenticated
  actionAllowedFor (ViewPatchR _) _ = AllowAuthenticated
  actionAllowedFor (CommentPatchR _ _) _ = AllowAuthenticated
  actionAllowedFor PatchGridR _ = AllowRoles $ S.fromList [RoleAdmin, RoleProgrammer]
  actionAllowedFor (PatchApproveR _ _) _ = AllowRoles $ S.fromList [RoleAdmin, RoleProgrammer]
  actionAllowedFor (PatchDeployR _ _) _ = AllowRoles $ S.fromList [RoleAdmin]
  actionAllowedFor MyBugsR _ = AllowAuthenticated
  actionAllowedFor AddBugR _ = AllowAuthenticated
  actionAllowedFor (EditBugR _) _ = AllowAuthenticated
  actionAllowedFor (DeleteBugR _) _ = AllowAuthenticated
  actionAllowedFor (ViewBugR _) _ = AllowAuthenticated

  -- cache user roles to reduce the amount of DB calls
  getUserRoles userId =
    cachedBy cacheKey . fmap toRoleSet . runDB $
    selectList [UserRoleUserId ==. userId] []
    where
      cacheKey = encodeUtf8 $ toPathPiece userId
      toRoleSet = S.fromList . map (userRoleRoleName . entityVal)

  setUserRoles userId rs =
    runDB $ do
      deleteWhere [UserRoleUserId ==. userId]
      mapM_ (insert_ . UserRole userId) $ S.toList rs

  mayAssignRoles = do
    authId <- requireAuthId
    roles <- getUserRoles authId
    return $ S.member RoleAdmin roles

instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage

instance RenderMessage App AppMessage where
  renderMessage _ = renderAppMessage

-- How to run database actions.
instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend
  runDB action = getYesod >>= runSqlPool action . appConnPool

instance YesodPersistRunner App where
  getDBRunner = defaultGetDBRunner appConnPool

instance YesodAuth App where
  type AuthId App = UserId

  loginDest _ = HomeR
  logoutDest _ = AuthR LoginR
  redirectToReferer _ = True

  authenticate creds = runDB $ do
    user <- getBy $ UniqueAuth (credsIdent creds) True
    case user of
      Just (Entity uid _) -> do
        timeNow <- liftIO getCurrentTime
        _ <- update uid [UserLastLogin =. Just timeNow]
        return $ Authenticated uid
      Nothing -> return $ UserError InvalidLogin

  maybeAuthId = do
    mauthId <- defaultMaybeAuthId
    case mauthId of
      Nothing -> return Nothing
      Just authId -> do
        user <- runDB $ get404 authId
        return $
          if userActive user
            then Just authId
            else Nothing

  authPlugins _ = [authHashDB (Just . flip UniqueAuth True)]
  authLayout = defaultLayout
  authHttpManager = getHttpManager

instance YesodAuthPersist App

instance HasHttpManager App where
  getHttpManager = appHttpManager

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger


instance Cms App where
  renderLanguages _ = ["en", "cs"]
  adminLayout = defaultLayout
  adminMenu = []

instance CmsActionLog App where
  data Log App = ActionLog
  logMsg = logMsgImpl

instance CmsMailer App where
  sendMail =
    liftIO . sendMailWithLogin "mail.zarybnicky.com" "dummy@zarybnicky.com" "dummy"

defaultAdminAuthLayout :: Widget -> Handler Html
defaultAdminAuthLayout widget = do
    mmsg      <- getMessage
    logoRowId <- newIdent

    pc <- widgetToPageContent $ do
      $(widgetFile "admin-auth-layout")
    withUrlRenderer $(hamletFile "templates/admin-auth-layout-wrapper.hamlet")

-- | Extension for bootstrap (give a name to input field).
withName :: Text -> FieldSettings site -> FieldSettings site
withName name fs = fs { fsName = Just name }

routeBestMatch :: RenderRoute master
                  => Maybe (Route master)
                  -> [Route master]
                  -> Maybe (Route master)
routeBestMatch (Just cr) rs = snd <$> find cmp orrs
    where
        (cparts, _) = renderRoute cr
        rrs = map ((fst . renderRoute) &&& id) rs
        orrs = sortBy (flip (comparing (length . fst))) rrs
        cmp (route', _) = route' == take (length route') cparts
routeBestMatch _ _ = Nothing

getHumanTimeLocale :: Handler HumanTimeLocale
getHumanTimeLocale = do
    langs <- languages
    y <- getYesod
    return $ selectHumanTimeLocale langs (T.unpack . renderMessage y langs)
