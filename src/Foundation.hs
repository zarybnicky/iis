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
import App.Module.Model (Module, ModuleId)
import App.Ticket.Model (Ticket, TicketId)
import App.Patch.Model (Patch)
import App.Language.Model (Language)
import App.Roles.Model (UserRole(..), EntityField(..))
import App.Roles.Types (RoleName(..))
import App.User.Model (User(..), UserId, EntityField(..), Unique(..))
import ClassyPrelude.Yesod hiding (modifyMVar)
import Cms.ActionLog.Class (CmsActionLog(..))
import Cms.Class (Cms(..))
import Cms.Crud.Route (CrudRoute(..))
import Cms.Mailer.Class
import Cms.Roles.Class (CmsRoles(..), Allow(..))
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

data MenuItem = MenuItem
    { menuItemLabel :: Text
    , menuItemRoute :: Route App
    , menuItemAccessCallback :: Bool
    }

data MenuTypes
    = NavbarLeft MenuItem
    | NavbarRight MenuItem

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

    muser <- maybeAuthPair
    mcurrentRoute <- getCurrentRoute

    (title, parents) <- breadcrumbs

    let navbarLeftMenuItems =
          [ MenuItem
            { menuItemLabel = "Home"
            , menuItemRoute = HomeR
            , menuItemAccessCallback = True
            }
          , MenuItem
            { menuItemLabel = "Profile"
            , menuItemRoute = ProfileR
            , menuItemAccessCallback = isJust muser
            }
          ]
    let navbarRightMenuItems =
          [ MenuItem
            { menuItemLabel = "Login"
            , menuItemRoute = AuthR LoginR
            , menuItemAccessCallback = isNothing muser
            }
          , MenuItem
            { menuItemLabel = "Logout"
            , menuItemRoute = AuthR LogoutR
            , menuItemAccessCallback = isJust muser
            }
          ]

    let navbarLeftFilteredMenuItems = [x | x <- navbarLeftMenuItems, menuItemAccessCallback x]
    let navbarRightFilteredMenuItems = [x | x <- navbarRightMenuItems, menuItemAccessCallback x]

    pc <- widgetToPageContent $ do
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
  breadcrumb ProfileR = return ("Profile", Just HomeR)
  breadcrumb  _ = return ("unknown", Nothing)

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
  logoutDest _ = HomeR
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
  authLayout = defaultAdminAuthLayout
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

instance CmsRoles App where
  type Roles App = RoleName

  actionAllowedFor RobotsR "GET" = AllowAll
  actionAllowedFor HomeR "GET" = AllowAll
  actionAllowedFor (AuthR _) _ = AllowAll
  actionAllowedFor _ _ = AllowRoles $ S.fromList [Admin]

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
    return $ S.member Admin roles

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
