{-# LANGUAGE PackageImports #-}
import "lobinuv-zavod" Application (getApplicationDev)
import Prelude (IO)
import Yesod.Default.Config2 (develMainHelper)

main :: IO ()
main = develMainHelper getApplicationDev
