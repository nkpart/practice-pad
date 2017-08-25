import Language.Javascript.JSaddle            (JSM)
import Language.Javascript.JSaddle.Run        (syncPoint)
import Language.Javascript.JSaddle.WebSockets (debugWrapper, jsaddleWithAppOr)
import Network.Wai                            (Application)
import Network.Wai.Handler.Warp               (defaultSettings, run, runSettings, setPort, setTimeout)
import Network.WebSockets                     (defaultConnectionOptions)
import Data.Monoid
import Data.Function

-- | A @main@ for doing development.
devMain :: Application -> JSM () -> Int -> IO ()
devMain backend frontend port = do
  putStrLn $ "Running dev server on localhost:" <> show port

  app <- jsaddleWithAppOr
    defaultConnectionOptions
    (frontend >> syncPoint)
    backend

  runSettings (defaultSettings & setTimeout 3600 & setPort port) app

-- | A version of @devMain@ that can be used with @ghcid --test@ to get an auto-reloading server.
devMainAutoReload :: Application -> JSM () -> Int -> IO ()
devMainAutoReload backend frontend port =
  debugWrapper $ \refreshMiddleware registerContext ->
    devMain (refreshMiddleware backend) (registerContext >> frontend) port
