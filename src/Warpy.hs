{-# LANGUAGE CPP #-}
module Warpy (
  -- * Running JSM over WebSockets
    runAndServe
#ifndef ghcjs_HOST_OS
  , module Language.Javascript.JSaddle.WebSockets
#endif
) where

#ifndef ghcjs_HOST_OS
import           Control.Applicative
import           Network.Wai.Handler.Warp               (defaultSettings,
                                                         runSettings, setPort,
                                                         setTimeout)
import           Network.WebSockets                     (defaultConnectionOptions)

import           Data.Maybe                             (fromMaybe)
import           Language.Javascript.JSaddle.Run        (syncPoint)
import           Language.Javascript.JSaddle.Types      (JSM)
import           Language.Javascript.JSaddle.WebSockets
import           Network.Wai.Application.Static
#endif

-- | Run the given 'JSM' action as the main entry point.  Either directly
--   in GHCJS or as a Warp server on the given port on GHC.
#ifdef ghcjs_HOST_OS
runAndServe :: Int -> FilePath -> IO () -> IO ()
runAndServe _port _thingo = id
#else
runAndServe :: Int -> FilePath -> JSM () -> IO ()
runAndServe port staticDir f =
  do reflexApp <- jsaddleOr defaultConnectionOptions (f >> syncPoint) (lulWatIsThis staticDir)
     runSettings (setPort port (setTimeout 3600 defaultSettings)) reflexApp

lulWatIsThis staticDir req sendResponse =
    fromMaybe
        (staticApp (defaultFileServerSettings staticDir) req sendResponse)
        (jsaddleAppPartial req sendResponse)

#endif
