{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE JavaScriptFFI              #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecursiveDo                #-}
import           Control.Monad              (join)
import           Control.Monad.IO.Class
import           Control.Monad.Trans
import qualified Data.Map.Strict            as Map
import           Data.Text                  (Text, unpack)
import           Data.Time
import           Reflex.Dom                 hiding (Reset)

import           Control.Concurrent         (forkIO)
import qualified GHCJS.DOM.HTMLMediaElement as Media
import           GHCJS.Types

-- Countdown timer models

data MS =
  MS Integer
  deriving (Eq, Show, Ord)

data Timer = Timer
  { _timerBegin   :: MS
  -- Nothing when the timer isn't running
  , _timerCurrent :: Maybe MS
  } deriving (Eq, Show)

isRunning :: Timer -> Bool
isRunning = isJust . _timerCurrent

stepTimer :: Timer -> Timer
stepTimer t =
  case _timerCurrent t of
    Just stepping -> t { _timerCurrent = Just (max (MS 0) (decr stepping)) }
    Nothing       -> t { _timerCurrent = Just (_timerBegin t) }

resetTimer :: Timer -> Timer
resetTimer t = t { _timerCurrent = Nothing }

decr :: MS -> MS
decr (MS n) = MS (n - 1)

data TimerEvent
  = Reset
  | Step deriving (Eq, Show)

applyEvent Reset = resetTimer
applyEvent Step  = stepTimer

-- Metronome model

newtype Rate = Rate Integer deriving (Eq, Show, Ord, Enum)

main = do
   player <- liftIO $ createAudio "243748__unfa__metronome-2khz-strong-pulse.flac"
   mainWidget $ do
    el "div" $ text "Musico Practico"
    el "ul" $ do

    --  el "li" $ display rateD
     rec bb <- el "li" $ buttonDyn (dynText buttonText) -- "test"
         isStarted <- toggle False bb
         let buttonText = fmap (\a -> if a then "Stop" else "Start") isStarted
     timerEvents <- countdownTimer (updated isStarted)
     timerD <- foldDyn applyEvent (Timer (MS 120) Nothing) (updated timerEvents)
     el "li" $ display timerD

     ------------

     (decrB, incrB) <- el "li" $ (,) <$> button "-" <*> button "+"
     metroBpmD <- foldDyn ($) (Rate 80) (leftmost [pred <$ decrB, succ <$ incrB])
     bb2 <- el "li" $ button "Start/Stop Metronome"
     let rateD = rateToPeriod <$> metroBpmD
     el "li" $ display metroBpmD

     startStopMetronomeE <- toggle False bb2
     x <- metronomey . updated $ (,) <$> (rateToPeriod <$> metroBpmD) <*> (startStopMetronomeE)
     let theJusts = flip fforMaybe id $ updated x
     addVoidAction ((Media.play player >> pure () ) <$ theJusts)
    --  l <- foldDyn (const succ) 0 theJusts
    --  el "li" $ display l
     pure ()

countdownTimer :: MonadWidget t m => Event t Bool -> m (Dynamic t TimerEvent)
countdownTimer =
  fmap join .
     widgetHold (pure (constDyn Reset)) . fmap (\x ->
       if not x
          then pure (constDyn Reset)
          else do e <- startTickingE 1
                  holdDyn Step (const Step <$> e)
          )

metronomey :: MonadWidget t m => Event t (NominalDiffTime, Bool) -> m (Dynamic t (Maybe ()))
metronomey =
  fmap join .
     widgetHold (pure (pure Nothing)) . fmap (\(rate, x) ->
       if not x
          then pure (pure Nothing)
          else do e <- startTickingE rate
                  -- We emit first because otherwise the first tick is delayed by `rate`
                  holdDyn (Just ()) (Just () <$ e))

startTickingE :: MonadWidget t m => NominalDiffTime -> m (Event t Integer)
startTickingE n = do
  started <- liftIO getCurrentTime
  l <- tickLossy n started
  pure (_tickInfo_n <$> l)

rateToPeriod :: Rate -> NominalDiffTime
rateToPeriod (Rate r) = (60/) . fromRational . toRational $ r

buttonDyn s = do
  (e, _) <- elAttr' "button" (Map.singleton "type" "button") s
  return $ domEvent Click e

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing  = False

-----------
foreign import javascript unsafe "new Audio($1)"
    createAudio :: JSString -> IO Media.HTMLMediaElement
