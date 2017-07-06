{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE JavaScriptFFI              #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecursiveDo                #-}
{-# LANGUAGE TypeApplications           #-}
import           Control.Monad              (join)
import           Control.Monad.IO.Class
import           Control.Monad.Trans
import qualified Data.Map.Strict            as Map
import           Data.Text                  (Text, unpack)
import           Data.Time
import           Reflex.Dom                 hiding (Pause, Reset)

import           Control.Concurrent         (forkIO)
import           Control.Lens
import qualified GHCJS.DOM.HTMLMediaElement as Media
import           GHCJS.Types

-- Countdown timer models

{-
TODOS
* Alarm state for timers
* Build out a boxy UI
* Add some presets for metronome and timers
* keep a log of completed alarms
* attach a note to completed alarms
  - this is a practice record
* attach badges reflecting achievement to either the whole practice session or a timed unit
-}

newtype Seconds =
  Seconds Integer
  deriving (Eq, Show, Ord, Enum, Num)

data LimitIns t =  LimitIns {
    _limitInsIncrease :: Event t Seconds
  , _limitInsDecrease :: Event t Seconds
}

data LimitOuts t =  LimitOuts {
    _limitOutsSeconds :: Dynamic t Seconds
}

createLimit :: MonadWidget t m => LimitIns t -> Seconds -> m (LimitOuts t)
createLimit input initial =
  fmap LimitOuts .
  accum (&) initial . mergeWith (.) $ [
    (-) <$> _limitInsDecrease input,
    (+) <$> _limitInsIncrease input
  ]

data TockerInputs t = TockerInputs {
   _tockerInputsLimit :: Behavior t Int
  ,_tockerInputsStart :: Event t ()
  ,_tockerInputsReset :: Event t ()
  ,_tockerInputsPause :: Event t ()
  }

data TockerOutputs t = TockerOutputs {
    timerElapsed  :: Event t Seconds
  , timerAlarming :: Event t Bool
  }

data Timer = Timer
  { _timerBegin   :: Seconds
  -- Nothing when the timer isn't running
  , _timerCurrent :: Maybe Seconds
  , _timerPaused  :: Bool
  } deriving (Eq, Show)

timerBegin :: Functor f => (Seconds -> f Seconds) -> Timer -> f Timer
timerBegin f t = (\x -> t { _timerBegin = x }) <$> f (_timerBegin t)

timerPaused :: Functor f => (Bool -> f Bool) -> Timer -> f Timer
timerPaused f t = (\x -> t { _timerPaused = x }) <$> f (_timerPaused t)

isRunning :: Timer -> Bool
isRunning = isJust . _timerCurrent

stepTimer :: Timer -> Timer
stepTimer t | _timerPaused t = t
            | otherwise =
  case _timerCurrent t of
    Just stepping -> t { _timerCurrent = Just (succ stepping) }
    Nothing       -> t { _timerCurrent = Just 0 }

resetTimer :: Timer -> Timer
resetTimer t = t { _timerCurrent = Nothing }

-- type TimerEvent = Timer -> Timer

newtype Rate = Rate Integer deriving (Eq, Show, Ord, Enum)

smallMinute = 5
addMinute = timerBegin +~ smallMinute
subtractMinute r = if _timerBegin r < smallMinute  then r else r & timerBegin -~ smallMinute

addTime t = timerBegin +~ t
subtractTime t = timerBegin -~ t

data TimerState = Ready | Ticking | Paused | Alarming

data TimerEvent = Reset | Tick | Pause

applyState :: TimerEvent -> Timer -> Timer
applyState Reset = resetTimer
applyState Tick  = stepTimer
applyState Pause = timerPaused %~ not

main = do
   player <- liftIO $ createAudio "243748__unfa__metronome-2khz-strong-pulse.flac"
   let initialTimer = Timer 10 Nothing False
       initialMetronomeRate = Rate 80
   mainWidget $ do
    el "div" $ text "Practice Pad"
    el "ul" $ do
     (decrTE, incrTE) <- el "li" $ (,) <$> button "-" <*> button "+"

     rec bb <- el "li" $ buttonDyn (dynText buttonText)
         isStarted <- accum (&) False . mergeWith (.) $ [
           not <$ bb
           -- Decided that changing the timer by +/- a minute shouldn't affect the alarm
           --  const False <$ decrTE,
           --  const False <$ incrTE
           ]
         let buttonText = (\a -> if a then "Stop" else "Start") <$> isStarted

         -- Timer should be in one of these states, with one of these action sets
         -- Ready -> Start
         -- Ticking -> Pause, Finish
         -- Paused -> Resume, Finish
         -- Alarming -> Finish

     -- stream of timer events that starts and stops based on changes to isStarted
     -- :: Event t TimerEvent
     timerEvents <- countdownTimer (updated isStarted)

     -- drive the current timer with the events out of timerEvents
     -- :: Dynamic t Timer
     activeTimerD <- accum (&) initialTimer . mergeWith (.) $ [
          applyState <$> updated timerEvents
        , addMinute <$ incrTE
        , subtractMinute <$ decrTE
        ]

     el "li" $ display activeTimerD

    el "ul" $ do
     ------------
     (decrB, incrB) <- el "li" $ (,) <$> button "-" <*> button "+"
     metroBpmD <- accum (&) initialMetronomeRate . mergeWith (.) $ [pred <$ decrB, succ <$ incrB]
     bb2 <- el "li" $ button "Start/Stop Metronome"
     let rateD = rateToPeriod <$> metroBpmD
     el "li" $ display metroBpmD

     startStopMetronomeE <- toggle False bb2
     x <- metronomey . updated $ (,) <$> (rateToPeriod <$> metroBpmD) <*> startStopMetronomeE
     let theJusts = fmapMaybe id $ updated x
     addVoidAction ((Media.play player >> pure () ) <$ theJusts)
     pure ()

countdownTimer :: MonadWidget t m => Event t Bool -> m (Dynamic t TimerEvent)
countdownTimer =
  fmap join .
     widgetHold (pure (constDyn Reset)) . fmap (\x ->
       if not x
          then pure (constDyn Reset)
          else do e <- startTickingE 1
                  holdDyn Tick (const Tick <$> e)
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
