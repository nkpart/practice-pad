{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE JavaScriptFFI              #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecursiveDo                #-}
{-# LANGUAGE TypeApplications           #-}
import           Control.Monad              (join)
import           Control.Monad.IO.Class
import qualified Data.Map.Strict            as Map
import           Data.Time
import qualified GHCJS.DOM.HTMLMediaElement as Media
import           GHCJS.Types
import           Reflex.Dom                 hiding (Pause, Reset)

-- Countdown timer models

{-
TODOS
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

-- BPM, for metronomes
newtype Rate = Rate Integer deriving (Eq, Show, Ord, Enum)

-------------------------
-- The Current Timer size

data LimitInputs t =  LimitInputs {
    _limitInsIncrease :: Event t Seconds
  , _limitInsDecrease :: Event t Seconds
  , _limitInsSet      :: Event t Seconds
}

data LimitOutputs t =  LimitOutputs {
    _limitOutsSeconds :: Dynamic t Seconds
}

createLimit :: MonadWidget t m => LimitInputs t -> Seconds -> m (LimitOutputs t)
createLimit input initial =
  fmap LimitOutputs .
  accum (&) initial . mergeWith (.) $ [
    (\x y -> y - x) <$> _limitInsDecrease input,
    (+) <$> _limitInsIncrease input,
    const <$> _limitInsSet input
  ]

---------------------------
-- A Timer and its controls

data TockerInputs t = TockerInputs {
   _tockerInputsLimit  :: Behavior t Seconds
  ,_tockerInputsStart  :: Event t ()
  ,_tockerInputsReset  :: Event t ()
  ,_tockerInputsPause  :: Event t ()
  ,_tockerInputsResume :: Event t ()
  }

data TockerOutputs t = TockerOutputs {
    _tockerOutputsElapsed  :: Dynamic t Seconds
  , _tockerOutputsAlarming :: Event t Bool
  }

createTocker
  :: MonadWidget t m
  => TockerInputs t -> m (TockerOutputs t)
createTocker inputs = do
  rec elapsedD <-
        fmap join .
        widgetHold (pure (pure 0)) . leftmost $
          [ pure (pure 0) <$ _tockerInputsReset inputs
          , secondsFrom 0 <$ _tockerInputsStart inputs
          , (\(v, ()) -> pure (constDyn v)) <$>
            attach (current elapsedD) (_tockerInputsPause inputs)
          , (\(v, ()) -> secondsFrom v) <$>
            attach (current elapsedD) (_tockerInputsResume inputs)
          ]

  let alarmingE =
        (attachWith
           (\limit step -> step >= limit)
           (_tockerInputsLimit inputs)
           (updated elapsedD))
  pure $ TockerOutputs elapsedD alarmingE

secondsFrom
  :: MonadWidget t m
  => Seconds -> m (Dynamic t Seconds)
secondsFrom initial = do
  e <- startTickingE 1
  (fmap ((+ initial) . Seconds) <$> count e)

--------------------------------------------
--- A Metronome rate

data RateInputs t =  RateInputs {
    _rateInputsIncrease :: Event t ()
  , _rateInputsDecrease :: Event t ()
  }

data RateOutputs t =  RateOutputs {
    _rateOutputs :: Dynamic t Rate
}

createRate :: MonadWidget t m => RateInputs t -> Rate -> m (RateOutputs t)
createRate inputs initial =
  let decrE = _rateInputsDecrease inputs
      incrE = _rateInputsIncrease inputs
   in
      RateOutputs <$> (accum (&) initial . mergeWith (.) $ [pred <$ decrE, succ <$ incrE])

-------------------------
-- The Metronome ticks

data MetronomeInputs t = MetronomeInputs {
    _metronomeInputsRate  :: Dynamic t Rate
  , _metronomeInputsStart :: Event t ()
  , _metronomeInputsStop  :: Event t ()
}

data MetronomeOutputs t = MetronomeOutputs {
    _metronomeOutputsBeep  :: Event t ()
  }

createMetronome :: MonadWidget t m => MetronomeInputs t -> m (MetronomeOutputs t)
createMetronome inputs =
  do startedOrNot <- accum (\_ y -> y) False . leftmost $ [
          True <$ _metronomeInputsStart inputs
        , False <$ _metronomeInputsStop inputs
      ]

     let timeD = rateToPeriod <$> (_metronomeInputsRate inputs)
     fmap (MetronomeOutputs . fmapMaybe id . updated . join) .
       widgetHold (pure (pure Nothing)) . fmap (\(rate, x) ->
        if not x
           then pure (pure Nothing)
           else do e <- startTickingE rate
                   -- We emit first because otherwise the first tick is delayed by `rate`
                   holdDyn (Just ()) (Just () <$ e)) $ updated (zipDynWith (,) timeD startedOrNot)

-------------------------------------
-- GOOOOOOOOOOO!

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap

main = do
   player <- liftIO $ createAudio "243748__unfa__metronome-2khz-strong-pulse.flac"
   let initialMetronomeRate = Rate 80
   mainWidget $ do
    el "div" $ text "Practice Pad"
    el "ul" $ do
      (incrL, decrL) <- el "li" $
        (,) <$> (const 5 <$$> button "+") <*> (const 5 <$$> button "-")

      changes <-
        el "li" $ do
          _1min <- (const 60) <$$> button "1:00"
          _2min <- (const 120) <$$> button "2:00"
          _5min <- (const 300) <$$> button "5:00"
          pure . leftmost $ [_1min, _2min, _5min]

      incrBig <-
        el "li" $ do
          _1min <- (const 60) <$$> button "+1:00"
          _2min <- (const 120) <$$> button "+2:00"
          pure . leftmost $ [_1min, _2min]

      tickerLimit <- createLimit (LimitInputs (leftmost [incrL, incrBig]) decrL changes) 10

      el "li" $ display (_limitOutsSeconds tickerLimit)

      tockerInputs <- el "li" $ do
        startE <- button "start"
        resetE <- button "reset"
        pauseE <- button "pause"
        resume <- button "resume"
        let limitB = current (_limitOutsSeconds tickerLimit)
        pure $ TockerInputs limitB startE resetE pauseE resume

      tocker <- createTocker tockerInputs
      el "li" $ display $ _tockerOutputsElapsed tocker
      (el "li" . display) =<< holdDyn False (_tockerOutputsAlarming tocker)
      pure ()

    el "ul" $ do
     ------------
     rateInputs <- el "li" $ RateInputs <$> button "-" <*> button "+"
     RateOutputs rateD <- createRate rateInputs initialMetronomeRate
     el "li" $ display rateD
     bb2 <- el "li" $ button "Start/Stop Metronome"
     startStopMetronomeE <- toggle False bb2

     let starts = () <$ ffilter id (updated startStopMetronomeE)
         stops = () <$ ffilter not (updated startStopMetronomeE)
     MetronomeOutputs x <- createMetronome $ MetronomeInputs rateD starts stops
     addVoidAction ((Media.play player >> pure () ) <$ x)
     pure ()

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

-----------
foreign import javascript unsafe "new Audio($1)"
    createAudio :: JSString -> IO Media.HTMLMediaElement
