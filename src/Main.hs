{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE JavaScriptFFI              #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE RecursiveDo                #-}

module Main (main) where

import           Control.Lens
import           Control.Monad               (join, void)
import           Control.Monad.IO.Class
import qualified Data.Map.Strict             as Map
import           Data.Monoid
import           Data.Text                   (pack)
import           Data.Time
import           Language.Javascript.JSaddle
import           Reflex.Dom.Core             hiding (Pause, Reset)
import           Warpy                       (runAndServe)

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
newtype Rate = Rate Integer deriving (Eq, Show, Ord, Enum, Num)

----------------------------

data ValControls t v =  ValControls {
    _valControlsIncrease :: Event t v
  , _valControlsDecrease :: Event t v
  , _valControlsSet      :: Event t v
  }

---

data Val t v =  Val {
    _val :: Dynamic t v
}

createVal :: (Ord v, MonadWidget t m, Num v) => ValControls t v -> v -> m (Val t v)
createVal input initial =
  let lowest = 0
  in
  fmap Val .
  accum (&) initial . mergeWith (.) $ [
    (\x y -> max lowest (y - x)) <$> _valControlsDecrease input,
    (+) <$> _valControlsIncrease input,
    const <$> _valControlsSet input
  ]

---------------------------
-- A Timer and its controls

data TockerInputs t = TockerInputs {
   _tockerInputsLimit  :: Dynamic t Seconds
  ,_tockerInputsStart  :: Event t ()
  ,_tockerInputsReset  :: Event t ()
  ,_tockerInputsPause  :: Event t ()
  ,_tockerInputsResume :: Event t ()
  }

data TockerOutputs t = TockerOutputs {
    _tockerOutputsCountdown :: Dynamic t Seconds
  -- , _tockerOutputsAlarming  :: Event t Bool
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

          --  (-)
          --  <$> _tockerInputsLimit inputs
          --  <@> updated elapsedD
  pure $ TockerOutputs (zipDynWith (\x y -> (\v -> x - v) y) (_tockerInputsLimit inputs) elapsedD)

secondsFrom
  :: MonadWidget t m
  => Seconds -> m (Dynamic t Seconds)
secondsFrom initial = do
  e <- startTickingE 1
  fmap ((+ initial) . Seconds) <$> count e

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
  do startedOrNot <- holdDyn False $ leftmost [
          True <$ _metronomeInputsStart inputs
        , False <$ _metronomeInputsStop inputs
      ]
     let timeD = rateToPeriod <$> _metronomeInputsRate inputs
         changeRateOrStartStop = zipDynWith (,) timeD startedOrNot
     fmap MetronomeOutputs . switchDyn . fmap (\(rate, started) ->
        if not started
           then pure never
           -- it would be nice to emit a tick on click here
           -- right now this fires after a rate delay
           else void <$> startTickingE rate) $ changeRateOrStartStop

switchDyn :: (MonadHold t m, PostBuild t m, DomBuilder t m) =>
             Dynamic t (m (Event t a)) -> m (Event t a)
switchDyn = (switchPromptly never =<<) . dyn

-------------------------------------
-- GOOOOOOOOOOO!

prefix0 n | n < 10 = "0" <> show n
          | otherwise = show n

formatSeconds (Seconds s) =
                  let (m,s') = divMod s 60
                   in pack $ prefix0 m <> ":" <> prefix0 s'

main = runAndServe 8008 "static" $ do
  beepFile <- pure "243748__unfa__metronome-2khz-strong-pulse.mp3"
  let initialMetronomeRate = Rate 80
      this =
        do
          elAttr "script" (Map.singleton "src" "lowLag.js") (pure ())
          elAttr "script" (Map.singleton "src" "https://code.jquery.com/jquery-1.8.0.min.js") (pure ())
          elAttr "link" (Map.fromList [
                ("rel", "stylesheet"),
                ("href","https://s3-us-west-2.amazonaws.com/colors-css/2.2.0/colors.min.css")
              ]) (pure ())
          elAttr "link" (Map.fromList [
                ("rel", "stylesheet"),
                ("href","https://cdnjs.cloudflare.com/ajax/libs/bulma/0.4.3/css/bulma.min.css")
              ]) (pure ())

          elAttr "link" (Map.fromList [("rel", "stylesheet"), ("href","site.css")]) (pure ())
            -- <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css">

  mainWidgetWithHead this $ do
    elClass "section" "hero is-primary" $
      -- elClass "div" "container" $
        -- elClass "section" "hero is-primary" $
          "div" `elClass` "hero-body" $ do
            elClass "div" "container has-text-centered" $
              elClass "h1" "title" $ text "The Practice Pad"

    elClass "section" "section" $ do
      elClass "div" "container" $ mdo
        currentTimer <- createVal timerLimitControls 120
        tocker <- createTocker tockerControls
        (tockerControls, timerLimitControls) <- elClass "div" "tile is-ancestor" $ do
          (addTimeE, subTimeE) <- elClass "div" "tile is-parent is-8" $ do
            elClass "div" "tile is-child has-text-centered" $ do

              elClass "p" "box title big-number" $ dynText $ formatSeconds <$> (_tockerOutputsCountdown tocker)
              addTimeE <- elClassClick "div" "title button" (text "+")
              subTimeE <- elClassClick "div" "title button" (text "-")
              pure (60 <$ addTimeE, 60 <$ subTimeE)

          tc <- elClass "div" "tile is-2" $ do
            -- Controls
            elClass "div" "tile is-parent is-vertical" $ do
              let thingo a =
                   elClassClick "div" "tile box is-child button has-text-centered aligner" $
                     el "p" (text a)
              startE <- thingo "START"
              resetE <- thingo "RESET"
              pauseE <- thingo "PAUSE"
              resumeE <- thingo "RESUME"
              let limitB = _val currentTimer
              pure (TockerInputs limitB startE resetE pauseE resumeE)

            -- PRESETS
          presets <- elClass "div" "tile is-2" $ do
            elClass "div" "tile is-parent is-vertical" $ do
              let thingo a =
                    elClassClick "div" "tile is-child button has-text-centered aligner is-warning" $
                      el "p" (text a)
              _2min <- const 120 <$$> thingo "2:00"
              _3min <- const 180 <$$> thingo "3:00"
              _5min <- const 300 <$$> thingo "5:00"
              _10min <- const 600 <$$> thingo "10:00"
              _20min <- const 1200 <$$> thingo "20:00"
              pure . leftmost $ [_2min, _3min, _5min, _10min,_20min]
          pure (tc, ValControls addTimeE subTimeE presets)
        pure ()

      ----- METRONOME
      elClass "div" "container" $ mdo
        Val metronomeRate <- createVal rateInputs initialMetronomeRate
        elClass "div" "tile is-ancestor" $ do
          elClass "div" "tile is-parent is-8" $ do
            elClass "div" "tile is-child has-text-centered" $ do
              elClass "p" "box title big-number" $ dynText $ formatRate <$> metronomeRate
              pure ()

          elClass "div" "tile is-4" $ do
            elClass "div" "tile is-parent is-vertical" $ do
              elClass "p" "" (text "lol")

        rateInputs <- el "ul" $ do
          ------------
          rateInputs <-
            do (d1, i1) <- el "li" $ (,) <$> (const 1 <$$> button "-") <*> (const 1 <$$> button "+")
               (d5, i5) <- el "li" $ (,) <$> (const 5 <$$> button "- 5") <*> (const 5 <$$> button "+ 5")
               pure (ValControls (leftmost [i1, i5]) (leftmost [d1, d5]) never)
          bb2 <- el "li" $ button "Start/Stop Metronome"
          startStopMetronomeE <- toggle False bb2
          let starts = () <$ ffilter id (updated startStopMetronomeE)
              stops = () <$ ffilter not (updated startStopMetronomeE)
          MetronomeOutputs x <- createMetronome $ MetronomeInputs metronomeRate starts stops
          addVoidAction ((liftJSM $ lowLag_play "beep") <$ x)
          pure rateInputs
        pure ()

    el "div" $ do
      b <- button "configure audio"
      performEvent_ ((liftJSM lowLag_init >> liftJSM (lowLag_load beepFile "beep")) <$ b)

formatRate (Rate r) = pack (show r) <> " BPM"

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

elClassClick elem c s = do
  (e, _) <- elAttr' elem (Map.singleton "class" c) s
  return $ domEvent Click e

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap

-----------
-- foreign import javascript unsafe "lowLag['init']()"
--  lowLag_init :: IO ()

lowLag_init :: JSM ()
lowLag_init = do ll <- jsg ("lowLag" :: String)
                 ll ^. js0 ("init" :: String)
                 pure ()

-- foreign import javascript unsafe "lowLag['load']($1, $2)"
--  lowLag_load :: JSString -> JSString -> IO ()

lowLag_load :: JSString -> JSString -> JSM ()
lowLag_load  a b =
  do ll <- jsg ("lowLag" :: String)
     ll ^. js2 ("load" :: String) a b
     pure ()

-- foreign import javascript unsafe "lowLag['play']($1)"
--  lowLag_play :: JSString -> IO ()

lowLag_play :: JSString -> JSM ()
lowLag_play a =
  do ll <- jsg ("lowLag" :: String)
     ll ^. js1 ("play" :: String) a
     pure ()
