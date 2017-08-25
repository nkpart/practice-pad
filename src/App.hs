{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE JavaScriptFFI              #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE RecursiveDo                #-}
{-# LANGUAGE TupleSections              #-}

module App (appHead, appWidget) where

import           Control.Lens
import           Control.Monad               (join, void, when)
import           Control.Monad.IO.Class
import qualified Data.Map.Strict             as Map
import           Data.Monoid
import           Data.Text                   (pack)
import           Data.Time
import           Language.Javascript.JSaddle
import           Prelude                     hiding (words)
import           Reflex.Dom.Core             hiding (Pause, Reset)

{-
TODOS
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
   _tockerInputsStart  :: Event t ()
  ,_tockerInputsReset  :: Event t ()
  ,_tockerInputsPause  :: Event t ()
  ,_tockerInputsResume :: Event t ()
  }

switchTockers e =
  TockerInputs <$> (switchPromptly never (_tockerInputsStart <$> e))
               <*> (switchPromptly never (_tockerInputsReset <$> e))
               <*> (switchPromptly never (_tockerInputsPause <$> e))
               <*> (switchPromptly never (_tockerInputsResume <$> e))

data TockerOutputs t = TockerOutputs {
    _tockerOutputsCountdown :: Dynamic t Seconds,
    _tockerOutputsState     :: Dynamic t TockerState
  -- , _tockerOutputsAlarming  :: Event t Bool
  }

data TockerState = Ready | Paused | Ticking
  deriving (Eq, Show)

createTocker
  :: MonadWidget t m
  => Dynamic t Seconds -> TockerInputs t -> m (TockerOutputs t)
createTocker ll inputs = do
  rec elapsedAndState <-
        fmap join .
        widgetHold (pure (pure (0, Ready))) . leftmost $
          [ pure (pure (0, Ready)) <$ _tockerInputsReset inputs
          , (fmap.fmap) (,Ticking) (secondsFrom 0) <$ _tockerInputsStart inputs
          , (\((v, _), ()) -> pure (constDyn (v, Paused))) <$>
            attach (current elapsedAndState) (_tockerInputsPause inputs)
          , (\((v, _), ()) -> fmap (, Ticking) <$> secondsFrom v) <$>
            attach (current elapsedAndState) (_tockerInputsResume inputs)
          ]

          --  (-)
          --  <$> _tockerInputsLimit inputs
          --  <@> updated elapsedD
  let countdown = zipDynWith (-) ll (fmap fst elapsedAndState)
      states = uniqDyn $ fmap snd elapsedAndState
  pure $ TockerOutputs countdown states

controls thingo Ticking =
   do resetE <- thingo "CANCEL"
      pauseE <- thingo "PAUSE"
      let startE = never
          resumeE = never
      pure (TockerInputs startE resetE pauseE resumeE)

controls thingo Paused =
   do resetE <- thingo "CANCEL"
      resumeE <- thingo "RESUME"
      let startE = never
          pauseE = never
      pure (TockerInputs startE resetE pauseE resumeE)

controls thingo Ready =
   do startE <- thingo "START"
      let pauseE = never
          resumeE = never
          resetE = never
      pure (TockerInputs startE resetE pauseE resumeE)

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

prefix0 n | n < 10 = "0" <> show n
          | otherwise = show n

formatSeconds s =
  let posFormat (Seconds s) = let (m,s') = divMod s 60
                               in pack $ prefix0 m <> ":" <> prefix0 s'
   in if s >= 0 then posFormat s else "-" <> posFormat (-s) <> " "

initialMetronomeRate = Rate 80

appHead =
        do
          elAttr "script" (Map.singleton "src" "lowLag.js") (pure ())
          elAttr "script" (Map.singleton "src" "https://code.jquery.com/jquery-1.8.0.min.js") (pure ())
          elAttr "script" (Map.singleton "src" "https://use.fontawesome.com/a0b38b0227.js") (pure ())

          elAttr "link" (Map.fromList [
                ("rel", "stylesheet"),
                ("href","https://s3-us-west-2.amazonaws.com/colors-css/2.2.0/colors.min.css")
              ]) (pure ())
          elAttr "link" (Map.fromList [("rel", "stylesheet"), ("href","site.css")]) (pure ())
            -- <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css">

appWidget = do
    elClass "div" "" $ do
      elClass "h1" "title" $ text "The Practice Pad"


      elClass "div" "" $ mdo
        currentTimer <- createVal timerLimitControls 120
        tocker <- createTocker (_val currentTimer) tockerControls

        (tockerControls, timerLimitControls) <- elClass "div" "" $ do
          (tc, subTimeE, addTimeE) <- elClass "div" "" $ do
            (down, up) <- elClass "div" "" $ do
              dd <- elClassClick "div" "" $ elClass "i" "fa fa-minus" (pure ())
              elClass "div" "" $ dynText $ formatSeconds <$> _tockerOutputsCountdown tocker
              uu <- elClassClick "div" "" $ elClass "i" "fa fa-plus" (pure ())
              pure (dd, uu)
            tc <- elClass "div" "" $ do
                  let thingo a = elClassClick "div" "" (text a)
                  switchTockers =<< dyn (controls thingo <$> (_tockerOutputsState tocker))
            pure (tc, 60 <$ down, 60 <$ up)

          presets1 <-
            -- Controls
            elClass "div" "" $ do
              let thingo a = elClassClick "div" "" $ (text a)
              _2min <- const 60 <$$> thingo "1:00"
              _3min <- const 120 <$$> thingo "2:00"
              _5min <- const 180 <$$> thingo "3:00"
              _10min <- const 300 <$$> thingo "5:00"
              _20min <- const 480 <$$> thingo "8:00"
              pure . leftmost $ [_2min, _3min, _5min, _10min,_20min]

            -- PRESETS
          presets <-
            elClass "div" "" $ do
              let thingo a = elClassClick "div" "" $ (text a)
              _2min <- const 600 <$$> thingo "10:00"
              _3min <- const 900 <$$> thingo "15:00"
              _5min <- const 1200 <$$> thingo "20:00"
              _10min <- const 1500 <$$> thingo "25:00"
              _20min <- const 1800 <$$> thingo "30:00"
              pure . leftmost $ [_2min, _3min, _5min, _10min,_20min]
          pure (tc, ValControls addTimeE subTimeE (leftmost [presets1, presets]))
        pure ()

      ----- METRONOME
      elClass "div" "container" $ mdo
        let starts = () <$ ffilter id (updated startStopMetronomeE)
            stops = () <$ ffilter not (updated startStopMetronomeE)
        MetronomeOutputs tick <- createMetronome $ MetronomeInputs metronomeRate starts stops
        firstTick <- headE tick
        haveInitted <- hold False =<< performEvent ((initAudio *> pure True) <$ firstTick)
        performEvent_ (flip when playBeep <$> tag haveInitted tick)
        (metronomeRate, startStopMetronomeE) <- elClass "div" "" $ do
          metronomeRate <- elClass "div" "" $ do
            elClass "div" "" $ dynText $ formatRate <$> metronomeRate
            elClass "div" "" $ rateRange

          startStop <-
            elClass "div" "" $ do
              let thingo a =
                   elClassClick "div" "" $
                     text a
              toggle False =<< thingo "Start/Stop Metronome"
          pure (metronomeRate, startStop)
        pure ()

rateRange = (Rate . round) <$$> (inputRange 80 "40" "160")

formatRate (Rate r) = pack (show r)

startTickingE :: MonadWidget t m => NominalDiffTime -> m (Event t Integer)
startTickingE n = do
  started <- liftIO getCurrentTime
  l <- tickLossy n started
  pure (_tickInfo_n <$> l)

rateToPeriod :: Rate -> NominalDiffTime
rateToPeriod (Rate r) = (60/) . fromRational . toRational $ r

elClassClick elem c s = do
  (e, _) <- elAttr' elem (Map.singleton "class" c) s
  return $ domEvent Click e

inputRange init from to = do
  do x <- rangeInput (RangeInputConfig init never x)
     pure $ _rangeInput_value x
  where x = pure (Map.fromList [("min", from), ("max", to)] )

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap

-----------

initAudio =
  liftJSM $
    do lowLag_init
       let beepFile = "243748__unfa__metronome-2khz-strong-pulse.mp3"
       lowLag_load beepFile "beep"

playBeep = liftJSM $ lowLag_play "beep"

lowLag_init :: JSM ()
lowLag_init = do ll <- jsg ("lowLag" :: String)
                 ll ^. js0 ("init" :: String)
                 pure ()

lowLag_load :: JSString -> JSString -> JSM ()
lowLag_load  a b =
  do ll <- jsg ("lowLag" :: String)
     ll ^. js2 ("load" :: String) a b
     pure ()

lowLag_play :: JSString -> JSM ()
lowLag_play a =
  do ll <- jsg ("lowLag" :: String)
     ll ^. js1 ("play" :: String) a
     pure ()
