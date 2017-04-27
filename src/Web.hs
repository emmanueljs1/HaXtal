{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

import Draw
import LSystem
import Examples
import Reflex.Dom
import Control.Monad.IO.Class
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.JSString hiding (concat, count)
import Data.Monoid
import Test.QuickCheck
import Data.Foldable
import Text.Read (readMaybe)
import qualified GHCJS.DOM.JSFFI.Generated.CanvasRenderingContext2D as CVS
import qualified GHCJS.DOM.Types as DOM
import qualified GHCJS.DOM.JSFFI.Generated.HTMLCanvasElement as CVS

main :: IO ()
main = mainWidget $ do
  rec
    let defLsys = lsysFromDD 1
        u = T.unpack
        p = T.pack
    el "h1" $ text "Welcome to HaXtal!"
    el "h2" $ text "Please select a fractal to display:"
    dd <- dropdown 1 dropdownOptions def

    -- Generates an infinite list of lsystems
    randomLs <- liftIO $ generate (infiniteListOf (arbitrary :: Gen LSystem))
    randCount <- count randButton
    let lsysEv = tagPromptlyDyn (lsysFromDD <$> (value dd)) (_dropdown_change dd)
        randLsysEv = (randomLs !!) <$> (updated randCount)

        rulesEv = leftmost [rulesString <$> lsysEv, rulesString <$> randLsysEv]
        rulesConfig = def {_textAreaConfig_setValue = p <$> rulesEv,
                           _textAreaConfig_initialValue = p $ rulesString defLsys}

        varsEv = leftmost [varsString <$> lsysEv, varsString <$> randLsysEv]
        varsConfig = def {_textInputConfig_setValue = T.pack <$> varsEv,
                          _textInputConfig_initialValue = p $ varsString defLsys}

        angleEv = leftmost [angleString <$> lsysEv, angleString <$> randLsysEv]
        angleConfig = def {_textInputConfig_setValue = T.pack <$> angleEv,
                           _textInputConfig_initialValue = p $ angleString defLsys}

        startEv = leftmost [startString <$> lsysEv, startString <$> randLsysEv]
        startConfig = def {_textInputConfig_setValue = T.pack <$> startEv,
                           _textInputConfig_initialValue = p $ startString defLsys}
    startText  <- textInput startConfig
    rulesText  <- textArea rulesConfig
    varsText   <- textInput varsConfig
    angleText  <- textInput angleConfig
    levelsText <- textInput def
                  {_textInputConfig_initialValue = T.pack $ show defaultLevels}
    generateButton <- button "Generate"
    randButton <- button "Random"
    el "br" blank

    -- Get canvas element
    (e, _) <- element "canvas" def blank
    let canvas = DOM.HTMLCanvasElement $
                DOM.unElement . DOM.toElement . _element_raw $ e
    ctx' <- CVS.getContext canvas (pack "2d")
    let ctx = DOM.CanvasRenderingContext2D ctx'
    CVS.setWidth canvas $ round canvasWidth
    CVS.setHeight canvas $ round canvasHeight

    -- Draw the default fractal from the starting selection of the dropdown
    drawPaths ctx (getPaths defaultLevels $ defLsys)

    -- Attach the redrawing of fractals to the 'generate' button and
    -- pass values of the fields to getLSystem.
    --
    -- Breakdown (for when I get confused)
    -- First, we combine all of the text inputs into one dynamic, then we
    -- map the value of the new dynamic to the action of the 'generate'
    -- button being pressed. Then we create and draw the lsystem, lift
    -- to an IO instance and perform the event.
  
    performEvent_ $ liftIO . drawPaths ctx . getPathsForLSysComps
                <$> tagPromptlyDyn (mconcat
                  [ (\x -> mempty {lscStart = x})  . u <$> value startText
                  , (\x -> mempty {lscRules = x})  . u <$> value rulesText
                  , (\x -> mempty {lscVars = x})   . u <$> value varsText
                  , (\x -> mempty {lscAngle = x})  . u <$> value angleText
                  , (\x -> mempty {lscLevels = x}) . u <$> value levelsText
                  ] ) generateButton
  return ()

getPathsForLSysComps lsc@(LSysComps _ _ _ _ levels) = getPaths l (getLSystem lsc)
  where
    l = getLevelStr levels

getLevelStr a = case (readMaybe a) of
  (Just l) -> l
  Nothing  -> defaultLevels

-- Draws a list of paths to the context
drawPaths ::(MonadIO m) => DOM.CanvasRenderingContext2D -> [[Point]] -> m ()
drawPaths ctx paths = do
  CVS.clearRect ctx 0 0 canvasWidth canvasHeight
  CVS.save ctx
  (CVS.translate ctx) (minX dBounds * (-drawingScale) + 30)
                      $ minY dBounds * (-drawingScale) + 30
  CVS.beginPath ctx
  -- Draw every path in the lsystem
  traverse_ drawPath paths
  CVS.stroke ctx
  CVS.restore ctx
  where
    dBounds = getDrawBounds $ concat paths
    drawingScale = 500.0 / ((maxX dBounds) - (minX dBounds))
    drawPath p@(p1:_) = do
      let tr p = mulSV drawingScale p
      uncurry (CVS.moveTo ctx) $ getV . tr $ p1
      traverse_ (uncurry (CVS.lineTo ctx) . getV . tr) p

-------------- Helpers and Constants -------------------------------------------
canvasWidth = 1000.0
canvasHeight = 1000.0
defaultLevels = 5
dropdownOptions = constDyn $ (1 =: "Gosper")
                  <> (2 =: "Hilbert")
                  <> (3 =: "Sierpinski")
                  <> (4 =: "Dragon")
                  <> (5 =: "Dragon 2")
                  <> (6 =: "Sierpinski Arrowhead")
                  <> (7 =: "Plant")
                  <> (8 =: "Sunflower")
                  <> (9 =: "Koch Lake")

lsysFromDD :: Integer -> LSystem
lsysFromDD 1 = gosper
lsysFromDD 2 = hilbert
lsysFromDD 3 = sierpinski
lsysFromDD 4 = dragon
lsysFromDD 5 = dragon2
lsysFromDD 6 = sierpinskiArrowhead
lsysFromDD 7 = plant
lsysFromDD 8 = sunflower
lsysFromDD 9 = kochLake

--------------------------------------------------------------------------------
