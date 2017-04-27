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
import Data.JSString hiding (concat)
import Data.Monoid
import Data.Foldable
import qualified GHCJS.DOM.JSFFI.Generated.CanvasRenderingContext2D as CVS
import qualified GHCJS.DOM.Types as DOM
import qualified GHCJS.DOM.JSFFI.Generated.HTMLCanvasElement as CVS

main :: IO ()
main = mainWidget $ do
  el "h1" $ text "Welcome to HaXtal!"
  el "h2" $ text "Please select a fractal to display:"
  dd <- dropdown 1 ddOpts def
  
  let tddLsys = tagPromptlyDyn (lsysFromDD <$> (value dd)) (_dropdown_change dd)
      rulesEv = rulesString <$> tddLsys
      rulesConfig = def {_textAreaConfig_setValue = T.pack <$> rulesEv}
      varsEv = varsString <$> tddLsys
      varsConfig = def {_textInputConfig_setValue = T.pack <$> varsEv}
      angleEv = angleString <$> tddLsys
      angleConfig = def {_textInputConfig_setValue = T.pack <$> angleEv}
      startEv = startString <$> tddLsys
      startConfig = def {_textInputConfig_setValue = T.pack <$> startEv}
  startText <- textInput startConfig
  rulesText <- textArea rulesConfig
  varsText <- textInput varsConfig
  angleText <- textInput angleConfig
  b <- button "Generate"
  el "br" blank
  (e, _) <- element "canvas" def blank
  let canvas = DOM.HTMLCanvasElement $
               DOM.unElement . DOM.toElement . _element_raw $ e
  ctx' <- CVS.getContext canvas (pack "2d")
  let ctx = DOM.CanvasRenderingContext2D ctx'
  CVS.setWidth canvas $ round canvasWidth
  CVS.setHeight canvas $ round canvasHeight
  -- Put the origin at the center of the canvas
  -- CVS.translate ctx (canvasWidth / 2.0) (canvasHeight / 2.0)
  -- Draw the default fractal from the starting selection of the dropdown
  drawPaths ctx (getPaths defaultLevels $ lsysFromDD 1)

  -- Attach the redrawing of fractals to the 'generate' button and
  -- pass values of the fields to getLSystem.
  --
  -- Breakdown (for when I get confused)
  -- First, we combine all of the text inputs into one dynamic, then we
  -- map the value of the new dynamic to the action of the 'generate'
  -- button being pressed. Then we create and draw the lsystem, lift
  -- to an IO instance and perform the event.
  let u = T.unpack
  performEvent_ $ liftIO . drawPaths ctx . getPaths defaultLevels
                . getLSystem
               <$> tagPromptlyDyn (mconcat
                [ (\x -> mempty {lscStart = x}) . u <$> value startText
                , (\x -> mempty {lscRules = x}) . u <$> value rulesText
                , (\x -> mempty {lscVars = x})  . u <$> value varsText
                , (\x -> mempty {lscAngle = x}) . u <$> value angleText
                ] ) b

-- Draws a list of paths to the context
drawPaths ::(MonadIO m) => DOM.CanvasRenderingContext2D -> [[Point]] -> m ()
drawPaths ctx paths = do
  CVS.clearRect ctx 0 0 canvasWidth canvasHeight
  CVS.save ctx
  CVS.translate ctx ((- minX dBounds) * drawingScale) ((- minY dBounds) * drawingScale)
  CVS.beginPath ctx
  -- Draw every path in the lsystem
  traverse_ drawPath paths
  CVS.stroke ctx
  CVS.restore ctx
  where
    dBounds = getDrawBounds $ concat paths
    drawingScale = 500.0 / ((maxX dBounds) - (minX dBounds))
    drawPath p@(p1:_) = do
      let tr (Point p) = Point $ mapTuple (* drawingScale) p
      uncurry (CVS.moveTo ctx) $ getP . tr $ p1
      traverse_ (uncurry (CVS.lineTo ctx) . getP . tr) p

-------------- Helpers and Constants -------------------------------------------
  


canvasWidth = 1000.0
canvasHeight = 1000.0
defaultLevels = 5
ddOpts = constDyn $ (1 =: "Gosper")
                  <> (2 =: "Hilbert")
                  <> (3 =: "Sierpinski")
                  <> (4 =: "Dragon")
                  <> (5 =: "Sierpinski Arrowhead")
                  <> (6 =: "Plant")
                  <> (7 =: "Sunflower")

lsysFromDD :: Integer -> LSystem
lsysFromDD 1 = gosper
lsysFromDD 2 = hilbert
lsysFromDD 3 = sierpinski
lsysFromDD 4 = dragon
lsysFromDD 5 = sierpinskiArrowhead
lsysFromDD 6 = plant
lsysFromDD 7 = sunflower
lsysFromDD _ = plant

--------------------------------------------------------------------------------
