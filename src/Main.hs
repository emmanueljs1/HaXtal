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
import Data.JSString
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
  el "br" blank
  (e, _) <- element "canvas" def blank
  let canvas = DOM.HTMLCanvasElement $
               DOM.unElement . DOM.toElement . _element_raw $ e
  ctx' <- CVS.getContext canvas (pack "2d")
  let ctx = DOM.CanvasRenderingContext2D ctx'
  CVS.setWidth canvas $ round canvasWidth
  CVS.setHeight canvas $ round canvasHeight
  -- Draw the default fractal from the starting selection of the dropdown
  drawPaths ctx (getPaths defaultLevels $ lsysFromDD 1)
  -- Attach the redrawing of fractals to the changing of the dropdown
  performEvent_ $ liftIO . (drawPaths ctx) . getPaths defaultLevels . lsysFromDD
               <$> (updated $ value dd)
  return ()

-- Takes a list of a list of points an
drawPaths ::(MonadIO m) => DOM.CanvasRenderingContext2D -> [[Vector]] -> m ()
drawPaths ctx paths = do
  CVS.clearRect ctx 0 0 canvasWidth canvasHeight
  CVS.save ctx
  CVS.beginPath ctx
  traverse_ drawPath paths
  CVS.stroke ctx
  CVS.restore ctx
  where
    drawPath p@(p1:_) = do
      let tr = map2Tuple (\e -> e * drawingScale + canvasWidth / 2.0)
      uncurry (CVS.moveTo ctx) $ tr p1
      traverse_ (uncurry (CVS.lineTo ctx) . tr) p

-------------- Helpers and Constants -------------------------------------------
canvasWidth = 1000.0
canvasHeight = 1000.0
drawingScale = 10.0
defaultLevels = 4
ddOpts = (constDyn $ (1 =: "Gosper")
                  <> (2 =: "Hilbert")
                  <> (3 =: "Sierpinski")
                  <> (4 =: "Dragon")
                  <> (5 =: "Sierpinski Arrowhead")
                  <> (6 =: "Plant"))
lsysFromDD :: Integer -> LSystem
lsysFromDD 1 = gosper
lsysFromDD 2 = hilbert
lsysFromDD 3 = sierpinski
lsysFromDD 4 = dragon
lsysFromDD 5 = sierpinskiArrowhead
lsysFromDD 6 = plant
lsysFromDD _ = dragon
--------------------------------------------------------------------------------
