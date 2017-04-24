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
import Data.Foldable (traverse_)
import qualified GHCJS.DOM.JSFFI.Generated.CanvasRenderingContext2D as CVS
import qualified GHCJS.DOM.Types as DOM
import qualified GHCJS.DOM.JSFFI.Generated.HTMLCanvasElement as CVS

main :: IO ()
main = mainWidget $ do
  el "h1" $ text "Welcome to HaXtal!"
  el "h2" $ text "Please select a fractal to display:"
  dd <- dropdown 1 (constDyn $ (1 =: "Sierpinski") <> (2 =: "Gosper")) def
  el "br" $ blank
  (e, _) <- element "canvas" def blank
  let canvas = DOM.HTMLCanvasElement $ DOM.unElement . DOM.toElement . _element_raw $ e
  ctx' <- CVS.getContext canvas (pack "2d")
  let ctx = DOM.CanvasRenderingContext2D $ ctx'
  CVS.setWidth canvas 1000
  CVS.setHeight canvas 1000
  drawPaths (getPaths gosper 5) ctx
  --traverse_ (flip drawPaths ctx) (getPaths . lsysFromDD $ value dd)

lsysFromDD :: Int -> LSystem
lsysFromDD 1 = sierpinski
lsysFromDD 2 = gosper
lsysFromDD _ = dragon

-- Takes a list of a list of points an
drawPaths :: (MonadIO m) => [[Vector]] -> DOM.CanvasRenderingContext2D -> m ()
drawPaths paths ctx = CVS.beginPath ctx >> (traverse_ drawPath paths) >> CVS.stroke ctx where
  drawPath p@(p1:_) = do
    let t v = map2Tuple (* 5.0) v
    uncurry (CVS.moveTo ctx) $ t p1
    traverse_ (\p' -> uncurry (CVS.lineTo ctx) $ t p') p
