{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

import Draw
import LSystem
import Reflex.Dom
import Control.Monad.IO.Class
import Control.Monad (foldM_)
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.JSString
import Data.Monoid

import qualified GHCJS.DOM.JSFFI.Generated.CanvasRenderingContext2D as D
import qualified GHCJS.DOM.Types as DOM
import qualified GHCJS.DOM.JSFFI.Generated.HTMLCanvasElement as CTX
main :: IO ()
main = mainWidget $ do
  el "h1" $ text "Welcome to HaXtal!"
  el "h2" $ text "Please select a fractal to display:"
  (e, _) <- element "canvas" def blank
  let canvas = DOM.HTMLCanvasElement $ DOM.unElement . DOM.toElement . _element_raw $ e
  ctx' <- CTX.getContext canvas (pack "2d")
  let ctx = DOM.CanvasRenderingContext2D $ ctx'
  CTX.setWidth canvas 1000
  CTX.setHeight canvas 1000
  drawPaths (getPaths gosper 5) ctx

drawPaths :: (MonadIO m) => [[Vector]] -> DOM.CanvasRenderingContext2D -> m ()
drawPaths paths ctx = D.beginPath ctx >> (foldM_ drawPath () paths) >> D.stroke ctx where
  drawPath _ p@(p1:_) = do
    let t v = map2Tuple (* 5.0) v
    uncurry (D.moveTo ctx) $ t p1
    foldM_ (\_ poi -> uncurry (D.lineTo ctx) $ t poi) () p
