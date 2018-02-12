module Main where

import Prelude

import Color (Color, black)
import Color.Scheme.MaterialDesign (deepPurple)
import Color.Scheme.X11 (grey, teal)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Array (replicate, sortBy, (..))
import Data.Foldable (foldMap)
import Data.Int (toNumber)
import Data.Maybe (fromJust, maybe)
import Data.Set (isEmpty)
import FRP (FRP)
import FRP.Behavior (Behavior, animate, fixB, integral', switcher)
import FRP.Behavior.Mouse (buttons)
import FRP.Behavior.Mouse as Mouse
import FRP.Behavior.Time as Time
import FRP.Event.Class (fold)
import FRP.Event.Mouse (down)
import Global (infinity)
import Graphics.Canvas (CANVAS, Composite(..), getCanvasElementById, getCanvasHeight, getCanvasWidth, getContext2D, setCanvasHeight, setCanvasWidth)
import Graphics.Drawing (Drawing, FillStyle, OutlineStyle(..), circle, fillColor, filled, lineWidth, outlineColor, outlined, rectangle, render, scale, translate)
import Math ((%))
import Partial.Unsafe (unsafePartial)

foreign import logAny :: forall a. a -> a


type Rectangle = { h :: Number, y :: Number, x :: Number , w :: Number }

scene :: { w :: Number, h :: Number } -> Behavior Drawing
scene { w, h } = pure background <> map renderTetrisGrid  gridArray where
  background :: Drawing
  background = filled (fillColor black) (rectangle 0.0 0.0 w h)

  scaleFactor :: Number
  scaleFactor = max w h / 16.0
  
  size = 1.0

  renderTetrisBlock :: forall a. Rectangle -> Drawing
  renderTetrisBlock {x, y, h, w} =  scale scaleFactor scaleFactor <<< translate 0.0 0.0 <<< scale size size $ 
                          outlined (outlineColor (altColor x y) <> gridOutline) (rectangle x y h w) 


  renderTetrisGrid :: Array Rectangle ->  Drawing
  renderTetrisGrid =  foldMap renderTetrisBlock

gridOutline :: OutlineStyle
gridOutline = outlineColor black <>  lineWidth 0.05  

altColor :: Number -> Number -> Color
altColor x y =  if (x % 2.0 == 0.0 && y % 2.0 == 0.0 ) then grey else grey

gridArray :: Behavior (Array Rectangle)
gridArray = singleGrid <$> Mouse.position where
             singleGrid x = do
                xx <- 1 .. 25
                yy <- 1 .. 25 
                let xxx = toNumber xx
                    yyy = toNumber yy
                pure {x: xxx , y: yyy , h: 2.0, w: 2.0}

main :: forall eff. Eff (canvas :: CANVAS, frp :: FRP | eff) Unit
main = do
  mcanvas <- getCanvasElementById "canvas"
  let canvas = unsafePartial (fromJust mcanvas)
  ctx <- getContext2D canvas
  w <- getCanvasWidth canvas
  h <- getCanvasHeight canvas
  _ <- setCanvasWidth w canvas
  _ <- setCanvasHeight h canvas
  _ <- animate (scene { w, h }) (render ctx)
  pure unit

