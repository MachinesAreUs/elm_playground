import List exposing(..)
import Graphics.Collage exposing(..)
import Graphics.Element exposing(..)
import Window exposing (..)
import Color exposing (..)
import Signal exposing (foldp, (~), (<~))
import Time exposing (fps)
import Keyboard
import Mouse

ballRadius   = 4 
periodLength = 100
maxAmp       = 300
minAmp       = 10
velocity     = 1

ball hue = circle ballRadius |> filled (hsl hue 1 0.5)

-- X points

xPositions windowWidth itemSize = 
  let adjustCoords x = x - (windowWidth / 2)
  in xPositions_ windowWidth itemSize [] |> map adjustCoords

xPositions_ remaining itemWidth positions = 
  let newPosition  = itemWidth * (length positions |> toFloat) + itemWidth
      newRemaining = remaining - itemWidth
  in if | remaining > itemWidth -> xPositions_ newRemaining itemWidth <| 
                                   newPosition :: positions                     
        | otherwise             -> reverse positions

-- Math stuff

angle v t p x =  (x - v * t) / p

ypos minA maxA w winWidth x = 
  let transX = x + (winWidth / 2)
      delta  = transX * (maxA - minA) / winWidth
  in (sin w) * (minA + delta)

-- Main

scene (wWidth, wHeight) time = 
  let (width, height) = (toFloat wWidth, toFloat wHeight)
      xcoords         = xPositions width (ballRadius * 4)
      angles          = map (angle velocity time periodLength) xcoords
      relYPos w' x    = ypos minAmp maxAmp w' width x
      toBall (x, w')  = ball w' |> move (x, relYPos w' x)
      dephase x' f    = moveX x' f
      basicWave       = map2 (,) xcoords angles |> map toBall
      dephasedWave    = basicWave |> map (dephase -periodLength)
      dephasedWave'   = basicWave |> map (dephase (-2*periodLength))
  in collage wWidth wHeight <| basicWave ++ dephasedWave ++ dephasedWave' 

main = let time = foldp (+) 0 (fps 30) 
       in Signal.map2 scene Window.dimensions time

