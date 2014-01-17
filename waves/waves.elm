import Mouse
import Window

ballRadius   = 2
periodLength = 100
maxAmp       = 200
minAmp       = 20
velocity     = 0.1

ball hue = circle ballRadius |> filled (hsv hue 1 1)

-- X points

xPositions windowWidth itemSize = 
  let adjustCoords x = x - (windowWidth / 2)
  in xPositions_ windowWidth (itemSize * 2) [] |> map adjustCoords

xPositions_ remaining itemWidth positions = 
  let newPosition  = itemWidth * (length positions |> toFloat) + itemWidth
      newRemaining = remaining - itemWidth
  in if | remaining > itemWidth -> xPositions_ newRemaining itemWidth <| 
                                   newPosition :: positions                     
        | otherwise             -> reverse positions

-- Math stuff

phaseAngle p x      = pi * x / p
phaseAngleT p v t x = pi * (x - v * t) / p
ypos minA maxA w winWidth x = 
  let transX = x + (winWidth / 2)
      delta  = transX * (maxA - minA) / winWidth
  in (sin w) * (minA + delta)

-- Main

scene (wWidth, wHeight) time = 
  let (wf, hf)        = (toFloat wWidth, toFloat wHeight)
      xcoords         = xPositions wf (ballRadius * 2)
      angles          = map (phaseAngleT periodLength velocity time) xcoords
      relYPos w' wf x = ypos minAmp maxAmp w' wf x
      toBall (x, w')  = ball w' |> move (x, relYPos w' wf x)
      dephase x' f    = moveX x' f
      basicWave       = map toBall <| zip xcoords angles
      dephasedWave    = basicWave |> map (dephase -periodLength)
  in collage wWidth wHeight <| basicWave ++ dephasedWave

main = lift2 scene Window.dimensions <| foldp (+) 0 (fps 30)
