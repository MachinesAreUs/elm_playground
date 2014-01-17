import Mouse
import Window

ballRadius   = 2
periodLength = 200
amplitude    = 100

ball hue = circle ballRadius |> filled (hsv hue 0.9 0.9)

-- X points

xPositions windowWidth itemSize = 
  xPositions_ windowWidth (itemSize*2) [] |> map (\p -> p - (windowWidth / 2))


xPositions_ remaining itemWidth positions = 
  let newPosition  = itemWidth * (length positions |> toFloat) + itemWidth
      newRemaining = remaining - itemWidth
  in if | remaining > itemWidth -> xPositions_ newRemaining itemWidth <| 
                                   newPosition :: positions                     
        | otherwise             -> reverse positions

-- Y points

yPositionsForX xcoords = 
  let toYcoord x = sin (pi * x / periodLength) * amplitude
  in map toYcoord xcoords

thetaForX x = pi * x / periodLength

-- Main        

scene (w,h) = 
  let (wf, hf) = (toFloat w, toFloat h)
      topLeft  = ballRadius - (wf/2)
      xcoords  = xPositions wf (ballRadius * 2)
      --ycoords  = yPositionsForX xcoords
      angles   = map thetaForX xcoords
      ypos w   = (sin w) * amplitude
      toBall (x,w) = ball w |> move (x, ypos w)
  in collage w h <| map toBall <| zip xcoords angles

main = lift scene Window.dimensions
