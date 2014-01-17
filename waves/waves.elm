import Mouse
import Window

ballRadius   = 2
periodLength = 200
amplitude    = 100

ball hue = circle ballRadius |> filled (hsv hue 0.9 0.9)

-- X points

xPositions windowWidth itemSize = 
  let adjustCoords x = x - (windowWidth / 2)
  in xPositions_ windowWidth (itemSize*2) [] |> map adjustCoords

xPositions_ remaining itemWidth positions = 
  let newPosition  = itemWidth * (length positions |> toFloat) + itemWidth
      newRemaining = remaining - itemWidth
  in if | remaining > itemWidth -> xPositions_ newRemaining itemWidth <| 
                                   newPosition :: positions                     
        | otherwise             -> reverse positions

-- Math stuff

pahseAngle p x = pi * x / p
ypos a w       = (sin w) * a

-- Main        

scene (w,h) = 
  let (wf, hf)     = (toFloat w, toFloat h)
      topLeft      = ballRadius - (wf/2)
      xcoords      = xPositions wf (ballRadius * 2)
      angles       = map (pahseAngle periodLength) xcoords
      toBall (x,w) = ball w |> move (x, ypos amplitude w)
  in collage w h <| map toBall <| zip xcoords angles

main = lift scene Window.dimensions
