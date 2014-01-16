import Mouse
import Window

ballRadius = 5

ball = circle ballRadius |> filled red

scene (w,h) = 
  let (wi, hi) = (toFloat w, toFloat h)
      topLeft  =  ballRadius - (wi/2)
  in collage w h 
    [ ball |> move (topLeft, 0) ]

{-xPositions : [Float]
xPositions with itemSize spaceSize = 
  let totalSize = itemSize + spaceSize
  in []-}

main = lift scene Window.dimensions
