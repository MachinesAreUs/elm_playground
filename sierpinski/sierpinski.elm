import List exposing(..)
import Graphics.Collage exposing(..)
import Window exposing (..)
import Color exposing (..)

iterations = 8

middle (x,y) (x',y') = ((x + x') / 2, (y + y') / 2)

set n idx [p1, p2, p3] = 
  let [a,b,c]      = [middle p1 p2, middle p2 p3, middle p1 p3]
      subTriangles = [
        [p1, a, c],
        [a, p2, b],
        [c, b, p3]
      ]
  in if | idx < n   -> [a,b,c] :: (concat (map (set n (idx + 1)) subTriangles)) 
        | otherwise -> []

sierpinski (w,h) = 
  let side         = 0.8 * min (toFloat w) (toFloat h)
      mainTriangle = [(0,0), (side * cos (pi / 3), side * sin (pi / 3)), (side,0)]
      center f     = move (-side / 2, tan (pi / 6) * -side / 2) f
  in collage w h 
       <| map center
       <| (mainTriangle |> filled blue) :: 
          (set iterations 0 mainTriangle |> map (filled white))

main = Signal.map sierpinski Window.dimensions
