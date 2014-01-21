import Window
import Graphics.Collage 

side = 500
rec  = 8

type Triangle = [(Float,Float)]

middle (x,y) (x',y') = ((x + x') / 2, (y + y') / 2)

whiteTriangle : Triangle -> Triangle
whiteTriangle  [p1, p2, p3] = 
  [middle p1 p2, middle p2 p3, middle p1 p3]

subTriangles : Triangle -> [Triangle]
subTriangles [p1, p2, p3] = [
    [p1, middle p1 p2, middle p1 p3],
    [middle p1 p2, p2, middle p2 p3],
    [middle p1 p3, middle p2 p3, p3]
  ]

set : Int -> Int -> Triangle -> [Triangle]
set n idx t = 
  if | idx == n  -> []
     | otherwise -> whiteTriangle t :: (concat (map (set n (idx + 1)) (subTriangles t))) 

sarpinksi (w,h) = 
  let mainTriangle = [(0,0),(side * cos (pi/3), side * sin (pi/3)),(side,0)]
  in collage w h <|
    (mainTriangle |> filled black) :: (set rec 0 mainTriangle |> map (filled white))

main = lift sarpinksi Window.dimensions