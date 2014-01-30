import Window
import Graphics.Collage 

iterations = 6

middle (x,y) (x',y') = ((x + x') / 2, (y + y') / 2)

distance (x,y) (x',y') = sqrt <| (x' - x) ^ 2 + (y' - y) ^ 2

trisect (x,y) (x',y') =
  let (dx, dy) = ((x' - x) / 3, (y' - y) / 3)
  in ((x + dx, y + dy), (x + 2 * dx, y + 2 * dy))

complete (x,y) (x',y') = 
  let side    = distance (x,y) (x',y')
      theta   = (pi / 3) + atan2 (y'-y) (x'-x)
      (a2,b2) = (side * cos theta, side * sin theta)
  in (x + a2, y + b2)

koch_set n idx [p1, p2, p3] = 
  let (a1,a2)      = trisect p1 p2
      (b1,b2)      = trisect p2 p3
      (c1,c2)      = trisect p1 p3
      subTriangles = [ [a1, complete a1 a2, a2]
                     , [b1, complete b1 b2, b2]
                     , [c2, complete c2 c1, c1]
                     , [c1, p1, a1]
                     , [a2, p2, b1]
                     , [b2, p3, c2]
                     ]
  in if | idx < n   -> [p1,p2,p3] :: concat (map (koch_set n (idx + 1)) subTriangles)
        | otherwise -> []

koch (w,h) t = 
  let style        = filled (hsv (t * pi / 2000 ) 0.9 0.9)
      side         = 0.8 * min (toFloat w) (toFloat h)
      (dx,dy)      = (side / 2, tan (pi / 6) * side / 2)
      mainTriangle = [ (-dx, -dy)
                     , (side * cos (pi / 3) - dx, side * sin (pi / 3) - dy)
                     , (dx, -dy)
                     ]
  in color black <| collage w h 
                 <| map style
                 <| koch_set iterations 0 mainTriangle

main = koch <~ Window.dimensions
             ~ foldp (+) 0 (fps 0.2)
