import Window
import Graphics.Collage 

iterations = 6

middle (x,y) (x',y') = ((x + x') / 2, (y + y') / 2)

distance (x,y) (x',y') = sqrt <| (x' - x) ^ 2 + (y' - y) ^ 2

trisect (x,y) (x',y') =
  let (dx, dy)       = ((x' - x) / 3, (y' - y) / 3)
  in ((x + dx, y + dy), (x + 2 * dx, y + 2 * dy))

complete (x,y) (x',y') = 
  let (x'',y'') = middle (x,y) (x',y')
      side      = distance (x,y) (x',y')
      height    = tan (pi/3) * side / 2
      theta     = (pi/2) + atan2 (y'-y) (x'-x)
      (a1,b1)   = (x,y)
      (a2,b2)   = (x''-x,y''-y)
      (a3,b3)   = (height * cos theta, height * sin theta)
  in (a1 + a2 + a3, b1 + b2 + b3)

set n idx [p1, p2, p3] = 
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
      subSet l     = map (set n (idx + 1)) l |> concat
  in if | idx == 0  -> [p1,p2,p3] :: subSet subTriangles
        | idx < n   -> [p1,p2,p3] :: subSet subTriangles
        | otherwise -> []

koch (w,h) t = 
  let style        = filled (hsv (t * pi / 2000 ) 0.9 0.9)
      side         = 0.8 * min (toFloat w) (toFloat h)
      mainTriangle = [(0,0), (side * cos (pi / 3), side * sin (pi / 3)), (side,0)]
      center f     = move (-side / 2, tan (pi / 6) * -side / 2) f
  in collage w h
       <| map center
       <| (mainTriangle |> style) :: 
          (set iterations 0 mainTriangle |> map style)

main = koch <~ Window.dimensions
             ~ foldp (+) 0 (fps 10)
