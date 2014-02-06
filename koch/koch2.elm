import Window
import Graphics.Collage 
 
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

koch_set n idx [p1,p5] =
  let (p2,p4) = trisect p1 p5
      p3      = complete p2 p4
  in if | idx < n   -> concat <| map (koch_set n (idx + 1)) 
                              <| [[p1,p2],[p2,p3],[p3,p4],[p4,p5]]
        | otherwise -> [[p1,p5]]

koch (w,h) t = 
  let iterations = 6
      style      = traced <| solid (hsv (t * pi / 2000 ) 0.9 0.9) 
      side       = 0.8 * min (toFloat w) (toFloat h)
      (dx,dy)    = (side / 2, tan (pi / 6) * side / 2)
      p1         = (-dx, -dy)
      p2         = (side * cos (pi / 3) - dx, side * sin (pi / 3) - dy)
      p3         = (dx, -dy)
      initialSet = [[p1,p2],[p2,p3],[p3,p1]]
      unify      = map head 
  in color black <| collage w h [ style <| unify
                                        <| concat 
                                        <| map (koch_set 6 0) 
                                        <| initialSet 
                                ]

main = koch <~ Window.dimensions 
             ~ foldp (+) 0 (fps 2)