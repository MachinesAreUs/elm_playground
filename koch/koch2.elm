import Window
import List exposing(..)
import Graphics.Collage exposing(..)
import Graphics.Element exposing(..)
import Window exposing (..)
import Color exposing (..)
import Signal exposing (foldp, (~), (<~))
import Time exposing (fps)
import Keyboard

iterations = 4
 
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

kochSet n idx [p1,p5] =
  let (p2,p4) = trisect p1 p5
      p3      = complete p2 p4
  in if | idx < n   -> [[p1,p2],[p2,p3],[p3,p4],[p4,p5]]
                         |> map (kochSet n (idx + 1)) 
                         |> concat 
        | otherwise -> [[p1,p5]]

koch (w,h) t theta = 
  let style      = filled (hsl (t * pi / 1000 ) 0.9 0.8) 
      side       = 0.8 * min (toFloat w) (toFloat h)
      (dx,dy)    = (side / 2, tan (pi / 6) * side / 2)
      p1         = (-dx, -dy)
      p2         = (side * cos (pi / 3) - dx, side * sin (pi / 3) - dy)
      p3         = (dx, -dy)
      initialSet = [[p1,p2],[p2,p3],[p3,p1]]
      heads l    = l |> head |> Maybe.withDefault (0,0)
      curve      = initialSet 
                     |> map (kochSet iterations 0)
                     |> concat
                     |> map heads
                     |> polygon
                     |> style
  in collage w h [ rotate theta curve ] |> color black

time       = foldp (+) 0 (fps 2)
clickCount = foldp (\{x,y} c -> c + x) 0 Keyboard.arrows
angle      = Signal.map toFloat clickCount

main = koch <~ Window.dimensions 
             ~ time
             ~ angle
