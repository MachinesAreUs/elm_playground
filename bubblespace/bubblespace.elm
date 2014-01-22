import Window
import Graphics.Collage 
import Random
import Text
import Signal

radius      = 30
lineWidth   = 3
bubbleAlpha = 0.4

-- Util

(!!) list idx = drop idx list |> head

text = 
  toForm . Text.text . (Text.color white) . toText . show 

-- Program

bubble r color = 
  let baseOutline = solid color
      outline     = { baseOutline | width <- lineWidth}
  in group [
    circle (r+lineWidth) |> outlined outline,
    circle r             |> filled color |> alpha bubbleAlpha
  ] 

scene {ttime, delta, rands, dim} =
  let (w,h)     = dim
      colors    = [red, orange, yellow, green, blue, purple]
      toBubble  = bubble radius
  in collage w h 
    (map toBubble colors)
    |> color black

type Input = { ttime: Time, delta: Time, rands: [Int], dim: (Int,Int) }

input = 
  let source = fps 30
      ttime  = foldp (+) 0 source
      rands  = combine <| map (\_ -> Random.range 0 100 source) [1..100] 
  in sampleOn ttime <| Input <~ ttime
                              ~ (inSeconds <~ source)
                              ~ rands
                              ~ Window.dimensions

main = lift scene input
