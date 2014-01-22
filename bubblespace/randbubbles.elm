import Window
import Random
import Text

radius      = 30
lineWidth   = 3
bubbleAlpha = 0.4

-- Util

(!!) list idx = drop idx list |> head

text = 
  toForm . Text.text . (Text.color white) . toText . show 

-- Program

bubble r content color = 
  let baseOutline = solid color
      outline     = { baseOutline | width <- lineWidth}
  in group [ circle (r+lineWidth) |> outlined outline
           , circle r             |> filled color |> alpha bubbleAlpha
           , text content
           ] 

scene {ttime, delta, rands, dim} =
  let (w,h) = dim
  in collage w h [ 
      bubble radius (rands !! 0) yellow |> move ( 50, 50)
    , bubble radius (rands !! 1) blue   |> move ( 50,-50)
    , bubble radius (rands !! 2) green  |> move (-50,-50)
    , bubble radius (rands !! 3) red    |> move (-50, 50)
  ] |> color black

type Input = { ttime: Time, delta: Time, rands: [Int], dim: (Int,Int) }

input = 
  let source = fps 30
      ttime  = foldp (+) 0 source
      rands  = combine <| map (\_ -> Random.range 0 100 source) [1..10] 
  in sampleOn ttime <| Input <~ ttime
                              ~ (inSeconds <~ source)
                              ~ rands
                              ~ Window.dimensions

main = lift scene input


