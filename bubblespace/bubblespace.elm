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

scatter : (Int,Int) -> Int -> [Int] -> Form -> Form
scatter (w,h) idx rands f =
  let (wf,hf) = (toFloat w, toFloat h)
      randsf  = map toFloat rands
      [a,b]   = drop (idx * 2) randsf |> take 2 
      pos     = (a * wf / 200, b * hf / 200)
  in move pos f

bubble r color = 
  let baseOutline = solid color
      outline     = { baseOutline | width <- lineWidth}
  in group [
    circle (r+lineWidth) |> outlined outline,
    circle r             |> filled color |> alpha bubbleAlpha
  ] 


scene {ttime, delta, rands, dim} =
  let (w,h)                = dim
      toBubble             = bubble radius
      colors               = [red, orange, yellow, green, blue, purple]
      unpositionedBubbles  = map toBubble colors
      indexedBubbles       = zip unpositionedBubbles [0..10]
      --bubbles              = map (\(f,idx) -> scatter (w,h) idx rands f) indexedBubbles
      bubbles              = map fst indexedBubbles
  in collage w h 
    bubbles
    |> color black

type Input = { ttime: Time, delta: Time, rands: [Int], dim: (Int,Int) }

input = 
  let source = fps 1
      ttime  = foldp (+) 0 source
      rands  = combine <| map (\_ -> Random.range -100 100 source) [1..100] 
  in sampleOn ttime <| Input <~ ttime
                              ~ (inSeconds <~ source)
                              ~ rands
                              ~ Window.dimensions

main = lift scene input

{-scatter : (Int,Int) -> Int -> [Int] -> Form -> Form
scatter (w,h) idx rands f =
  let randsf  = map toFloat rands
      (wf,hf) = (toFloat w, toFloat h)
      [a,b]   = drop (idx * 2) randsf |> take 2 
      pos     = (a * wf / 200, b * hf / 200)
  in move pos f

main = collage 400 400 [
    circle 30 |> filled blue |> scatter (400,400) 0 [50,-50,2,50,1,90]
  ]
-}