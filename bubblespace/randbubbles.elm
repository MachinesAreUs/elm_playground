import Window exposing (..)
import Random exposing (..)
import Signal exposing (..)
import Text exposing (..)
import Time exposing (..)
import List exposing (..)
import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Collage exposing (..)

radius      = 30
lineWidth   = 3
bubbleAlpha = 0.4

-- Util

(!!) list idx = drop idx list |> head

text = 
  Graphics.Collage.text << (Text.color white) << fromString << toString

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
    , bubble radius (rands !! 2) red    |> move (-50, 50)
  ]-- |> color black

type alias Input = { ttime: Time, delta: Time, rands: List Int, dim: (Int,Int) }

input = 
  let source = fps 30
      ttime  = foldp (+) 0 source
      rands  = constant <| List.map (\_ -> generate (int 0 100) (initialSeed 123) |> fst) [1..10] 
  in sampleOn ttime <| Input <~ ttime
                              ~ (inSeconds <~ source)
                              ~ rands
                              ~ Window.dimensions

main = Signal.map scene input

