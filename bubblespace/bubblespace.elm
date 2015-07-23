import Window exposing (..)
import Random exposing (..)
import Signal exposing (..)
import Text exposing (..)
import Time exposing (..)
import List exposing (..)
import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)

radius      = 30
lineWidth   = 3
bubbleAlpha = 0.4

-- Util

(!!) list idx = drop idx list |> head

text = 
  Graphics.Collage.text << (Text.color white) << fromString << toString

-- This function was removed form Signal package
-- See: https://github.com/elm-lang/core/commit/e7c5c4c57850867bf46ac267cde091b391fefb26

combine : List (Signal a) -> Signal (List a)
combine = List.foldr (Signal.map2 (::)) (constant [])

-- Program

scatter : (Int,Int) -> Int -> List Int -> Form -> Form
scatter (w,h) idx rands f =
  let (wf,hf) = (toFloat w, toFloat h)
      randsf  = List.map toFloat rands
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
      unpositionedBubbles  = List.map toBubble colors
      indexedBubbles       = List.map2 (,) unpositionedBubbles [0..10]
      --bubbles              = List.map (\(b,idx) -> scatter dim idx rands b) indexedBubbles
  in collage w h 
    unpositionedBubbles
    |> Graphics.Element.color black

type alias Input = { ttime: Time, delta: Time, rands: List Int, dim: (Int,Int) }

input = 
  let source     = fps 1
      ttime      = foldp (+) 0 source
      seed t     = initialSeed(floor(t))
      randSignal = Signal.map (\s -> generate (int 0 100) (seed s) |> fst) ttime
      rands      = combine <| List.map (\_ -> randSignal) [1..10]
  in sampleOn ttime <| Input <~ ttime
                              ~ (inSeconds <~ source)
                              ~ rands
                              ~ Window.dimensions

main = Signal.map scene input
