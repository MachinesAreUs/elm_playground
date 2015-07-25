import Window exposing (..)
import Random exposing (..)
import Signal exposing (foldp, sampleOn, (<~), (~))
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

combine = foldr (Signal.map2 (::)) (Signal.constant [])

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

scene {ttime, delta, randSeed, dim} =
  let (w,h)               = dim
      toBubble            = bubble radius
      colors              = [red, orange, yellow, green, blue, purple]
      unpositionedBubbles = map toBubble colors
      indexedBubbles      = map2 (,) unpositionedBubbles [0..10]
      bubbles             = map (\(b,idx) -> scatter dim idx rands b) indexedBubbles
      rands'              = map (\_ -> generate (int -1 100) randSeed) [1..10] 
      rands               = map fst rands'
  in collage w h 
    --unpositionedBubbles
    bubbles
    |> Graphics.Element.color black

type alias Input = { ttime: Time, delta: Time, randSeed: Seed, dim: (Int,Int) }

input = 
  let source     = fps 1
      ttime      = foldp (+) 0 source
  in sampleOn ttime <| Input <~ ttime
                              ~ (inSeconds <~ source)
                              ~ (initialSeed <~ (floor <~ ttime))
                              ~ Window.dimensions

main = scene <~ input
