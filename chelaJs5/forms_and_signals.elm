import Graphics.Element exposing(..)
import Graphics.Collage exposing(..)
import Color exposing(..)
import Signal exposing(..)
import Time exposing(..)
import Markdown

welcome = Markdown.toElement """
# Hola Chela JS !!!
Gracias por acompañarnos! ^_^
"""

logo = image 200 200 "./img/chelajs.png"

stack = flow down [welcome, logo]

composition w = collage 300 300 
  [ ngon 5 100 |> filled yellow |> rotate -w
  , circle 100 |> outlined (dashed red) 
  , stack |> toForm |> rotate w |> scale 0.7
  ]

time  = foldp (+) 0 (fps 20)
theta = (\t -> pi * t / 5000) <~ time

main = composition <~theta
