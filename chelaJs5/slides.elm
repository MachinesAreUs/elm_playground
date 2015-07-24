import Signal exposing (foldp,(<~),(~))
import Graphics.Collage exposing(..)
import Graphics.Element exposing(..)
import Color exposing(..)
import List exposing(..)
import Keyboard
import Markdown

elm = Markdown.toElement """
# ¿Qué es Elm?
- Lenguaje orientado a gráficas e interfaces.
- Implementa el modelo de Programación Funcional Reactiva
- Basado en Haskell (subset)
- Implementado en Haskell
- Compila a js + html
- ¡Pura diversión! 
"""

fp = Markdown.toElement """
# Programación Funcional
- Funciones son elementos de primer nivel. Pueden:
    - Declararse "al vuelo".
    - Pasarse como parámetros.
    - Ser devueltas como valores de retorno.
- La funciones son (en su mayoría) puras.
    - Sin efectos colaterales.
    - Transparencia referncial.
- Se evita mantener estado mutable.
    - Cuando no es opción, se encapsula.
- Programación declarativa.
"""

features = Markdown.toElement """
# Características del lenguaje.
- Tipado estático e inferencia de tipos.
- Todo son expresiones. (No instrucciones)
- Las listas son muy importantes.
- Pattern matching.
- Las señales encapsulan secuencias de valores.
"""

missing = Markdown.toElement """
# Cosas que no importa de Haskell
- Evaluación perezosa.
- List comprehensions.
- Sentencias 'where'.
- Guardas.
- Notación 'do' o 'proc' para encapsular efectos colaterales.
"""

code = Markdown.toElement """
# Dígaloooo cooon códigooooo!!!
```elm
    quicksort xs = 
      let lesser  h = filter (\\x -> x < h) 
          greater h = filter (\\x -> x >= h)
      in case xs of
         []     -> []
         hd::tl -> (quicksort (lesser hd tl)) ++ 
                   [hd] ++ 
                   (quicksort (greater hd tl))
```
"""

title = Markdown.toElement "# Elements and Layouts"

welcome_md = Markdown.toElement """
# Me caga JS !!!
Gracias por acompañarnos! ^_^
"""

elems_and_layouts = 
  let logo  = image 180 180 "./img/chelajs.png"
      stack = flow down [welcome_md, logo]
      composition = collage 250 250
        [ ngon 7 100 |> filled yellow   
        , circle 100 |> outlined (dashed red) 
        , stack |> toForm |> rotate (degrees 30) |> scale 0.7
        ]
  in flow down [title, composition]

pages = [ elm
        , fp
        , features
        , missing
        , code
        , elems_and_layouts
        ]

logo = image 160 160 "./img/elm.png"

navigation = collage 160 30
  [ image 50 30 "./img/arrow_left.png"  |> toForm |> move (-40,0)
  , image 50 30 "./img/arrow_right.png" |> toForm |> move (40,0)
  ]

sidebar = flow down [logo, navigation]

heads x = case head x of
          Just e -> e
          _      -> show ""

scene n = flow right [ sidebar, drop n pages |> heads]

toPage arrows previousPage = 
  let newPage = arrows.x + previousPage
  in if | newPage <= 0  -> 0
        | otherwise -> min newPage (length pages - 1)

currPage = foldp toPage 0 Keyboard.arrows

main = scene <~ currPage
