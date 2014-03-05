import Keyboard

elm = [markdown|
# ¿Qué es Elm?
- Lenguaje orientado a gráficas e interfaces.
- Implementa el modelo de Programación Funcional Reactiva
- Basado en Haskell (subset)
- Implementado en Haskell
- Compila a js + html
- ¡Pura diversión! 
|]

fp = [markdown|
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
|]

features = [markdown|
# Características del lenguaje.
- Tipado estático e inferencia de tipos.
- Todo son expresiones. (No instrucciones)
- Las listas son muy importantes.
- Pattern matching.
- Las señales encapsulan secuencias de valores.
|]

missing = [markdown|
# Cosas que no importa de Haskell
- Evaluación perezosa.
- List comprehensions.
- Sentencias 'where'.
- Guardas.
- Notación 'do' o 'proc' para encapsular efectos colaterales.
|]

code = [markdown|
# Dígaloooo cooon códigooooo!!!

    quicksort xs = 
      let lesser  h = filter (\x -> x < h) 
          greater h = filter (\x -> x >= h)
      in case xs of
         []     -> []
         hd::tl -> (quicksort (lesser hd tl)) ++ 
                   [hd] ++ 
                   (quicksort (greater hd tl))
|]

welcome_md = [markdown|
# Me caga JS !!!
Gracias por acompañarnos! ^_^
|]

elems_and_layouts = 
  let logo  = image 180 180 "./img/chelajs.png"
      stack = flow down [welcome_md, logo]
      composition = collage 250 250
        [ ngon 7 100 |> filled yellow   
        , circle 100 |> outlined (dashed red) 
        , stack |> toForm |> rotate (degrees 30) |> scale 0.7
        ]
      title = [markdown|# Elements and Layouts|]
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

scene n = flow right [ sidebar, drop n pages |> head]

toPage arrows previousPage = 
  let newPage = arrows.x + previousPage
  in if | newPage <= 0  -> 0
        | otherwise -> min newPage (length pages - 1)

currPage = foldp toPage 0 Keyboard.arrows

main = lift scene currPage
