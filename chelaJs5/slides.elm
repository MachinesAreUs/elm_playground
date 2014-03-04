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
- Guardas
- Notación 'do' o 'proc' para encapsular efectos colaterales.
|]

code = [markdown|
# Dígaloooo cooon códigooooo!!!

    quicksort l = 
      let h       = head l
          xs      = tail l
          lesser  = filter (\x -> x < h) xs
          greater = filter (\x -> x >= h) xs
      in if |(length l) == 0 -> []
            |otherwise       -> (quicksort lesser) ++ [h] ++ (quicksort greater)
|]

pages = [ elm, fp, features, missing, code ]

logo = image 200 200 "./img/elm.png"

navigation = collage 200 30
  [ image 50 30 "./img/arrow_left.png"  |> toForm |> move (-40,0)
  , image 50 30 "./img/arrow_right.png" |> toForm |> move (40,0)
  ]

sidebar = flow down [logo, navigation]

scene n = flow right [ sidebar, drop n pages |> head]

toPage k n = 
  let new = k.x + n
  in if | new <= 0  -> 0
        | otherwise -> min new (length pages - 1)

currPage = foldp toPage 0 Keyboard.arrows

main = lift scene currPage
