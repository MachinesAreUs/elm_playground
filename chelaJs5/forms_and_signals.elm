welcome = [markdown|
# Hola Chela JS !!!
Gracias por acompañarnos! ^_^
|]

logo = image 200 200 "./img/chelajs.png"

stack = flow down [welcome, logo]

composition w = collage 300 300 
  [ ngon 5 100 |> filled yellow |> rotate -w
  , circle 100 |> outlined (dashed red) 
  , stack |> toForm |> rotate w |> scale 0.7
  ]

time  = foldp (+) 0 (fps 20)
theta = lift (\t -> pi * t / 5000) time

main = lift composition theta
