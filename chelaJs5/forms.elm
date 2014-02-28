welcome = [markdown|
# Hola Chela JS !!!
Gracias por acompañarnos! ^_^
|]

logo = image 200 200 "./img/chelajs.png"

stack = flow down [welcome, logo]

composition = collage 300 300 
  [ ngon 5 100 |> filled blue   
  , circle 100 |> outlined (dashed red) 
  , stack |> toForm |> rotate (degrees 30) |> scale 0.7
  ]

main = composition