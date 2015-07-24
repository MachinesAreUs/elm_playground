import Markdown
import Graphics.Element exposing (..)

welcome = Markdown.toElement """
# Hola Chela JS !!!
Gracias por acompañarnos! ^_^

Salud!
"""

logo = image 200 200 "./img/chelajs.png"

main = flow down [welcome, logo]
