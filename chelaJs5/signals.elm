import Signal exposing((<~))
import Mouse

main = show <~ Mouse.position
