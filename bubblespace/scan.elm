import Graphics.Element exposing(..)
import Signal exposing ((<~))

scanr             : (a -> b -> b) -> b -> List a -> List b
scanr f q0 list = 
  case list of 
    []     -> [q0]
    x::xs  -> let qs@(q::_) = scanr f q0 xs
              in f x q :: qs

main = 
  let values = scanr (+) 0 [1..10]
  in show <~ values
