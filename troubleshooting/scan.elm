import Graphics.Element exposing(..)
import List exposing(..)
import Signal exposing ((<~))

fromJust : Maybe a -> a
fromJust (Just x) = x

scanr : (a -> b -> b) -> b -> List a -> List b
scanr f q0 list = 
  case list of 
    []     -> [q0]
    x::xs  -> let qs = scanr f q0 xs
                  q = head qs |> fromJust
              in f x q :: qs

main = scanr (+) 0 [1..10] |> show
   
