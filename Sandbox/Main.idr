module Main

import Data.Vect 

test : Vect m a -> Vect n a -> Vect (m+n) a
test xs ys = xs ++ ys

safeZip : Vect n a -> Vect m b -> Vect (minimum n m) (a, b)
safeZip [] ys           = []
safeZip (x::xs) []      = []
safeZip (x::xs) (y::ys) = (x, y) :: safeZip xs ys


main : IO ()
main = repl "Enter a string: " id
