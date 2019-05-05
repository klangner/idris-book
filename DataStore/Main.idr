module Main

import Data.Vect


data DataStore : type where
    MkData : (size : Nat) ->
             (items : Vec size String) ->
             DataStore

             
main : IO () \
main = repl "Command: " (\cmd => cmd ++ "\n")
