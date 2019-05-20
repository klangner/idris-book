module Hangman

import Data.Vect


total
removeElem : (value : a) -> (xs : Vect (S n) a) ->
             {auto prf : Elem value xs} ->
             Vect n a
removeElem value (value :: ys) {prf = Here} = ys
removeElem {n = Z} value (y :: []) {prf = There later} = absurd later
removeElem {n = (S k)} value (y :: ys) {prf = There later}
                                          = y :: removeElem value ys


data WordState : (guesses : Nat) -> (missing : Nat) -> Type where
  MkWordState : (word : String) ->                -- Word we try to guess
                (missing : Vect letters Char) ->  -- What is still missing
                WordState guesses letters


data Finished : Type where
  Lost : (game : WordState 0 (S letters)) -> Finished
  Won  : (game : WordState (S guesses) 0) -> Finished


data ValidInput : List Char -> Type where
  Letter : (c : Char) -> ValidInput [c]


isValidNil : ValidInput [] -> Void
isValidNil (Letter _) impossible

isValidTwo : ValidInput (x :: (y :: xs)) -> Void
isValidTwo (Letter _) impossible

isValidInput : (cs : List Char) -> Dec (ValidInput cs)
isValidInput [] = No isValidNil
isValidInput [x] = Yes (Letter x)
isValidInput (x :: ( y :: xs)) = No isValidTwo


isValidString : (s : String) -> Dec (ValidInput (unpack s))
isValidString s = isValidInput (unpack s)


readGuess : IO (x ** ValidInput x)
readGuess = do putStr "Guess: "
               x <- getLine
               case isValidString (toUpper x) of
                 Yes prf => pure (_ ** prf)
                 No contra => do putStr "Invalid guess\n"
                                 readGuess


processGuess : (letter : Char) ->
               WordState (S guesses) (S letters) ->
               Either (WordState guesses (S letters))
                      (WordState (S guesses) letters)
processGuess letter (MkWordState word missing)
    = case isElem letter missing of
      Yes prf => Right (MkWordState word (removeElem letter missing))
      No contra => Left (MkWordState word missing)


game : WordState (S guesses) (S letters) -> IO Finished
game {guesses} {letters} st
    = do (_ ** Letter letter) <- readGuess
         case processGuess letter st of
           Left l => do putStrLn "Wrong!"
                        case guesses of
                          Z => pure (Lost l)
                          (S k) => game l
           Right r => do putStrLn "Right!"
                         case letters of
                           Z => pure (Won r)
                           (S k) => game r


main : IO ()
main = do result <- game {guesses=2} (MkWordState "test" ['T', 'E', 'S'])
          case result of
            Won (MkWordState word letters) =>
              putStrLn "You won!"
            Lost (MkWordState word letters) =>
              putStrLn ("You lost! the word was " ++ word)
