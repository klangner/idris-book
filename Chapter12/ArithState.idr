module ArithState


-- Game State

record Score where
  constructor MkScore
  correct : Nat
  attempted : Nat


record GameState where
  constructor MkGameState
  score : Score
  difficulty : Nat

Show GameState where
  show st = show (correct (score st)) ++ "/" ++
            show (attempted (score st)) ++ "\n" ++
            "Difficulty: " ++ show (difficulty st)


initState : GameState
initState = MkGameState (MkScore 0 0) 12


setDifficulty : Nat -> GameState -> GameState
setDifficulty level = record { difficulty = level }


addWrong : GameState -> GameState
addWrong state = record { score->attempted = attempted (score state) + 1 } state


addCorrect : GameState -> GameState
addCorrect = record { score->correct $= (+ 1), score->attempted $= (+ 1) }

-- Running quiz

data Command : Type -> Type where
  PutStr : String -> Command ()
  GetLine : Command String

  GetRandom : Command Int
  GetGameState : Command GameState
  PutGameState : GameState -> Command ()

  Pure : ty -> Command ty
  Bind : Command a -> (a -> Command b) -> Command b


data ConsoleIO : Type -> Type where
  Quit : a -> ConsoleIO a
  Do : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b


namespace CommandDo
  (>>=) : Command a -> (a -> Command b) -> Command b
  (>>=) = Bind

namespace ConsoleDo
  (>>=) : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b
  (>>=) = Do


data Fuel = Dry | More (Lazy Fuel)

partial
forever : Fuel
forever = More forever


randoms : Int -> Stream Int
randoms seed = let seed' = 1664525 * seed + 1013904223 in
                   (seed' `shiftR` 2) :: randoms seed'


runCommand : Command a -> IO a
runCommand (PutStr x) = putStr x
runCommand GetLine = getLine
runCommand GetRandom = ?hole_1
runCommand GetGameState = ?hole_2
runCommand (PutGameState x) = ?hole_3
runCommand (Pure x) = pure x
runCommand (Bind x f) = do res <- runCommand x
                           runCommand (f res)


run : Fuel -> ConsoleIO a -> IO (Maybe a)
run fuel (Quit val) = do pure (Just val)
run (More fuel) (Do cmd f) = do res <- runCommand cmd
                                run fuel (f res)
run Dry _ = pure Nothing


mutual
  correct : ConsoleIO GameState
  correct = do PutStr "Correct!\n"
               st <- GetGameState
               PutGameState (addCorrect st)
               quiz

  wrong : Int -> ConsoleIO GameState
  wrong ans
        = do PutStr ("Wrong, the answer is " ++ show ans ++ "\n")
             st <- GetGameState
             PutGameState (addWrong st)
             quiz

  data Input = Answer Int
             | QuitCmd

  readInput : (prompt : String) -> Command Input
  readInput prompt
     = do PutStr prompt
          answer <- GetLine
          if toLower answer == "quit"
             then Pure QuitCmd
             else Pure (Answer (cast answer))


  quiz : ConsoleIO GameState
  quiz = do num1 <- GetRandom
            num2 <- GetRandom
            st <- GetGameState
