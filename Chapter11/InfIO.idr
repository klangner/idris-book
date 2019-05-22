module InfIO


data InfIO : Type where
  Do : IO a -> (a -> Inf InfIO) -> InfIO


loopPrint : String -> InfIO
loopPrint msg = Do (putStrLn msg) (\_ => loopPrint msg)


run : InfIO -> IO ()
run (Do action next) = do res <- action
                          run (next res)
