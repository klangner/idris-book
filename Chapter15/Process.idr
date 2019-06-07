module Process

import System.Concurrency.Channels


data MessagePID = MkMessage PID

data Message = Add Nat Nat


data Process : Type -> Type where
  Action : IO a -> Process a
  Spawn : Process () -> Process (Maybe MessagePID)
  Request : MessagePID -> Message -> Process (Maybe Nat)
  Respond : ((msg : Message) -> Process Nat) -> Process (Maybe Message)

  Pure : a -> Process a
  (>>=) : Process a -> (a -> Process b) -> Process b


run : Process t -> IO t
run (Action action) = action
run (Spawn proc) = do Just pid <- spawn (run proc)
                           | Nothing => pure Nothing
                      pure (Just (MkMessage pid))
run (Request (MkMessage pid) msg)
    = do Just chan <- connect pid
              | Nothing => pure Nothing
         ok <- unsafeSend chan msg
         if ok then do Just answer <- unsafeRecv Nat chan
                            | Nothing => pure Nothing
                       pure (Just answer)
               else pure Nothing
run (Respond calc) = do Just sender <- listen 1
                             | Nothing => pure Nothing
                        Just msg <- unsafeRecv Message sender
                             | Nothing => pure Nothing
                        res <- run (calc msg)
                        unsafeSend sender res
                        pure (Just msg)
run (Pure x) = pure x
run (x >>= f) = do x' <- run x
                   run (f x')


procAdder : Process ()
procAdder = do Respond (\msg => case msg of
                                Add x y => Pure (x + y))
               procAdder


procMain : Process ()
procMain = do Just adder_pid <- Spawn procAdder
                  | Nothing => Action (putStrLn "Spawn failed")
              Just answer <- Request adder_pid (Add 2 3)
                  | Nothing => Action (putStrLn "Request failed")
              Action (printLn answer)
