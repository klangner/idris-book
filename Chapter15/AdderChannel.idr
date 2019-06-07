module AdderChannel

import System.Concurrency.Channels


data Message = Add Nat Nat


adder : IO ()
adder = do Just sender_chan <- listen 1
                | Nothing => adder
           Just msg <- unsafeRecv Message sender_chan
                | Nothing => adder
           case msg of
                Add x y => do ok <- unsafeSend sender_chan (x + y)
                              adder


main : IO ()
main = do Just adder_pid <- spawn adder
               | Nothing => putStrLn "Can't spawn adder"
          Just chan <- connect adder_pid
               | Nothing => putStrLn "Can't connect to adder"
          ok <- unsafeSend chan (Add 2 3)
          Just answer <- unsafeRecv Nat chan
               | Nothing => putStrLn "No response"
          printLn answer