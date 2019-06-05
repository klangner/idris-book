module ATM

import Data.Vect


PIN : Type
PIN = Vect 4 Char

data ATMState = Ready | CardInserted | Session

data PinCheck = CorrectPin | IncorrectPin

data HasCard : ATMState -> Type where
  HasCI : HasCard CardInserted
  HasSession : HasCard Session

data ATMCmd : (ty : Type) -> ATMState -> (ty -> ATMState) -> Type where
  InsertCard : ATMCmd () Ready (const CardInserted)
  EjectCard : {auto prf : HasCard state} -> ATMCmd () state (const Ready)
  GetPin : ATMCmd PIN CardInserted (const CardInserted)
  CheckPin : PIN -> ATMCmd PinCheck CardInserted
              (\check => case check of
                            CorrectPin => Session
                            IncorrectPin => CardInserted)
  GetAmount : ATMCmd Nat state (const state)
  Dispense : (amount : Nat) -> ATMCmd () Session (const Session)

  Message : String -> ATMCmd () state (const state)
  Pure : (res : ty) -> ATMCmd ty (state_fn res) state_fn
  (>>=) : ATMCmd a state1 state2_fn ->
           ((res : a) -> ATMCmd b (state2_fn res) state3_fn) ->
           ATMCmd b state1 state3_fn


atm : ATMCmd () Ready (const Ready)
atm = do InsertCard
         pin <- GetPin
         pinOk <- CheckPin pin
         case pinOk of
           CorrectPin => do cash <- GetAmount
                            Dispense cash
                            EjectCard
           IncorrectPin => EjectCard


testPin : PIN
testPin = ['1', '2', '3', '4']


readVect : (n : Nat) -> IO (Vect n Char)
readVect Z = do discard <- getLine -- rest of input up to enter
                pure []
readVect (S k) = do ch <- getChar
                    chs <- readVect k
                    pure (ch :: chs)


runATM : ATMCmd res initState outState_fn -> IO res
runATM InsertCard = do putStrLn "Insert card (Press Enter)"
                       _ <- getLine
                       pure ()
runATM EjectCard = putStrLn "Card Ejected"
runATM GetPin = do putStrLn "Enter PIN"
                   readVect 4
runATM (CheckPin pin) = if pin == testPin
                        then pure CorrectPin
                        else pure IncorrectPin
runATM GetAmount = do putStrLn "How much?"
                      amount <- getLine
                      pure (cast amount)
runATM (Dispense amount) = putStrLn ("Here is: " ++ show amount)
runATM (Message msg) = putStrLn msg
runATM (Pure res) = pure res
runATM (x >>= f) = do x' <- runATM x
                      runATM (f x')


-- badATM : ATMCmd () Ready (const Ready)
-- badATM = EjectCard


insertEject : ATMCmd () Ready (const Ready)
insertEject = do InsertCard
                 EjectCard
