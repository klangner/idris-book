module Vending


VendState : Type
VendState = (Nat, Nat)


-- Possible user inputs
data Input = COIN
           | VEND
           | CHANGE
           | REFILL Nat


data MachineCmd : Type -> VendState -> VendState -> Type where
  InsertCoin : MachineCmd () (coins, bars) (S coins, bars)
  Vend       : MachineCmd () (S coins, S bars) (coins, bars)
  GetCoins   : MachineCmd () (coins, bars) (Z, bars)
  Refill     : (amount : Nat) -> MachineCmd () (Z, bars) (Z, bars + amount)
  Display    : String -> MachineCmd () state state
  GetInput   : MachineCmd (Maybe Input) state state

  Pure       : ty -> MachineCmd () state state
  (>>=)      : MachineCmd a state1 state2 ->
               (a -> MachineCmd b state2 state3) ->
               MachineCmd b state1 state3

data MachineIO : VendState -> Type where
  Do : MachineCmd a state1 state2 ->
       (a -> Inf (MachineIO state2)) -> MachineIO state1

namespace MachineDo
  (>>=) : MachineCmd a state1 state2 ->
       (a -> Inf (MachineIO state2)) -> MachineIO state1
  (>>=) = Do
