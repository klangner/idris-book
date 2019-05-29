module Exercises


-- ----------------------------------------------------------------------------
-- Exercise 2
-- ----------------------------------------------------------------------------
data GuessCmd : Type -> Nat -> Nat -> Type where
  Try : Integer -> GuessCmd Ordering (S state) state

  Pure : ty -> GuessCmd ty state state
  (>>=) : GuessCmd a state1 state2 ->
          (a -> GuessCmd b state2 state3) ->
          GuessCmd b state1 state3


threeGuesses: GuessCmd () 3 0
threeGuesses = do Try 10
                  Try 20
                  Try 15
                  Pure ()


-- This shouldn't type check
-- noGuesses : GuessCmd () 0 0
-- noGuesses = do Try 10
--                Pure ()


-- ----------------------------------------------------------------------------
-- Exercise 3
-- ----------------------------------------------------------------------------
data Matter = Solid | Liquid | Gas

data MatterCmd : Type -> Matter -> Matter -> Type where
  Melt      : MatterCmd () Solid Liquid
  Boil      : MatterCmd () Liquid Gas
  Condense  : MatterCmd () Gas Liquid
  Freeze    : MatterCmd () Liquid Solid

namespace MatterCmdDo
  Pure  : ty -> MatterCmd () state state
  (>>=) : MatterCmd a state1 state2 ->
          (a -> MatterCmd b state2 state3) ->
          MatterCmd b state1 state3


iceSteam : MatterCmd () Solid Gas
iceSteam = do Melt
              Boil

steamIce : MatterCmd () Gas Solid
steamIce = do Condense
              Freeze

-- The following definition should not type-check:
-- overMelt : MatterCmd () Solid Gas
-- overMelt = do Melt
--               Melt
