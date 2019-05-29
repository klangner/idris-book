module Stack

import Data.Vect


data StackCmd : Type -> Nat -> Nat -> Type where
  Push : Integer -> StackCmd () size (S size)
  Pop  : StackCmd Integer (S size) size
  Top  : StackCmd Integer (S size) (S size)

  Pure : ty -> StackCmd ty size size
  (>>=): StackCmd a size1 size2 ->
         (a -> StackCmd b size2 size3) ->
         StackCmd b size1 size3


testAdd : StackCmd Integer 0 0
testAdd = do Push 10
             Push 20
             val1 <- Pop
             val2 <- Pop
             Pure (val1 + val2)


runStack : (Vect inSize Integer) ->
           StackCmd ty inSize outSize ->
           (ty, Vect outSize Integer)
runStack xs (Push x) = ((), x :: xs)
runStack (x::xs) Pop = (x, xs)
runStack (x::xs) Top = (x, x::xs)
runStack xs (Pure x) = (x, xs)
runStack xs (f >>= g) = let (x, st) = runStack xs f
                        in runStack st (g x)


doAdd : StackCmd () (S (S size)) (S size)
doAdd = do x <- Pop
           y <- Pop
           Push (x+y)
