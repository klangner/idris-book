module InfList

import Data.Vect


data InfList : Type -> Type where
  (::) : (value : elem) -> Inf (InfList elem) -> InfList elem

%name InfList xs, ys, zs


total
countFrom : Integer -> InfList Integer
countFrom n = n :: countFrom (n + 1)


getPrefix : (count : Nat) -> InfList a -> List a
getPrefix Z xs = []
getPrefix (S k) (value :: xs) = value :: getPrefix k xs
