module Chapter9

import Data.Vect


data MyElem : a -> Vect k a -> Type where
  MyHere  : MyElem x (x :: xs)
  MyThere : (later : MyElem x xs) -> MyElem x (y::xs)


firstInVect : MyElem 1 [1, 2, 3, 4]
firstInVect = MyHere

thirdInVect : MyElem 3 [1, 2, 3, 4]
thirdInVect = MyThere (MyThere MyHere)


removeElem : (value : a) ->
             (xs : Vect (S k) a) ->
             {auto prf : Elem value xs} ->
             Vect k a
removeElem value (value :: xs) {prf = Here} = xs
removeElem value {k = Z} (x :: []) {prf = There later} = absurd later
removeElem value {k = S n} (x :: xs) {prf = There later} = x :: removeElem value xs


notInNill : Elem value [] -> Void
notInNill Here impossible
notInNill (There _) impossible


notInTail : (notThere : Elem value xs -> Void) ->
            (notHere : (value = x) -> Void) ->
            Elem value (x :: xs) ->
            Void
notInTail notThere notHere Here = notHere Refl
notInTail notThere notHere (There later) = notThere later


isMyElem : DecEq a => (value : a) -> (xs : Vect n a) -> Dec (Elem value xs)
isMyElem value [] = No notInNill
isMyElem value (x :: xs) = case decEq value x of
                            Yes Refl => Yes Here
                            No notHere => case isMyElem value xs of
                              Yes prf => Yes (There prf)
                              No notThere => No (notInTail notThere notHere)
