module Chapter8


-- 8.1

same_cons : {xs : List a} -> {ys : List a} -> xs = ys -> x :: xs = x :: ys
same_cons = cong



same_list : {xs : List a} -> {ys : List a} -> x = y -> xs = ys -> x :: xs = y :: ys
same_list Refl Refl = Refl

data ThreeEq : a -> b -> c -> Type where
  Same : (x : a) -> ThreeEq x x x


allSameS : (x, y, z : Nat) -> ThreeEq x y z -> ThreeEq (S x) (S y) (S z)
allSameS n n n (Same n) = Same (S n)

-- 8.2

|||
||| ```idris example
||| concat [[1,2,3], [4,5,6]]
||| ```
myPlusCommutes : (n : Nat) -> (m : Nat) -> n + m = m + n
myPlusCommutes Z m = rewrite (plusZeroRightNeutral m) in Refl
myPlusCommutes (S k) m = rewrite myPlusCommutes k m in
                         rewrite plusSuccRightSucc m k in Refl
