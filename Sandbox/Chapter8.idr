module Chapter8


same_cons : {xs : List a} -> {ys : List a} -> xs = ys -> x :: xs = x :: ys
same_cons = cong



same_list : {xs : List a} -> {ys : List a} -> x = y -> xs = ys -> x :: xs = y :: ys
same_list Refl Refl = Refl

data ThreeEq : a -> b -> c -> Type where
  Same : (x : a) -> ThreeEq x x x


allSameS : (x, y, z : Nat) -> ThreeEq x y z -> ThreeEq (S x) (S y) (S z)
allSameS n n n (Same n) = Same (S n)
