module TreeLabelType

import Control.Monad.State


data Tree a = Empty | Node (Tree a) a (Tree a)


testTree : Tree String
testTree = Node (Node (Node Empty "Jim" Empty) "Fred"
                      (Node Empty "Sheila" Empty)) "Alice"
                (Node Empty "Bob"
                      (Node Empty "Eve" Empty))

flatten : Tree a -> List a
flatten Empty = []
flatten (Node x y z) = flatten x ++ (y :: flatten z)


treeLabelWith : Tree a -> State (Stream labelType) (Tree (labelType, a))
treeLabelWith Empty = pure Empty
treeLabelWith (Node x y z)
  = do leftNode <- treeLabelWith x
       (this :: rest) <- get
       _ <- put rest
       rightNode <- treeLabelWith z
       pure (Node leftNode (this, y) rightNode)


run : Tree (Integer, String)
run = evalState (treeLabelWith testTree) [10..]


-- ----------------------------------------------------------------------------
-- Exercises
-- ----------------------------------------------------------------------------

update : (stateType -> stateType) -> State stateType ()
update f = do x <- get
              put (f x)

increase : Nat -> State Nat ()
increase n = update (+ n)


countEmpty : Tree a -> State Nat ()
countEmpty Empty = increase 1
countEmpty (Node x y z) = do countEmpty x
                             countEmpty z


countEmptyNode : Tree a -> State (Nat, Nat) ()
countEmptyNode Empty = do (e, n) <- get
                          put (e+1, n)
countEmptyNode (Node x y z) = do countEmptyNode x
                                 countEmptyNode z
                                 (e, n) <- get
                                 put (e, n+1)
