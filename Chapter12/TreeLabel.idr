module TreeLabel


data Tree a = Empty | Node (Tree a) a (Tree a)


testTree : Tree String
testTree = Node (Node (Node Empty "Jim" Empty) "Fred"
                      (Node Empty "Sheila" Empty)) "Alice"
                (Node Empty "Bob"
                      (Node Empty "Eve" Empty))

flatten : Tree a -> List a
flatten Empty = []
flatten (Node x y z) = flatten x ++ (y :: flatten z)


treeLabelWith : Stream labelType -> Tree a ->
                (Stream labelType, Tree (labelType, a))
treeLabelWith lbls Empty = (lbls, Empty)
treeLabelWith lbls (Node x y z)
  = let (lblThis :: lblsLeft, leftNode) = treeLabelWith lbls x
        (lblsRight, rightNode) = treeLabelWith lblsLeft z
    in (lblsRight, Node leftNode (lblThis, y) rightNode)


treeLabel : Tree a -> Tree (Integer, a)
treeLabel tree = snd $ treeLabelWith [1..] tree
