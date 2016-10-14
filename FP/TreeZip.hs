data Tree a = Leaf | Node a (Tree a) (Tree a) deriving (Read, Show, Eq, Ord)

treezip :: (Tree a) -> (Tree b) -> (Tree (a,b))
treeunzip :: (Tree (a,b)) -> (Tree a, Tree b)

treezip Leaf Leaf = Leaf
treezip (Node _ _ _) Leaf = Leaf
treezip Leaf (Node _ _ _) = Leaf
treezip (Node a l1 r1) (Node b l2 r2) = 
    let l = treezip l1 l2
        r = treezip r1 r2
    in Node (a,b) l r

treeunzip Leaf = (Leaf, Leaf)

