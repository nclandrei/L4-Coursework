-- defining Tree data type
data Tree a = Leaf | Node a (Tree a) (Tree a) deriving (Read, Show, Eq, Ord)

-- function sigantures for tree zipping and unzipping
treezip :: (Tree a) -> (Tree b) -> (Tree (a,b))
treeunzip :: (Tree (a,b)) -> (Tree a, Tree b)

-- implementation for tree zipping - including corner cases
treezip Leaf Leaf = Leaf
treezip (Node _ _ _) Leaf = Leaf
treezip Leaf (Node _ _ _) = Leaf
treezip (Node a l1 r1) (Node b l2 r2) = 
    let l = treezip l1 l2
        r = treezip r1 r2
    in Node (a,b) l r

-- implementation for tree unzipping
treeunzip Leaf = (Leaf, Leaf)
treeunzip (Node (a,b) l r) = 
    let (l1, l2) = treeunzip l
        (r1, r2) = treeunzip r
    in (Node a l1 r1, Node b l2 r2)

