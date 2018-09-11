type Pos = (Int, Int)
data Direction = North | South | East | West

move :: Direction -> Pos -> Pos
move North (x,y) = (x, y+1)
move West  (x,y) = (x-1, y)
move South (x,y) = (x, y-1)
move East  (x,y) = (x+1, y)

moves :: [Direction] -> Pos -> Pos
moves [x] p = move x p
moves (x:xs) p = moves xs $ move x p

data Nat = Zero | Succ Nat
    deriving (Eq, Show, Read, Ord)

-- mult :: Nat -> Nat -> Nat
-- ?
-- nat2int :: Nat -> Int
-- ?
-- int2nat :: Int -> Nat
-- ?

-- Every element in the left subtree is <= node value.
data Tree a = Leaf | Node a (Tree a) (Tree a)
    deriving (Eq, Show, Read, Ord)

insert :: Ord i => i -> Tree i -> Tree i
insert i Leaf = Node i Leaf Leaf
insert i (Node a l r)
    | i == a = Node i l r
    | x < a = Node a (insert i l) r
    | x > a = Node a l (insert i r)
