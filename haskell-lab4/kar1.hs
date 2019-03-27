data MyType = C1 Int | C2 Double Bool

instance Eq MyType where
    (==) (C1 x) (C1 y)     = x == y
    (==) (C1 x) (C2 y _)   = (fromIntegral x) == y
    (==) (C2 x _) (C2 y _) = x == y

data BinTree a = NodeBT (BinTree a) (BinTree a) | Leaf a
    deriving (Show, Read)

t1 = NodeBT (Leaf 1) (NodeBT (Leaf 2) (Leaf 3))
t2 = Leaf True

mapBT :: (a -> b) -> BinTree a -> BinTree b
mapBT f (Leaf l) = Leaf (f l)
mapBT f (NodeBT lt rt) = NodeBT (mapBT f lt) (mapBT f rt)
