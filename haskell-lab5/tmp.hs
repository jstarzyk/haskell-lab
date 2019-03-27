data MyType a b = C1 a | C2 b

instance (Eq a, Eq b) => Eq (MyType a b) where
  (==) (C1 a) (C1 b) = a == b
  (==) (C2 a) (C2 b) = a == b
  (==) _ _ = False

