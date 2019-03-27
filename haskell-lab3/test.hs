sumWith :: Num a => (a -> a) -> [a] -> a
sumWith f [] = 0
sumWith f (x:xs) = f x + (sumWith f xs)

sumSqr :: Num a => [a] -> a
sumSqr x = sumWith (\e -> e ^ 2) x

sumAbs :: (Num a, Ord a) => [a] -> a
sumAbs x = sumWith (\e -> if e < 0 then (-e) else e) x

