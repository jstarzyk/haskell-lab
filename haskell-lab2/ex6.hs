fib :: (Num a, Eq a) => a -> a
fib n =
 if n == 0 || n == 1 then n
 else fib (n - 2) + fib (n - 1)

sum' :: Num a => [a] -> a
sum' []   = 0
sum' (x:xs) = x + sum' xs

prod' :: Num a => [a] -> a -- prod' [1,2,3] = 6
prod' [] = 0
prod' [x] = x
prod' (x:xs) = x * prod' xs

--length' :: [a] -> Int -- length' [1,1,1,1] = 4
--or' :: [Bool] -> Bool -- or' [True, False, True] = True
--and' :: [Bool] -> Bool -- and' [True, False, True] = False
--elem' :: Eq a => a -> [a] -> Bool -- elem' 3 [1,2,3] = True
--doubleAll :: Num t => [t] -> [t] -- double doubleAll [1,2] = [2,4]
--squareAll :: Num t => [t] -> [t] -- double squareAll [2,3] = [4,9]
--selectEven :: Integral t => [t] -> [t] -- selectEven [1,2,3] = [2]

sum'2 :: Num a => [a] -> a
sum'2 xs = loop 0 xs
 where loop acc []     = acc
       loop acc (x:xs) = loop (x + acc) xs

sum'3 :: Num a => [a] -> a
sum'3 = loop 0
 where loop acc []     = acc
       loop acc (x:xs) = loop (x + acc) xs

prod'2 :: Num a => [a] -> a
prod'2 xs = loop 1 xs
  where loop acc []     = acc
        loop acc (x:xs) = loop (x * acc) xs

length'2 :: [a] -> Int
length'2 xs = loop 0 xs
  where loop acc []     = acc
        loop acc (x:xs) = loop (1 + acc) xs




