import Data.Foldable

isPalindrome :: [Char] -> Bool
isPalindrome [] = True
isPalindrome [x] = True
isPalindrome (x:xs)
  | x == (last xs) = isPalindrome (init xs)
  | otherwise = False

--isPrime :: Integral t => t -> Bool
--isPrime n = [i | i <- [2..n-1], n `mod` i == 0] == []

primes :: [Int]
primes = eratoSieve [2..]
 where
   eratoSieve :: [Int] -> [Int]
   eratoSieve (p : xs) = p : eratoSieve [x | x <- xs, x `mod` p /= 0]

--elemIncreasing :: Int -> [Int] -> Bool
elemIncreasing x l = find (>= x) l == Just x

isPrime :: Int -> Bool
isPrime n = elemIncreasing n primes
