import Data.List

-- fun with prime numbers!
-- also some list sorting and merging going on here.

isPrime :: Integer -> Bool
isPrime 1 = False
isPrime x | x > 1 = checkPrime x 2

checkPrime :: Integer -> Integer -> Bool
checkPrime x n | x < n*n = True
checkPrime x 2 | mod x 2 == 0 = False
checkPrime x 2 | mod x 2 == 1 = checkPrime x 3
checkPrime x n | mod x n == 0 = False
checkPrime x n | mod x n > 0 = checkPrime x (n+2)

cntPrimes :: Integer -> Integer
cntPrimes x
  | x==1 = 0
  | x==2 = 1
  | x==3 = 2
  | mod x 2 == 0 = cntPrimes (x-1)
  | mod x 2 == 1 && isPrime x = 1 + cntPrimes (x-2)
  | otherwise = cntPrimes (x-2)
  
oddPspTO :: Integer -> Integer -> [Integer]
--oddPspTO a upb = [n | n <- [3,5..upb], mod (a^(n-1)) n == 1, not (isPrime n)]
oddPspTO a upb = [n | n <- [3,5..upb], expmod a (n-1) n == 1, not (isPrime n)]

expmod :: Integer -> Integer -> Integer -> Integer
expmod a e n
  | e==1 = mod a n
  | a>n = expmod (mod a n) e n
  | mod e 2 == 0 = expmod (a*a) (div e 2) n
  | mod e 2 == 1 = mod ((expmod (a*a) (div (e-1) 2) n) * a) n
  
numberOfPsp :: Integer -> Integer -> Int
numberOfPsp x y = length(oddPspTO x y)

order :: Integer -> Integer -> Integer
order a p = ord a (mod a p) 1 p
  where ord a e k p = if e == 1 then k else ord a (mod (a*e) p) (k+1) p

primeFactors :: Integer -> [Integer]
primeFactors a = [ n | n <- 2:[3,5..a], mod a n == 0, isPrime n]

sortUniq :: Ord a => [a] -> [a]
sortUniq [] = []
sortUniq [x] = [x]
sortUniq (p:xs) = sortUniq smaller ++ [p] ++ sortUniq larger
  where 
    smaller = [x | x <- xs, x < p]
    larger = [x | x <- xs, x > p]

primeMult :: [Integer] -> [Integer]
primeMult []  = []
primeMult (x:[]) = [x]
primeMult (x:y:[]) = [x]++[y]++[x*y]
primeMult (x:y:z) = [x*y]++primeMult (x:z)++primeMult (y:z)

orders :: Integer -> Integer -> [Integer]
orders a p = [e | e <- sortUniq (primeMult (primeFactors (p-1))), expmod a e p == 1]

order2 :: [Integer] -> Integer
order2 [] = 0
order2 (x:[]) = x
order2 (x:xs) = x


oddPspTO2 :: Integer -> Integer -> [Integer]
oddPspTO2 a upb = [n | n <- try a upb, expmod a (n-1) n == 1, not (isPrime n)]


try :: Integer -> Integer -> [Integer]
try a upb = mergeLists (tryList a upb)

mergeLists :: [[Integer]] -> [Integer]
mergeLists [] = []
mergeLists(x:xs) = merge x (mergeLists xs)

merge :: [Integer] -> [Integer] -> [Integer]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  | x < y     = x : (merge xs (y:ys))
  | x == y    = x : (merge xs ys)
  | otherwise = y : (merge (x:xs) ys)

tryList :: Integer -> Integer -> [[Integer]]
tryList a upb = [candidate a p upb|p <- takeWhile (< intsq(upb)) primes]

intsq :: Integer -> Integer
intsq = ceiling . sqrt . fromIntegral

candidate :: Integer -> Integer -> Integer -> [Integer]
candidate a p upb
  | (order2 (orders a p)) == 0 && odd (order a p) = [(((order a p)* p * 2) + p),(((order a p)* p * 4 + p)) .. upb]
  | (order2 (orders a p)) == 0 && even (order a p) = [(((order a p) * p) + p),(((order a p) * p * 2) + p) .. upb]
  | odd (order2 (orders a p)) = [(((order2 (orders a p)) * p * 2) + p),(((order2 (orders a p)) * p * 4) + p) .. upb]
  | otherwise = [(((order2 (orders a p)) * p) + p),(((order2 (orders a p)) * p * 2) + p) .. upb]

primes :: [Integer]
primes = sieve [ 3, 5..]
  where sieve (p:x) = p : sieve [n | n <- x, mod n p > 0]