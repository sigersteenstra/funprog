import Data.List
import Data.Function

--HELPER FUNCTIONS

--returns an infinite list of prime numbers
primes :: [Integer]
primes = 2:sieve [ 3, 5..]
  where sieve (p:x) = p : sieve [n | n <- x, mod n p > 0]

--takes the square root of an integer (rounded up)
intsqrt :: Integer -> Integer
intsqrt = ceiling . sqrt . fromIntegral

--takes out all factors x from n
div2 :: Integer -> Integer -> Integer
div2 n x 
  | mod (div n x) x == 0 = div2 (div n x) x
  | otherwise = div n x

--helper function to merge lists
merge :: [Integer] -> [Integer] -> [Integer]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  | x < y     = x : (merge xs (y:ys))
  | x == y    = x : (merge xs ys)
  | otherwise = y : (merge (x:xs) ys)

--function that determines the primefactors of an integer
primeFactors :: Integer -> [Integer]
primeFactors n = primeFactors2 n primes

primeFactors2 :: Integer -> [Integer] -> [Integer]
primeFactors2 n (p:primes)
  |n <= 1 = []
  |mod n p == 0 = p:(primeFactors2 (div n p) (p:primes))
  |p*p>n = [n]
  |otherwise = primeFactors2 n primes
  
--returns an array with the separate digits from the input integer
giveDigits:: Integer -> [Integer]
giveDigits 0 = []
giveDigits x = giveDigits (x `div` 10) ++ [x `mod` 10]

--the expmod from the previous lab assignment
expmod :: Integer -> Integer -> Integer -> Integer
expmod a e n
  | e==1 = mod a n
  | a>n = expmod (mod a n) e n
  | mod e 2 == 0 = expmod (a*a) (div e 2) n
  | mod e 2 == 1 = mod ((expmod (a*a) (div (e-1) 2) n) * a) n

--gives the factorial of an integer
factorial :: Integer -> Integer
factorial n = foldr (*) 1 [1..n]

--the next 5 functions are helper functions to create 
--an infinite list of palindromic numbers.
createPals:: [Integer]
createPals = 0:concat (map createPals2 [1..])

createPals2:: Integer -> [Integer]
createPals2 n = [read(x)|x <- createPals3 n ['0'..'9'], last x /= '0']
 
createPals3:: (Integral a)=> a -> [Char] -> [String]
createPals3 n chars = concat $ (map (createPals4 n) chars)

createPals4:: (Integral a)=> a -> Char -> [String]
createPals4 n charx
  |n==1 = [[charx]]
  |n==2 = [[charx,charx]]
  |n>2 = map (createPals5 charx) (createPals3 (n-2) ['0'..'9'])
  |otherwise = []

createPals5:: Char -> String -> String
createPals5 sur inf = [sur] ++ inf ++ [sur]


----------------------
----------------------
----LAB2 FUNCTIONS----
----------------------
----------------------

----EXERCISE 1----
smallestMultiple :: Integer -> Integer
smallestMultiple 0 = 0
smallestMultiple n = foldr (lcm) 1 [1..n]


----EXERCISE 2----
mults :: Integer -> [Integer]
mults n
  | n<=0 = [0]
  | n>0 = [x|x <- [n,2*n..]]

multiples :: [Integer] -> [Integer]
multiples [] = []
multiples xs = foldr merge [] (map mults xs)

multSum :: Integer -> [Integer] -> Integer
multSum n xs = foldr (+) 0 (takeWhile (< n) (multiples xs))


----EXERCISE 3----
powers :: Integer -> [Integer]
powers a = [(a^x) | x <- [2..]]

distinctPowers :: Integer -> Int -> Int
distinctPowers m n = length(foldr merge [] (map (take (n-1)) (map powers [2..m])))


----EXERCISE 4----
numberOfPalindromicComposites :: Integer -> Int
numberOfPalindromicComposites n = length [x|x <- takeWhile (< n) createPals, length (primeFactors x) == 2]


----EXERCISE 5----
-- From a number n, take the last d digits.
lastDigits :: Integer -> Int -> [Integer]
lastDigits n d = reverse(take d (reverse (giveDigits (sum [expmod x x (10^d) | x <- [1..n] ]))))


----EXERCISE 6----
--f(n) to yield the sum of the factorials of the digits of n
funf :: Integer -> Integer
funf n = sum(map factorial (giveDigits n))

--sf(n) to yield the sum of the digits of f(n)
funsf :: Integer -> Integer
funsf n = sum(giveDigits (funf n))

--g(i) to yield the smallest positive integer n such that sf(n)=i
fung :: Integer -> Integer
fung 0 = 0
fung i = head[x|x <- [1..], funsf x == i]

--sg(i) to yield the sum of the digits of g(i)
funsg :: Integer -> Integer
funsg 0 = 0
funsg i = sum(giveDigits (fung i))

--sumsg to yield the sum of sg(i) from 1 to n
sumsg :: Integer -> Integer
sumsg 0 = 0
sumsg n = (funsg n) + (sumsg (n-1))



----EXERCISE 8----

--An actual sudoku solver is up next!
--We decided to keep the helper functions below, for readability, as most of 
--them are too specific to be actual helper functions for other purposes.

--returns whether the given sudoku has 1, and only 1 solution
isCorrectSudoku:: [String] -> Bool
isCorrectSudoku sud 
  | length(take 10 (solveSudoku sud))>9 || length(solveSudoku sud)==0 = False
  | otherwise = True

--returns the number of solutions to a sudoku
--this may take a while if you enter an entirely empty sudoku...
numberOfSolns:: [String] -> Int
numberOfSolns sud = div (length (solveSudoku sud)) 9

--solves sudoku by calling helper function
solveSudoku:: [String] -> [String]
solveSudoku sud = cleanUp (solveSud1 sud 0)

--helper function to solve sudoku
solveSud1:: [String] -> Int -> [String]
solveSud1 sud x 
  | x==81 = sud
  | (valueAt sud x)==0 && length(areAllowed x sud)==0 = [""]
  | (valueAt sud x)/=0 = (solveSud1 sud (x+1))
  | otherwise = concat [solveSud1 (fillSudoku n x sud) (x+1)|n<-(areAllowed x sud)]

--removes empty strings from list of strings
cleanUp:: [String] -> [String]
cleanUp xs = [x|x<-xs, (length x)/=0]

--the given sudoku that should be a proper sudoku
sudoku:: [String]
sudoku = ["003020600","900305001","001806400","008102900","700000008","006708200","002609500","800203009","005010300"]

--an inproper sudoku, just for testing purposes
sudoku2:: [String]
sudoku2 = ["000020600","900005001","001806400","008102900","700000008","006708200","002609500","800200009","005010000"]

--returns the value at a certain index
valueAt:: [String] -> Int -> Integer
valueAt sud n = read [(sud!!(div n 9))!!(mod n 9)] :: Integer

--returns an array holding the integers allowed at index x
areAllowed:: Int -> [String] -> [Integer]
areAllowed x sud = [n|n<-[1..9], isAllowed n x sud]

--returns whether an integer n is allowed at index x
isAllowed:: Integer -> Int -> [String] -> Bool
isAllowed n x sud = checkRow n x sud && checkCol n x sud && checkQuad n x sud

--checks if integer n is not already in the row holding index x
checkRow:: Integer -> Int -> [String] -> Bool
checkRow n x sud = not(isSubStr (show n) (sud!!(div x 9)))

--checks if integer n is not already in the column holding x
checkCol:: Integer -> Int -> [String] -> Bool
checkCol n x sud = not(isSubStr (show n) (map (!!(mod x 9)) sud))

--checks if integer n is not already in the quadrant holding x
--yeah, this is a long one, but we couldn't figure out a better way...
checkQuad:: Integer -> Int -> [String] -> Bool
checkQuad n x sud
  | mod (div x 3) 3 == 0 && div x 3<9 = not(isSubStr (show n) (show (map (valueAt sud) [0,1,2,9,10,11,18,19,20])))
  | mod (div x 3) 3 == 1 && div x 3<9 = not(isSubStr (show n) (show (map (valueAt sud) [3,4,5,12,13,14,21,22,23])))
  | mod (div x 3) 3 == 2 && div x 3<9 = not(isSubStr (show n) (show (map (valueAt sud) [6,7,8,15,16,17,24,25,26])))
  | mod (div x 3) 3 == 0 && div x 3<18 = not(isSubStr (show n) (show (map (valueAt sud) [27,28,29,36,37,38,45,46,47])))
  | mod (div x 3) 3 == 1 && div x 3<18 = not(isSubStr (show n) (show (map (valueAt sud) [30,31,32,39,40,41,48,49,50])))
  | mod (div x 3) 3 == 2 && div x 3<18 = not(isSubStr (show n) (show (map (valueAt sud) [33,34,35,42,43,44,51,52,53])))
  | mod (div x 3) 3 == 0 = not(isSubStr (show n) (show (map (valueAt sud) [54,55,56,63,64,65,72,73,74])))
  | mod (div x 3) 3 == 1 = not(isSubStr (show n) (show (map (valueAt sud) [57,58,59,66,67,68,75,76,77])))
  | mod (div x 3) 3 == 2 = not(isSubStr (show n) (show (map (valueAt sud) [60,61,62,69,70,71,78,79,80])))

--puts integer n at index x in the sudoku
fillSudoku :: Integer -> Int -> [String] -> [String]
fillSudoku n x sud = concat ([take (div x 9) sud]++[[(changeInt n x (sud!!(div x 9)))]]++[drop ((div x 9)+1) sud])

--change the integer at index x to n (helper function for fillSudoku)
changeInt :: Integer -> Int -> String -> String
changeInt n x sudl = concat ([take (mod x 9) sudl]++[(show n)]++[drop ((mod x 9)+1) sudl])


--checks whether the first string is a substring of the second string
--this is a helper function to the helper functions of isAllowed
--(inspired by one of the exercises from the book)
isSubStr :: String -> String -> Bool
isSubStr [] _ = True
isSubStr _ [] = False
isSubStr (x:xs) (y:ys)
  | x == y = isSubStr xs ys
  | otherwise = isSubStr (x:xs) ys
