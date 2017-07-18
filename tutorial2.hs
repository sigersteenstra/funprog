import Data.Char

--7.2
sumFirstTwo :: [Integer] -> Integer
sumFirstTwo(x:y:_) = x + y
sumFirstTwo(x:_) = x
sumFirstTwo [] = 0

--7.6
and2 :: [Bool] -> Bool
and2 [] = True
and2 (b:bs) = b && (and2 bs)

or2 :: [Bool] -> Bool
or2 [] = False
or2 (b:bs) = b || (or2 bs)

--7.8
elemNum :: Integer -> [Integer] -> Integer
elemNum a [] = 0
elemNum a (x:xs)
  | a==x = 1 + (elemNum a xs)
  | otherwise = elemNum a xs
  
elemNum2 :: Integer -> [Integer] -> Integer
elemNum2 a xs = sum [1 | x<-xs, x==a]
  
--7.9
unique :: [Integer] -> [Integer]
unique xs = [x | x<-xs, elemNum x xs ==1]

removeElement :: Integer -> [Integer] -> (Bool,[Integer])
removeElement _ [] = (False,[])
removeElement x (y:ys)
  | x == y = (True, rem)
  | otherwise = (flag, y:rem)
  where(flag,rem) = removeElement x ys

unique2 :: [Integer] -> [Integer]
unique2 [] = []
unique2 (x:xs)
  | flag = unique2 rem
  | otherwise = x : (unique2 rem)
  where(flag,rem) = removeElement x xs
    
--7.16
ins :: Integer -> [Integer] -> [Integer]
ins x [] = [x]
ins x (y:ys)
  | x == y = y:ys
  | x > y = x:(y:ys)
  | otherwise = y : ins x ys
  
iSort :: [Integer] -> [Integer]
iSort [] = []
iSort (x:xs) = ins x (iSort xs)

--7.25
isSubStr :: String -> String -> Bool
isSubStr [] _ = True
isSubStr _ [] = False
isSubStr (x:xs) (y:ys)
  | x == y = isSubStr xs ys
  | otherwise = isSubStr (x:xs) ys

isSubSeq :: String -> String -> Bool
isSubSeq [] _ = True
isSubSeq _ [] = False
isSubSeq (x:xs) (y:ys)
  | x == y = (unzip(zip (x:xs) (y:ys))) == ((x:xs),(x:xs)) || isSubSeq (x:xs) ys
  | otherwise = isSubSeq (x:xs) ys


--7.33
isPalin :: String -> Bool
isPalin str = s == reverse s
  where s = [toLower c | c <- str, isAlpha c]
	
--7.34
--subst :: String -> String -> String -> String
--subst __ [] = []
--subst (x:xs sub (y:ys)
--  | match = sub ++ (drop (length xs) ys)
--  | otherwise = y:(subst (x:xs) sub ys)
--  where match = (unzip(zip (x:xs) (y:ys))) == ((x:xs,(x:xs))
