--1
import Data.Char(toUpper)
f :: Char -> Bool
f x
   | toUpper x >= 'A' && toUpper x <= 'M' = True
   | otherwise = False

--2
g :: String -> Bool
g l = g1 l > g2 l 

g1 :: String -> Integer
g1 l  = sum[1 | x <- l, f x == True]

g2 :: String  -> Integer
g2 l  = sum[1 | x<-l, f x == False]


--3
h1 :: String -> Integer -> Integer -> Bool
h1 [] 0 0 = False
h1 [] nr1 nr2 = nr1 > nr2
h1 (x:xs) nr1 nr2 
   | f x  = h1 xs (nr1+1) nr2 
   | otherwise = h1 xs nr1 (nr2+1)
   

h :: String -> Bool
h l = h1 l 0 0


--4
{-numaraElem :: Int -> [Int]  -> Bool
numaraElem x l 
   | sum[1| y <- l, y == x] >= 2  = True
   | otherwise = False

nrElem :: Int -> [Int]  -> Int
nrElem x l = sum[1| y <- l, y == x] 

 c :: [Int] -> [[Int]
--c l =  map(replicate (nrElem) l) . filter(\x -> numaraElem x l) 
c l = map(\x -> take ((nrElem x l)  (repeat x))) . filter(\x -> numaraElem x l)

--adaugaNr :: [Int] -> Int -> [Int]
--adaugaNr l x  -}

c :: [Int] -> [Int]
c [] = []
c [x] = []
c xs = concat[[y] |(y,i) <- xs `zip` tail(xs), y == i]


--5
d :: [Int] -> [Int]
d (x:xs) = if(length xs > 0)
             then if x == head xs
                 then x : d xs
                 else d xs
             else []

--6
prop_cd :: Bool
prop_cd = c [1,2,2,2,3,3,3,3,4] == d [1,2,2,2,3,3,3,3,4] && c [42] == d [42]

import Test.QuickCheck
prop_cd1 :: [Int] -> Bool
prop_cd1 xs = c xs == d xs