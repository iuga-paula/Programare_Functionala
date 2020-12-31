
import   Data.List
--L3.1
--per 1,1 1,2 1,3 2,4 ..
--[[x .. y] | x <- [1 .. 5], y <- [1 .. 5], x < y ] prod cartezian [[1,2],[1,2,3],[1,2,3,4],[1,2,3,4,5],[2,3],[2,3,4],[2,3,4,5],[3,4],[3,4,5],[4,5]]

factori :: Int -> [Int]
factori x = [y| y<-[2..x-1], x `rem` y == 0]

prim :: Int -> Bool
prim x
   |length (factori x) == 0 = True
   |otherwise = False


numerePrime :: Int -> [Int]
numerePrime x = [nr| nr<-[2..x], prim nr == True]

--L3.2
myzip3 :: [Int] -> [Int] -> [Int] -> [(Int, Int, Int)]
--myzip3 l1 l2 l3 = [(x,y,z) | x <-l1, y<-l2, z<-l3] --prod cartezian nu zip, zip ia doar primul elem din l1 cu priul din l2 cu primul din 3 si sare la aldoilea elem din l1 etc
unpack :: ((Int, Int), Int) -> (Int,Int,Int)
unpack ((a1,a2),b) = (a1,a2,b)
myzip3 a b c = map unpack(zip (zip a b) c)

--map ( `elem` [2, 3] ) [1, 3, 4, 5] 
--pune elem din lista mare la inceput si verifica daca 1 se gaseste in [2, 3], daca 3 se gaseste in [2,3] etc

--L3.3
--1
first :: (a,b) -> a
first (a,b) = a
firstEl :: [(a,b)] -> [a]
firstEl l = map (first) l

--2
sumList :: [[Int]] -> [Int]
sumList l = map (sum) l

--3
f :: Int -> Int
f x 
    | even x = x `div` 2
    | otherwise = 2*x

prel2 :: [Int] -> [Int]
prel2 l = map (f) l

--L3.4
--1
f1 :: Char -> [[Char]]-> [[Char]]
f1 c l = filter (elem c) l

--2
f2 :: [Int] -> [Int]
f2 = map (^2) . filter odd

--3
{- pozitiiImpareComp :: [Int] -> [Int]
pozitiiImpareComp l = [ i | (i,x) <- [1..] `zip` l, odd x] 

verif :: [Int] -> Bool
verif x l
  | x<-l 'elem' pozitiiImpareComp L==True  =  True
  | otherwise = False

f3 :: [Int] -> [Int]
f3 l = (^2) filter(pozitiiImpareComp) l -}

f3 :: [Int] -> [Int]
f3 l =
    let lWithPos = zip l [0..]
      in map((^2).fst)
          .filter (odd . snd)
          $ lWithPos