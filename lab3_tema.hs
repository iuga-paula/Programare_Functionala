import Data.List()
--L3.4

--4

f4 :: [[Char]] -> [[Char]]
f4 l = map(filter(\x -> filtreazaLitere x)) l

filtreazaLitere :: Char -> Bool
filtreazaLitere c 
    | c `elem` "aeiouAEIOU"  = False
    | otherwise = True


--L3.5
mymap ::  (a -> b) -> [a] -> [b] --map ia argument o functie f care are ca argument a si retuneaza o valoare b, si o lista [a] si returneaza o lista [b] dupa ce aplica fiecarui elem din [a] functia f
mymap _ [] = []
mymap f (x:xs) = f x : map f xs

myfilter :: (a -> Bool) -> [a] -> [a] --filter ia argument un predicat ( o functie care ia argument un an si returneaza ture sau false daca a satisface un criteriu)
                                      --si o lista [a] pe care aplica predicatul; returneaza tot o lista cu elementele care satisfac conditia din predicat
myfilter _ [] = []
myfilter p (x:xs)
    | p x = x : filter p xs
    | otherwise = filter p xs

--Material Suplimentar
--ciurului lui Eratostene 
numerePrimeCiur :: Int -> [Int]
numerePrimeCiur x = ciurEratostene x [2..]

ciurEratostene ::Int -> [Int] -> [Int]
ciurEratostene elem (p:xs)
    | p < elem  = p : ciurEratostene elem [x | x<-xs, x `mod` p > 0]
    | otherwise = []

--and [True , False , True ]  => False face && intre toate comp listei daca gaseste && False se scurcircuiteaza si returneaza false
-- and [ 1 < 2 , 2 < 3 , 3 < 4 ] <=>  and [True, True, True] => True
-- and [ 1 < 2 , 2 < 3 , 3 < 1 ] <=> and [True, True, False] => False

--1
--ar trebui sa avem and [primul elem < al doilea, al doilea < al treilea, ...]
ordonataNat' :: [Int] -> Bool
ordonataNat' [] = True
ordonataNat' [x] = True
ordonataNat' xs =  and [ y <= i|  (y,i) <- xs `zip` tail(xs)]


--2
ordonataNat1 :: [Int] -> Bool
ordonataNat1 [] = True
ordonataNat1 [x] = True
ordonataNat1 (x:xs)
  | x < head xs && ordonataNat1 xs = True
  | otherwise = False

--3
--a
ordonata :: [a] -> (a -> a -> Bool) -> Bool
ordonata l f = and [ f y i|  (y,i) <- l `zip` tail(l)]

--c
(*<*) :: (Integer, Integer) -> (Integer, Integer) -> Bool
(*<*) (a,b) (c,d) 
    | a < c  && b < d = True
    | otherwise = False

--ordonata [(1,2), (3,4), (5,6)] (*<*) => True
--ordonata [(1,2), (0,1), (5,6)] (*<*) => False

--4
compuneList :: (b -> c) -> [(a -> b)] -> [(a->c)]
compuneList f l = [f . x | x <- l]
{-  :t compuneList (+1) [sqrt, (^2), (/2)]
compuneList (+1) [sqrt, (^2), (/2)] :: Floating c => [c -> c] -}

aplicaList :: a -> [(a -> b)] -> [b]
aplicaList a l = [x $ a | x<-l]  

--myzip3
myzip3 :: [a] -> [b] -> [c] -> [(a,b,c)]
myzip3 (a:as) (b:bs) (c:cs) = (a,b,c) : myzip3 as bs cs
myzip3 _ _ _  = []

unpack :: ((a, a), a) -> (a,a,a)
unpack ((a1,a2),b) = (a1,a2,b)
myzip3' :: [a] -> [a] -> [a] -> [(a, a, a)]
myzip3' a b c = map unpack(zip (zip a b) c)