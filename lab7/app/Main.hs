module Main where

import Lib
import Test.QuickCheck
import Data.Char
import Data.List

main :: IO ()
main = someFunc
double :: Int -> Int
double x = 2 * x

triple :: Int -> Int 
triple x =  3 * x

penta :: Int -> Int
penta x = 5 * x

test x = (double x + triple x) == (penta x)
test2 x = (double x + double x) == (penta x)

myLookUp :: Int -> [(Int,String)]-> Maybe String
myLookUp x [] = Nothing
nyLookUp x l = head [Just j| (i,j)<-l, i == x]

myLookUp2 :: Int -> [(Int, String)] -> Maybe String
myLookUp2 n l = process (filter(\(a, _) -> a == n) l)
     where
         process :: [(Int,String)] -> Maybe String
         process [] =  Nothing
         process (x:xs) = Just(snd x)

testLookUp :: Int -> [(Int, String)] -> Bool
testLookUp x l = myLookUp x l == lookup x l

testLookUp2 :: Int -> [(Int, String)] -> Bool
testLookUp2 x l = myLookUp2 x l == lookup x l

--generate (choose(1,4))

testLookUpCond :: Int -> [(Int,String)] -> Property
testLookUpCond n list = n > 0 && n `div` 5 == 0 ==> testLookUp2 n list

testLookUpCond' :: [(Int, String)] -> Property --listele le ia singur quickCheck
testLookUpCond' list = forAll myGen (\n -> testLookUp2 n list)
   where
       myGen :: Gen Int 
       myGen = choose(1,4)

--8
--a)
myLookUp' :: Int -> [(Int,String)]-> Maybe String
myLookUp' n l = process (filter(\(a, _) -> a == n) l)
     where
         process :: [(Int,String)] -> Maybe String
         process [] =  Nothing
         process [(_,"")] = Just ""
         process (x:xs) =  Just(toUpper(head(snd x)) : tail (snd x))

--b)
verifList :: [(Int, String)] -> Bool
verifList list = length list == length [x| x<-list, (toUpper(head(snd x)) : tail (snd x)) == snd x]

testLookUp' :: Int -> [(Int,String)] -> Bool
testLookUp' x l = myLookUp' x l == lookup x l

testLookUpCond'' ::  Int -> [(Int,String)] -> Property
testLookUpCond'' n list = verifList list ==> testLookUp' n list

--9
data ElemIS = I Int | S String
     deriving (Show,Eq)

instance Arbitrary ElemIS where
    arbitrary = do 
        intVal <- arbitrary :: Gen Int
        stringVal <- arbitrary :: Gen String
        elements [I intVal, S stringVal]

-- instance Arbitrary ElemIS where
--     arbitrary =
--         let iGen = fmap I (arbitrary :: Gen Int)
--             sGen = fmap S (arbitrary :: Gen String)
--         in  oneof [iGen, sGen]

myLookUpElem :: Int -> [(Int,ElemIS)]-> Maybe ElemIS
myLookUpElem n l = process (filter(\(a, _) -> a == n) l)
     where
         process :: [(Int,ElemIS)] -> Maybe ElemIS
         process [] =  Nothing
         process (x:xs) = Just(snd x)

testLookUpElem :: Int -> [(Int,ElemIS)] -> Bool
testLookUpElem x l = myLookUpElem x l == lookup x l

--4 simulare
c :: [Int] -> [Int]
c [] = undefined
c l = [i|  (i,j) <- zip l (tail(l)), i==j]


--5 simulare
d :: [Int] -> [Int] --var recursiva
d [] = undefined
d [_] = []
d l@(x:y:xs)
   | x == y = x : d (y:xs)
   | otherwise = d(y:xs)


--6 simulare
prop_cd :: [Int] -> Property
prop_cd l = (not . null) l ==>c l == d l

--L2.3
pozitiveRec :: [Int] -> Int
pozitiveRec list = calculeaza 0 list

calculeaza :: Int -> [Int] -> Int
calculeaza contor [] = contor
calculeaza contor (x:xs)
    | x > 0 = calculeaza (contor + 1) xs
    | otherwise = calculeaza contor xs 

pozitiveComp :: [Int] -> Int
pozitiveComp list = sum[1| x <- list, x>0]

pozitiveComp2 :: [Int] -> Int
pozitiveComp2 list = length[x| x <- list, x>0]

testPozitive :: [Int] -> Bool
testPozitive list = pozitiveRec list == pozitiveComp list -- OK 100 tests passed

testPozitiveCond :: [Int] -> Property   -- +++ OK, passed 100 tests; 126 discarded.
testPozitiveCond list = length list > 3 ==> pozitiveRec list == pozitiveComp list

testPozitiveCond2 :: [Int] -> Property  -- +++ OK, passed 100 tests; 119 discarded.
testPozitiveCond2 list = length list > 3 ==> testPozitive list

--L2.4
pozImpRec :: [Int] -> [Int]
pozImpRec  list = pozContor 0 list

pozContor :: Int -> [Int] -> [Int]
pozContor contor [] = []
pozContor contor (x:xs)
    | even contor = x:pozContor (contor + 1) xs
    | otherwise = pozContor (contor + 1) xs

pozImpComp :: [Int] -> [Int]
pozImpComp l = [j| (i,j) <- zip [0..] l, even i] 

testImp :: [Int] -> Bool
testImp l = pozImpRec l == pozImpComp l

verifL :: [Int] -> Bool
verifL l = length l ==length list
    where list = [x| x<-l, even x] 

testImpCond :: [Int] -> Property
testImpCond l = verifL l ==> testImp l

--L2.5
multDigitRec :: String -> Int
multDigitRec [] = 1
multDigitRec (x:xs) 
    | isDigit x = (digitToInt x) * multDigitRec xs
    | otherwise = multDigitRec xs

multDigitComp :: String -> Int
multDigitComp str = product[digitToInt x| x<-str, isDigit x]

testmultDigit :: String -> Bool
testmultDigit str = multDigitComp str == multDigitRec str

testmultDigitC :: String -> Property
testmultDigitC str = verif2chrs str ==> testmultDigit str

verif2chrs :: String -> Bool -- sa nu fie 2 caract string pe poz consecutive
verif2chrs "" = True
verif2chrs str@(x:xs) = and[if isDigit i && isDigit j then False else True| (i,j) <- zip str xs]

--L3.1
factori :: Int -> [Int]
factori x = [i| i<-[2..x-1], x `mod` i == 0]

factoriRec :: Int -> [Int]
factoriRec x = fcr x (x-1)

fcr :: Int -> Int -> [Int]
fcr  n jos 
  | jos > 1 && n `mod` jos == 0 = jos : fcr n (jos - 1)
  | jos > 1 && n `mod` jos /= 0 = fcr n (jos - 1)
  | otherwise = []

testfactori :: Int -> Bool
testfactori n = factori n == reverse (factoriRec n)

prim :: Int -> Bool
prim x = factori x == []

numerePrime :: Int -> [Int]
numerePrime n = [x| x<- [2..n], prim x]

--L3.3
firstEl :: [(Char, b)] -> [b]
fristEl [] = []
firstEl (x:xs) 
    | isAlpha (fst x) = (snd x):fristEl xs
    | otherwise = fristEl xs

firstElC :: [(a, b)] -> [b]
firstElC l = [snd x| x<-l]

firstEl' l = map(snd) l

sumList :: [[Int]] -> [Int]
sumList l = map(sum) l

prel :: [Int] -> [Int]
prel l = map(\x -> x*2) l

prel2 :: [Int] -> [Int]
prel2 l = map(f) l

f :: Int -> Int
f x 
    | even x = x `div` 2
    | otherwise = 2*x

--finding the sum of all odd squares that are smaller than 10,000.
oddSSum :: Int
oddSSum = sum $ takeWhile(<1000) list 
    where 
        list = map(^2) . filter(odd) $ [1..]

--l3.4
f1 :: [String] -> Char -> [String]
f1 [] _ = []
f1 (x:xs) c
    | c `elem` x = x : f1 xs c
    | otherwise = f1 xs c

f1' :: [String] -> Char -> [String]
f1' l c = filter(c `elem` ) l

f2 :: [Int] -> [Int]
f2 l = map(^2) $ filter(\x -> odd x) l

f3 :: [Int] -> [Int]
f3 l = map((^2) . fst) $ filter(odd . snd) poz 
    where poz = zip l [0..]

f4 :: [String] -> [String]
f4  l = map(eliminaCons) l
    where
        eliminaCons str = filter(`elem` "aeiouAEIOU") str

--sumpl L4
--1
ordonataNat :: [Int] -> Bool
ordonataNat l@(x:xs) = and[i<j| (i, j) <- zip l xs]

--2
ordonataNat' :: [Int] -> Bool
ordonataNat' [] = True
ordonataNat' [x] = True
ordonataNat' l@(x:y:xs) 
    | x < y = ordonataNat' (y:xs)
    | otherwise =  False

--sim1
fs :: Char -> Bool
fs c 
  |  (not . isAlpha) c = error "Nu e caracter"
  | (toLower c) `elem` ['a'..'m'] = True
  | otherwise = False

--sim2
g :: [Char] -> Bool
g str = length firsthalf > length secondhalf
    where
        firsthalf = [x| x<-str, fs x]
        secondhalf = str \\ firsthalf

--sim3
g' :: [Char] -> Bool
g' str = gcontor 0 0 str 

gcontor :: Int -> Int -> String -> Bool
gcontor i j [] = i > j
gcontor i j (x:xs)
  | fs x = gcontor (i+1) j xs
  | (not.fs) x && isAlpha x = gcontor  i (j+1) xs
  | otherwise = gcontor i j xs

  