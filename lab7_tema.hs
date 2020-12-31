module Main where

import Lib
import Test.QuickCheck
import Data.Char

main :: IO ()
main = someFunc
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