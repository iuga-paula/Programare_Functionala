module Main where

import Lib
import System.Random
import Data.Char as Char

main :: IO ()
main = someFunc

--pr nou cu stack si dependenta -random in package.yaml

--1
newtype MyRandom a = MyRandom { runRandom :: StdGen -> (a,StdGen) }

randomPositive :: MyRandom Int
randomPositive = (MyRandom next)

--runRandom randomPositive(mkStdGen 4)
--(159378,200070 40692)
--mkStdGen returneaza un StdGen cu seed ul 4

--2
--a Myradnom instanta de Functor
instance Functor MyRandom where
    --fmap :: (a -> b) -> MyRandom a ->  MyRandom b
    fmap f (MyRandom ma) =
         MyRandom $ \gen ->
             let (result, newGen) = ma gen
                in (f result, newGen)

        --ma e functie care primeste StdGen -> (a,StdGen)
        --gen e de tip StdGen 

--b se fol instanta Functor pt a defini o functie care intoare un int random mai mic decat nr dat a
randomBoundedInt :: Int -> MyRandom Int
randomBoundedInt a = fmap f randomPositive
    where
        f :: Int -> Int
        f x = x `mod` a
    --fmap (filter (< a)) randomPositive(MyRandom a)

--c se fol instanta Functor pt a defini o functie random litera
--pt apelul lui ord din Data.Char  se poate folosi Char.ord
randomLetter :: MyRandom Char
randomLetter  = fmap Char.chr (randomBoundedInt( Char.ord 'z'))
-- apel runRandom randomLetter (mkStdGen 234)

--3
-- <*> operator aplicativ
-- (<*>) :: Applicative t => t( a -> b) -> t a -> t b
--(+) <$> Just 3  :: Maybr (Int -> Int)
-- <*> stie  sa aplice +3 pe 4
--(+) <$> Just 3 <*> Just 4   -- Just 7

{- :t pure
pure :: Applicative f => a -> f a -}

instance Applicative MyRandom where
    pure x = MyRandom $ \gen -> (x,gen) --gen mereu acel nr x
    (MyRandom mf) <*> (MyRandom ma) = 
        MyRandom $ \gen ->
            let (f, newGen) = mf gen
                (a, newNewGen) = ma newGen
            in (f a, newNewGen)

random10LetterPair :: MyRandom(Int, Char)
random10LetterPair =  (,) <$> randomBoundedInt 10 <*> randomLetter

--c
constructDigit = (+) <$> ((*10) <$> randomBoundedInt 9)<*> randomBoundedInt 9

{- constructDigit = c * 10 + d
    where
        c = \x -> (x,gen) 
            where (x,gen) = randomBoundedInt 9 
        d = \x -> (x,gen) 
            where (x,gen) = randomBoundedInt 9 -}