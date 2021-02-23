module Main where

import Lib
import System.Random
import Data.Char as Char

main :: IO ()
main = someFunc
newtype MyRandom a = MyRandom { runRandom :: StdGen -> (a,StdGen) }

randomPositive :: MyRandom Int
randomPositive = (MyRandom next)


instance Functor MyRandom where
    --fmap :: (a -> b) -> MyRandom a ->  MyRandom b
    fmap f (MyRandom ma) =
         MyRandom $ \gen ->
             let (result, newGen) = ma gen
                in (f result, newGen)


randomBoundedInt :: Int -> MyRandom Int
randomBoundedInt a = fmap f randomPositive
    where
        f :: Int -> Int
        f x = x `mod` a

--ex 2 c
randomLetter :: MyRandom Char
randomLetter  = fmap Char.chr (randomBoundedInt( Char.ord 'z'))

--ex 3 c
constructDigit = (+) <$> ((*10) <$> randomBoundedInt 9)<*> randomBoundedInt 9