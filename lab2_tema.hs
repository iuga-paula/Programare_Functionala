
import Data.List
import Data.Char ( isDigit, digitToInt )

-- L2.3

pozitiveRec :: [Int] -> Int
pozitiveRec [] = 0
pozitiveRec [x] = 1
pozitiveRec l
  | h >0 = pozitiveRec t + 1
  | otherwise = pozitiveRec t
  where 
    h = head l
    t = tail l


pozitiveComp :: [Int] -> Int
pozitiveComp l = length[x | x<-l, x>0]

-- L2.4 
pozitiiImpareRec :: [Int] -> [Int]
pozitiiImpareRec l = contor l 0

contor :: [Int] -> Int -> [Int]
contor [] _  = []
contor l i
   | odd h = i:contor t (i+1)
   | otherwise = contor t (i+1)
   where 
       h = head l
       t = tail l


{- pozitiiImpareRec [] = []
pozitiiImpareRec l
    | odd h = x : t'
    | otherwise = t'
    where 
        h = head l
        t = tail l
        t' = pozitiiImpareRec t
        x = Nr_contor x
        Nr_contor :: Int -> Int
        Nr_contor x = 
            x + 1 -}



pozitiiImpareComp :: [Int] -> [Int]
pozitiiImpareComp l = [ i | (i,x) <- [1..] `zip` l, odd x] 


-- L2.5
multDigitsRec :: String -> Int
multDigitsRec [] = 1
multDigitsRec sir
    |isDigit x = digitToInt x*multDigitsRec t
    |otherwise = multDigitsRec t
    where
        t = tail sir
        x = head sir

multDigits :: String -> Int
--multDigits sir = product[digitToInt x | x <- sir, isDigit x == True]
multDigits sir = product[if isDigit x then digitToInt x else 1 | x <- sir]

-- L2.6
discountComp :: [Float] -> [Float]
discountComp l = [x - 1/4*x | x <- l, x - 1/4*x < 200]

discountRec :: [Float] -> [Float]
discountRec [] = []
discountRec l
  | h - 1/4*h < 200 = (h - 1/4*h):t'
  | otherwise = discountRec t
  where
      h = head l
      t = tail l
      t' = discountRec t