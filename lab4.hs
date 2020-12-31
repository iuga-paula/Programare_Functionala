import Numeric.Natural

produsRec1 :: [Integer] -> Integer -> Integer
produsRec1 [] acc = acc
produsRec1 (x:xs) acc = produsRec1 xs (acc*x)


produsRec :: [Integer] -> Integer
produsRec l = produsRec1 l 1




produsFold :: [Integer] -> Integer
produsFold l = foldr (*) 1 l


andRec :: [Bool] -> Bool
andRec [] = True
andRec (x:xs) = x && andRec(xs)



andFold :: [Bool] -> Bool
andFold = foldr (&&) True
--merge si fara param 

concatRec :: [[a]] -> [a]
concatRec [] = []
concatRec (x:xs) =  x ++ concatRec xs

concatFold :: [[a]] -> [a]
concatFold = foldr(++) []

rmChar :: Char -> String -> String
rmChar  c l = filter (c /= ) l
--rmChar  c l = filter (\x -> x /= c) l

rmCharsRec :: String -> String -> String
rmCharsRec s [] = []
rmCharsRec s (x:xs)
    | elem x s = rmCharsRec s xs
    | otherwise = x: rmCharsRec s xs

rmCharsRec2 :: String -> String -> String
rmCharsRec2 [] str = str
rmCharsRec2 (x : xs) str = rmChar x (rmCharsRec2 xs str)

test_rmchars :: Bool
test_rmchars = rmCharsRec ['a'..'l'] "fotbal" == "ot"



rmCharsFold :: String -> String -> String
rmCharsFold charsToRemove str = foldr f unit charsToRemove
    where
		unit = str
		f = rmChar



logistic :: Num a => a -> a -> Natural -> a
logistic rate start = f
  where
    f 0 = start
    f n = rate * f (n - 1) * (1 - f (n - 1))




logistic0 :: Fractional a => Natural -> a
logistic0 = logistic 3.741 0.00079
ex1 :: Natural
ex1 = undefined


ex20 :: Fractional a => [a]
ex20 = [1, logistic0 ex1, 3]

ex21 :: Fractional a => a
ex21 = head ex20

ex22 :: Fractional a => a
ex22 = ex20 !! 2

ex23 :: Fractional a => [a]
ex23 = drop 2 ex20

ex24 :: Fractional a => [a]
ex24 = tail ex20


ex31 :: Natural -> Bool
ex31 x = x < 7 || logistic0 (ex1 + x) > 2

ex32 :: Natural -> Bool
ex32 x = logistic0 (ex1 + x) > 2 || x < 7
ex33 :: Bool
ex33 = ex31 5

ex34 :: Bool
ex34 = ex31 7

ex35 :: Bool
ex35 = ex32 5

ex36 :: Bool
ex36 = ex32 7

semn :: [Integer] -> String
semn [] = ""
semn (x:xs) 
	| x `elem` [-9..(-1)] = "-" ++ semn xs
	| x == 0 = "0" ++ semn xs
	| x `elem` [1..9] = "+" ++ semn xs
	| otherwise = semn xs
	
semnFold :: [Integer] -> String
semnFold = foldr op unit
	where
		unit = ""
		x `op` rez
			| x `elem` [-9..(-1)] = "-" ++ rez
			| x == 0 = "0" ++ rez
			| x `elem` [1..9] = "+" ++ rez
			| otherwise = rez
