import Data.Char(toUpper, toLower)
import System.IO

{- Monada Maybe este definita in GHC.Base 

instance Monad Maybe where
  return = Just
  Just va  >>= k   = k va
  Nothing >>= _   = Nothing


instance Applicative Maybe where
  pure = return
  mf <*> ma = do
    f <- mf
    va <- ma
    return (f va)       

instance Functor Maybe where              
  fmap f ma = pure f <*> ma   
-}
--1
(<=<) :: (a -> Maybe b) -> (c -> Maybe a) -> c -> Maybe b
f <=< g = (\ x -> g x >>= f)

f :: [Char] -> Maybe Char
f [] = Nothing
f str = Just (head str)

g :: [Char] -> Maybe [Char]
g [] = Nothing
g str = Just ([toUpper $ head str] ++ (map toLower $ tail str))

--g "mere" >>= f 
--Just 'M'

--f <=< g $ "mere"
--Just 'M'

--2
{-
e1 >>= \x1 -> (e2 >>= \_ -> e3)

echiv cu

do
    x1 <- e1
    e2
    e3
-}

pos :: Int -> Bool
pos  x = if (x>=0) then True else False

pos' :: Int -> Bool
pos' x = 
    do
        if x >=0 then True else False



foo :: Maybe Int ->  Maybe Bool 
foo  mx =  mx  >>= (\x -> Just (pos x))  

foo' :: Maybe Int ->  Maybe Bool 
foo'  mx = do
    x <- mx
    Just (pos x) --de mai sus verifica daca nr e pozitiv sau nu

--3
addM :: Maybe Int -> Maybe Int -> Maybe Int
addM mx my = mx >>= (\x -> my >>= \y -> return (x + y))

addM' :: Maybe Int -> Maybe Int -> Maybe Int
addM' mx my = do
    x <- mx
    y <- my
    Just(x+y)

testAddM :: Maybe Int -> Maybe Int -> Bool
testAddM mx my = addM mx my  == addM' mx my 
--test pt quickcheck

--4
cartesian_product xs ys = do
    x <- xs
    y <- ys
    (x,y)

prod f xs ys = do
    x <- xs --se parcurge lista xs cu elem x
    y <- ys
    f x y

myGetLine :: IO String --citeste recursiv o linie caracter cu caracter
{- myGetLine (x:xs) = do
    a <- x
    b <- xs
    if a == '\n' then [] else b
     -}

myGetLine = do
    x <- getChar
    --y <- myGetLine
    if x == '\n' then 
        return [] 
    else 
        do
            xs <- myGetLine
            return (x:xs)

--5
prelNo noin = sqrt noin
ioNumber = (readLn :: IO Float) >>= \noin -> (putStrLn("Intrare\n" ++ (show noin)) >> let noout = prelNo noin in (putStrLn "Iesire" >> print noout))


-- Monada Writer

newtype WriterS a = Writer { runWriter :: (a, String) } 


instance  Monad WriterS where
  return va = Writer (va, "")
  ma >>= k = let (va, log1) = runWriter ma
                 (vb, log2) = runWriter (k va)
             in  Writer (vb, log1 ++ log2)


instance  Applicative WriterS where
  pure = return
  mf <*> ma = do
    f <- mf
    a <- ma
    return (f a)       

instance  Functor WriterS where              
  fmap f ma = pure f <*> ma  

--1

tell :: String -> WriterS () 
tell log = Writer ((), log)
  
--1.1
logIncrement :: Int  -> WriterS Int
logIncrement x = do
    tell ("s-a inscrementat " ++ (show x) ++ "\n")
    return (x+1) --un writer cu val si nm ca string
    --se comb rez

--runWriter (logIncrement 5)
--(6,"s-a inscrementat 5")



logIncrement2 :: Int  -> WriterS Int
logIncrement2 x = do
    y <- logIncrement x 
    logIncrement y 


--1.2
logIncrementN :: Int -> Int -> WriterS Int
logIncrementN x n = 
    if n == 0 then
        return x
    else 
        do
            y <- logIncrement x
            logIncrementN y (n-1)
    
--runWriter (logIncrementN 3 2)
--(5,"s-a inscrementat 3\ns-a inscrementat 4\n")

--2
newtype WriterS' a = Writer' { runWriter' :: (a, [String]) } 


instance  Monad WriterS' where
  return va = Writer' (va, [])
  ma >>= k = let (va, log1) = runWriter' ma
                 (vb, log2) = runWriter' (k va)
             in  Writer' (vb, log1 ++ log2)

instance  Applicative WriterS' where
  pure = return
  mf <*> ma = do
    f <- mf
    a <- ma
    return (f a)       

instance  Functor WriterS' where              
  fmap f ma = pure f <*> ma  


tell' :: String -> WriterS' () 
tell' log = Writer' ((), [log])

  
logIncrement' :: Int  -> WriterS' Int
logIncrement' x = do
    tell' ("increment: " ++ (show x))--tell face el o lista de Stringuri
    return (x+1) --un writer cu val si nm ca string
    --se comb rez


logIncrementN' :: Int -> Int -> WriterS' Int
logIncrementN' x n = do
    if n == 0 then
        return x
    else 
        do
            y <- logIncrement' x
            logIncrementN' y (n-1)

--runWriter' $ logIncrementN' 2 4
--(6,["increment: 2","increment: 3","increment: 4","increment: 5"])

--3                      
isPos :: Int -> WriterS' Bool
isPos x = if (x>= 0) then (Writer' (True, ["poz"])) else (Writer' (False, ["neg"]))                           
--map isPos [1,2,3] eroare nu are show
--map runWriter' $ map isPos [1,2,-3]
--[(True,["poz"]),(True,["poz"]),(False,["neg"])]

--4 --inalnuirea efectelor
mapWriterLS :: (a -> WriterS' b) -> [a] -> WriterS' [b]
mapWriterLS f xs = do
    let listW = map f xs
    let list0 = map runWriter' listW
    let list1 = map fst list0
    let list2 = concat(map snd list0) --lista de liste
    --return(concat list2)
    Writer'(list1,list2)
    
--5 --mapWriterLS pt WriterS si Maybe
mapWriterLSS :: (a -> WriterS b) -> [a] -> WriterS [b]
mapWriterLSS f xs = do
    let listW = map f xs
    let list0 = map runWriter listW
    let list1 = map fst list0
    let list2 = concat(map snd list0) --lista de liste
    --return(concat list2)
    Writer(list1,list2)

isPos' :: Int -> WriterS Bool
isPos' x = if (x>= 0) then (Writer (True, "poz")) else (Writer (False, "neg"))  
--runWriter $ mapWriterLSS isPos' [1,-2,3]
--([True,False,True],"poznegpoz")

mapWriterLSM :: (a -> Maybe b) -> [a] -> [Maybe  b]
mapWriterLSM f xs = do
    map f xs --[Maybe b]

{- mapWriterLSM' :: (a -> Maybe b) -> [a] -> Maybe  [b]
mapWriterLSM' f xs = do
    let listM  = map f xs --[Maybe b] 
    (Just x) <- listM
    return (Just x) -}

isPos'' :: Int -> Maybe Bool
isPos'' x = if (x>= 0) then (Just True) else (Just False) 