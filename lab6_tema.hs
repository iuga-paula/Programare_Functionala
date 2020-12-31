import Data.Char
-- --------Cripatare si Decriptare-------

--1
slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

rotate :: Int -> [Char] -> [Char]
rotate n l 
   | n > 0 && n < length l = slice n (length l - 1) l   ++ slice 0 (n-1) l
   | otherwise = 
       error 
       $ "wrong value for n!"

--2
prop_rotate :: Int -> String -> Bool
prop_rotate k str = rotate (l - m) (rotate m str) == str
                        where l = length str
                              m = if l == 0 then 0 else k `mod` l
--fs prop_rotate verifica daca rezultatul intors de fs rotate este unul corect
-- pt k < lungimea sirului => m = k => rotate m str pune la coada primele k caractere iar apoi rotate lungime sir - m de rezultat pune la coada toate caracterele care nu au fost rotite in primul apel al fs rotate => ar trebui sa se obtina sirului str initial
-- pt k > lungimea sirului => ordinul de apel al functiilor rotate descrise mai sus o sa fie inversat dar se obtine tot sirul initial
-- pe scurt, fs prop_rotate roteste sirul initial cu ajutorul fs rotete cu un numar de m caractere, iar apoi rezultaul il rosteste cu nr de caratere care au ramas nerotite la primul pas din sirul initial => trebuie sa se obtina sirul initial


--3
alfabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
makeKey :: Int -> [(Char,Char)]
makeKey n = [x| x <- alfabet `zip`  rotate n alfabet]

--4
lookUp :: Char -> [(Char, Char)] -> Char
lookUp c l
    | not (null calcul) = snd (head calcul) 
    | otherwise = c
    where
        calcul = [(i,j)| (i,j) <- l, i == c]


--5
encipher :: Int -> Char -> Char
encipher key c = chr ((ord c  - 65 + key) `mod` 26 + 65)


--6
normalize :: String -> String
normalize l = [toUpper c| c <- l, (isAlpha c) || (isDigit c) || c == ' ']

--7
encipherStr :: Int -> String -> String
encipherStr key str = [if isAlpha c then encipher key c else c| c <- strnormal] 
                      where
                          strnormal = normalize str


-- -------- Decodarea unui mesaj --------

--8
reverseKey :: [(Char, Char)] -> [(Char, Char)]
reverseKey l = [(j,i)| (i,j) <- l]

--9
decipher :: Int -> Char -> Char
decipher key c =  chr ((ord c  - 65 - key) `mod` 26 + 65)

deciptherStr :: Int -> String -> String
deciptherStr _ [] = ""
deciptherStr key (x:xs)
    | x `elem` ['A'..'Z'] = decipher key x : deciptherStr key xs
    | x `elem` ' ': ['0'..'9'] = x : deciptherStr key xs 
    | otherwise = deciptherStr key xs


-- -------- Matrice --------
--
data Linie = L [Int]
    deriving Show

data Matrice = M [Linie]

--a
suma :: Linie -> Int
suma (L lista) = sum lista

verifica :: Matrice -> Int -> Bool
verifica (M a) val = all(== val)(map suma a) 
-- verifica (M [L [1, 2, 3], L [4, 5], L [2, 3, 6, 8], L [8, 5, 3]]) 10 - False
-- verifica (M [L [2, 20, 3], L [4, 21], L [2, 3, 6, 8, 6], L [8, 5, 3, 9]]) 25 - True

--b

{- arataLinie :: (Show a) => Linie a -> String  
arataLinie  (L a) = show a -}

arataLinie :: Linie -> String
arataLinie (L []) = ""
arataLinie (L (x:xs)) = show x ++ " " ++ arataLinie(L xs) 

instance Show Matrice where
    show (M []) = ""
    show (M (x:xs)) = arataLinie x ++ "\n" ++ show (M xs)

-- M [L [1, 2, 3], L [4, 5], L [2, 3, 6, 8], L [8, 5, 3]]

--c
daLungLinie :: Int -> Linie -> Bool
daLungLinie n (L lista)  = length lista == n

verifPoz :: Linie -> Bool
verifPoz (L lista) = length lista == length [x| x <- lista, x >0] 

doarPozN :: Matrice -> Int -> Bool
doarPozN (M a) n = and(map verifPoz(filter (daLungLinie n) a))

-- doarPozN (M [L [1, 2, 3], L [4, 5], L [2, 3, 6, 8], L [8, 5, 3]]) 3 - True
-- doarPozN (M [L [1, 2, -3], L [4, 5], L [2, 3, 6, 8], L [8, 5, 3]]) 3 - False