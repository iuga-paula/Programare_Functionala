-- -------- Fructe --------
--
data Fruct --constr de disjunctii de constructori
    = Mar String Bool
    | Portocala String Int

ionatanFaraVierme = Mar "Ionatan" False
goldenCuVierme = Mar "Golden Delicious" True
portocalaSicilia10 = Portocala "Sanguinello" 10

listaFructe =
    [ Mar "Ionatan" False
    , Portocala "Sanguinello" 10
    , Portocala "Valencia" 22
    , Mar "Golden Delicious" True
    , Portocala "Sanguinello" 15
    , Portocala "Moro" 12
    , Portocala "Tarocco" 3
    , Portocala "Moro" 12
    , Portocala "Valencia" 2
    , Mar "Golden Delicious" False
    , Mar "Golden" False
    , Mar "Golden" True
    ]

ePortocalaDeSicilia :: Fruct -> Bool
ePortocalaDeSicilia (Portocala s _) = elem s ["Moro", "Sanguinello","Tarocco"]
ePortocalaDeSicilia _ = False

--sau
--ePortocalaDeSicilia f = 
   -- case f of
       -- Porotcala soi _ -> soi `elem` ["Moro", "Sanguinello","Taroco"]
             --           -> False

 --sau
--  ePortocalaDeSicilia (Porocala "Moro" _) = True
--  ePortocalaDeSicilia (Porocala   "Taroco" _) = True
--   ePortocalaDeSicilia (Porocala   "Sanguinello" _) = True
--   ePortocalaDeSicilia  _ = False

test_ePortocalaDeSicilia1 =
    ePortocalaDeSicilia (Portocala "Moro" 12) == True

test_ePortocalaDeSicilia2 =
    ePortocalaDeSicilia (Mar "Ionatan" True) == False

--nrFeliiSicilia :: [Fruct] -> Int
--nrFeliiSicilia [] = 0
--nrFeliiSicilia l = nrFel l 0  

--nrFel :: [Fruct] -> Int -> Int
--nrFel l nr = sum $ map(nr) filter ePortocalaDeSicilia l

nrFeliiSicilia :: [Fruct] -> Int
nrFeliiSicilia l = sum(map getFelie(filter ePortocalaDeSicilia l))
                   where
                       getFelie :: Fruct -> Int
                       getFelie (Portocala _ nr) = nr
                       getFelie (Mar _ _) = 0


test_nrFeliiSicilia = nrFeliiSicilia listaFructe == 52

nrMereViermi :: [Fruct] -> Int
nrMereViermi l = length (filter eMarcuviermi l)
                 where
                     eMarcuviermi :: Fruct -> Bool
                     eMarcuviermi (Mar _ viermi) = (viermi == True)
                     eMarcuviermi _ = False

test_nrMereViermi = nrMereViermi listaFructe == 2

-- -------- Matrice --------
--
data Linie = L [Int]
    deriving Show

data Matrice = M [Linie]

-- verifica (M [L [1, 2, 3], L [4, 5], L [2, 3, 6, 8], L [8, 5, 3]]) 10
-- verifica (M [L [2, 20, 3], L [4, 21], L [2, 3, 6, 8, 6], L [8, 5, 3, 9]]) 25

-- M [L [1, 2, 3], L [4, 5], L [2, 3, 6, 8], L [8, 5, 3]]

-- doarPozN (M [L [1, 2, 3], L [4, 5], L [2, 3, 6, 8], L [8, 5, 3]]) 3
-- doarPozN (M [L [1, 2, -3], L [4, 5], L [2, 3, 6, 8], L [8, 5, 3]]) 3
