--1
--
import Data.List (nub, subsequences)
import Data.Maybe (fromJust)
import Data.Char(isAlpha)


type Nume = String
data Prop
  = Var Nume
  | F
  | T
  | Not Prop
  | Prop :|: Prop --disj sau
  | Prop :&: Prop --conj si
  | Prop :->: Prop -- implicatia
  | Prop :<->: Prop --echivalenta
  deriving (Eq, Read)
infixr 2 :|:
infixr 3 :&:

p1 :: Prop
p1 = (Var "P" :|: Var "Q") :&: (Var "P" :&: Var "Q")

--
p2 :: Prop
p2 = (Var "P" :|: Var "Q") :&: (Not(Var "P") :&: Not(Var "Q"))

--
p3 :: Prop
p3 = ((Var "P" :&: (Var "Q" :|: Var "R")) :&: ((Not(Var "P") :|: Not(Var "Q") :&: (Not(Var "P") :|: (Not(Var "Q"))))))

--ex2

{-instance Show Prop where
    show (Not x ) = "~" ++ show x
    show (x :|: y) = show x ++ "|" ++ show y
    show (x :&: y) = show x ++ ":&:" ++ show y
    show F = "F"
    show T = "T"-}

showProp :: Prop -> String
showProp (Var p) = p 
showProp F = "F"
showProp T = "T"
showProp (Not p) = "(~" ++ showProp p ++ ")"
showProp (p :|: q) = "(" ++ showProp p ++ "|" ++ showProp q ++ ")"
showProp (p :&: q) = "(" ++ showProp p ++ "&" ++ showProp q ++ ")"
showProp (p :->: q) = "(" ++ showProp p ++ "->" ++ showProp q ++ ")"
showProp (p :<->: q) = "(" ++ showProp p ++ "<->" ++ showProp q ++ ")"

instance Show Prop where
    show = showProp

test_ShowProp :: Bool
test_ShowProp =
    show (Not (Var "P") :&: Var "Q") == "((~P)&Q)"


type Env = [(Nume, Bool)] --asociere (variabila, val_de_adev)

impureLookup :: Eq a => a -> [(a,b)] -> b
impureLookup a = fromJust . lookup a --ar trebui cu un Maybe in caz ca nu exista cheia a
--fromJust e un maybe a si intoarce un a => nesigura la Nothing


--ex3
--eval calc val de adevar a expresiei Prop data ca parametru
eval :: Prop -> Env -> Bool
eval (Var p) e = impureLookup p e
eval F _ = False
eval T _ = True
eval (Not p) e = not (eval p e)
eval (p :|: q) e = (eval p e) || (eval q e)
eval (p :&: q) e = (eval p e) && (eval q e)
eval (p :->: q) e = not (eval p e) || (eval q e) --not p sau q
eval (p :<->: q) e = (not(eval p e) || (eval q e)) && (not(eval q e) || (eval p e))-- p implica q si q implica p => (not p sau q) si (not q sau p)


test_eval :: Bool
test_eval = eval  (Var "P" :|: Var "Q") [("P", True), ("Q", False)] == True

--ex4 -- satisfibailitate
variabile :: Prop -> [Nume]
variabile T = []
variabile F = [] 
variabile (Var x) = [x]
variabile (Not p) = variabile p
variabile (p :|: q) = nub(variabile p ++ variabile q) -- ca sa nu fie duplicate
variabile(p :&: q) = nub(variabile p ++ variabile q)
variabile(p :->: q) = nub(variabile p ++ variabile q)
variabile(p :<->: q) = nub(variabile p ++ variabile q)
 
test_variabile :: Bool
test_variabile =
  variabile (Not (Var "P") :&: Var "Q") == ["P", "Q"]

--ex5
envs :: [Nume] -> [Env]
envs nume = 
    let submultimi = subsequences nume 
    in map mkEnv submultimi
 where
     mkEnv submultime = map(valuation submultime) nume

     valuation :: [Nume] -> Nume -> (Nume, Bool)
     valuation subm n 
        | n `elem` subm = (n, True)
        | otherwise = (n, False) -- trb sa le sortam ca sa mearga test_envs pt ca [1,2,3] /= [2,3,1]

--envs ["P", "Q", "R"]


test_envs :: Bool
test_envs = 
    envs ["P", "Q"]
    ==
    [ [ ("P",False)
      , ("Q",False)
      ]
    , [ ("P",False)
      , ("Q",True)
      ]
    , [ ("P",True)
      , ("Q",False)
      ]
    , [ ("P",True)
      , ("Q",True)
      ]
    ]

--ex 6
satisfiabila :: Prop -> Bool
satisfiabila x = any(==True)(map(eval x) (envs (variabile x)))

--ex 7
valida :: Prop -> Bool
valida x = satisfiabila(Not x) == False

test_satisfiabila1 = satisfiabila (Not (Var "P") :&: Var "Q") == True
test_satisfiabila2 = satisfiabila (Not (Var "P") :&: Var "P") == False

--tema 8,9,10

--8

variabile' :: Prop -> String
variabile' T = ""
variabile' F = "" 
variabile' (Var x) =  x
variabile' (Not p) = variabile' p
variabile' (p :|: q) = nub(variabile' p ++ " " ++ variabile' q) -- ca sa nu fie duplicate
variabile' (p :&: q) = nub(variabile' p ++ " " ++ variabile' q)
variabile' (p :->: q) = nub(variabile' p ++ " " ++ variabile' q)
variabile' (p :<->: q) = nub(variabile' p ++ " " ++ variabile' q)

tabelAdevar :: Prop -> String
tabelAdevar x =  variabile' x ++ " " ++ show x ++ "\n" ++ tabelEvaluari x (envs $ variabile x)

tabelfinal :: Prop -> IO ()
tabelfinal p = putStrLn$ tabelAdevar  p

tabelEvaluari :: Prop -> [Env] -> String
tabelEvaluari _ [] = ""
tabelEvaluari p (x:xs) = tabelEvaluari2 x ++ show(eval p x) ++ "\n" ++ tabelEvaluari p xs

tabelEvaluari2 ::  Env -> String --pt a parcurge fiecare lista din lista de liste
tabelEvaluari2  [] = " "
tabelEvaluari2 (x:xs) = fst x ++ " " ++ show(snd x) ++ " " ++ tabelEvaluari2 xs



{- tabelEvaluari :: Prop -> [Env] -> String
tabelEvaluari p (x:xs) = foldr ((++) . tabelEvaluari2 p) "" xs

tabelEvaluari2 :: Prop -> Env -> String --pt a parcurge fiecare lista din lista de liste
tabelEvaluari2 p [] = show(eval p x) ++ "\n"
tabelEvaluari2 p (x:xs) = fst x ++ " " ++ show(snd x) ++ " " ++ tabelEvaluari2  p xs  -}

--9

--mai sus am facut + modificat celelalte functii(eval, variabile, show, variabile')

p4 :: Prop
p4 = (Var "P" :->: Var "Q")

p5 :: Prop
p5 = (Var "P" :<->: Var "Q")

--tabelfinal p4
--tabelfinal p5


--10

echivalenta :: Prop -> Prop -> Bool
echivalenta p q = echivalenta1 evaluari (p :<->: q)
    where 
      evaluari = envs $ variabile (p :<->: q) -- da True pe toate testele
  
echivalenta1 :: [Env] -> Prop -> Bool
echivalenta1 e p = and [eval p x |x <- e]
 
test_echivalenta1 =
  True
  ==
  (Var "P" :&: Var "Q") `echivalenta` (Not (Not (Var "P") :|: Not (Var "Q")))
test_echivalenta2 =
  False
  ==
  (Var "P") `echivalenta` (Var "Q")
test_echivalenta3 =
  True
  ==
  (Var "R" :|: Not (Var "R")) `echivalenta` (Var "Q" :|: Not (Var "Q"))


