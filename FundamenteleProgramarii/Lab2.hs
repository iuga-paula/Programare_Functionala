import Data.Maybe
import Data.List

--name alis pt string adica variabile din progr
--prgm este program are o lista de variabile si o instructiune stmt
--stmt skip e instr vida/ inlantuire de instructiuni (inlantuite prin ::: in loc de un ; normal)
--            /ifuri cu expresie intr instr/ while-uri expresie instr care se repeta cat timp expr e adev
--              / atriburi variabila := expresie
--expr Aexpr nume intregi Lit/ imultire/adunare a 2 exp aritm sau o valoarea unei variabile
-- o expr de tip bool Bexpr adev/fals/ not Bexp sau == intre e expresii


type Name = String

data  Pgm  = Pgm [Name] Stmt
        deriving (Read, Show)

data Stmt = Skip | Stmt ::: Stmt | If BExp Stmt Stmt | While BExp Stmt | Name := AExp
        deriving (Read, Show)

data AExp = Lit Integer | AExp :+: AExp | AExp :*: AExp | Var Name
        deriving (Read, Show)

data BExp = BTrue | BFalse | AExp :==: AExp | Not BExp
        deriving (Read, Show)

infixr 2 :::
infix 3 :=
infix 4 :==:
infixl 6 :+:
infixl 7 :*:


type Env = [(Name, Integer)]


factStmt :: Stmt
factStmt =
  "p" := Lit 1 ::: "n" := Lit 3 :::
  While (Not (Var "n" :==: Lit 0))
    ( "p" := Var "p" :*: Var "n" :::
      "n" := Var "n" :+: Lit (-1)
    )

--p ia val 1 si n ia val 3
--cat timp n != 0  se rep p= p*n, n-=n
   
pg1 = Pgm [] factStmt 
--pg1 calc n!

aEval :: AExp -> Env -> Integer
aEval (Lit int) env = int --snd (head env) 
aEval (a :+: b) env =  aEval a env + aEval b env
aEval (a :*: b) env = aEval a env * aEval b env
aEval (Var x) env = fromJust (lookup x env)

bEval :: BExp -> Env -> Bool
bEval (BTrue) env = True 
bEval (BFalse) env = False
bEval (a :==: b) env = (aEval a env) == (aEval b env)
bEval (Not b) env = not (bEval b env)

sEval :: Stmt -> Env -> Env --o expresie cu valorile variabilelor env se intoarce env
sEval (Skip) env = env
sEval (a ::: b) env = (sEval b evnou)
    where evnou = (sEval a env)

sEval (If bexp a b) env = if (bEval bexp env) == True then sEval a env else sEval b env
sEval (While bexp a) env = if (bEval bexp env) == True then sEval (While bexp a) (sEval a env) else env
sEval (var := exp) env = [(var, aEval exp env)]

pEval :: Pgm -> Env  --eval unui program
pEval (Pgm variabile stmt) = sEval stmt (f variabile)
    where f :: [Name] -> Env
          f [] = []
          f (x:xs) = [(x,0)] ++ f xs 


class Applicative m => Monad m where
    (>>=) :: m a -> (a -> m b) -> m b 
    (>>) :: m a -> m b -> m b -- fs asta e deja impl nu trb scrisa iar la instante
    --ma >> mb = ma >>= \_ -> mb nu mai depinde de tipul primului elem
    return :: a -> m a

{- 
    m Maybe (constructor de tip)
    a Int (tip)
    m a = Just 5 
    return 5 in clasa Maybe va fi Just 5
-}

   -- y = just 5
f :: Int -> Maybe Int
f x = if x > 10 then Just (x*2) else Nothing

    --f y nu merge !!!
    --y>>=f = Nothing