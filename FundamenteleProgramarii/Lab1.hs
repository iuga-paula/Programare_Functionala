
data Prog = On Instr
--constr on urmate de o insturctiune on => se poerneste un program
data Instr = Off | Expr :> Instr
--off finalul unui program sau expr inlantuite
data Expr = Mem | V Int | Expr :+ Expr
--mem ce e calulcat v o variabila si + se aduna expresii
p1 :: Prog
p1 = On ((V 3) :> ((Mem :+ (V 5)):> Off))

type Env = Int -- valoarea celulei de memorie

type DomProg = [Int]
--val unui program => o lista de itnregi pt fiecare inst din program
type DomInstr = Env -> [Int]

type DomExpr = Env -> Int

prog :: Prog -> DomProg
prog (On instr) = stmt instr 0

stmt :: Instr -> DomInstr
stmt Off a = []
stmt (ex :> instr) a = let e = (expr ex a)
                           i = stmt instr e
                       in e : i 

expr :: Expr -> DomExpr
expr Mem a = a
expr (V a) b = a
expr (a :+ b) c = (expr a c) + (expr b c)




type Name = String
data Hask = HTrue
 | HFalse
 | HLit Int
 | HIf Hask Hask Hask
 | Hask :==: Hask
 | Hask :+: Hask
 | HVar Name
 | HLam Name Hask
 | Hask :$: Hask
  deriving (Read, Show)
infix 4 :==:
infixl 6 :+:
infixl 9 :$:

data Value = VBool Bool
 | VInt Int
 | VFun (Value -> Value)
 | VError -- pentru reprezentarea erorilor

instance Show Value where
    show (VBool b) = show b
    show (VInt i) = show i
    show (VFun f) = "avem functie"
    show (VError) = "eroare"
    
instance Eq Value where
    (Vbool b1) == (Vbool b2) = b1 == b2
    (VInt i1) == (VInt i2) = b1 == b2
    (VFun _) == (VFun _) = error "nu putem egala functii"
    VError == VError = error ""
    _ == _ = False

type HEnv = [(Name, Value)]

type DomHask = HEnv -> Value

hEval :: Hask -> DomHask
hEval HTrue env = VBool True
hEval HFalse env = VBool False
hEval (HLit i) = Vint i
hEval (HIf cond c1 c2) env = evalIf (hEval cond env) (hEval c1 env) (hEval c2 env)
    where
        evalIf (VBool b) c d = if b then c else d
        evalIf _ _ _ = VError --prima comp nu se ev la Bool e un if incorect
        
hEval (c1 :==: c20) env = evalEq (hEval c1 env) (hEval c2 env)
    where evalEq (VInt i1) (VInt i2) = VBool (i1 == i2)
          evalEq _ _ = VError
          
hEval (c1 :+: c20) env = evalAdd (hEval c1 env) (hEval c2 env)
    where evalAdd (VInt i1) (VInt i2) = VInt (i1 + i2)
          evalAdd _ _ = VError

--o variabila string care se cauta intr-o lista de perechi
hEval (HVar n) env = case lookup n env of
                     Just v -> v 
                     Nothing -> VError
                     
hEval (HLam x e) env = VFun(\ v -> hEval e ((x,v) : env)) --ev lui v tine cont de per x,v

hEval (c1 :$: c2) env = evalApp (hEval c1 env) (hEval c2 env)
    where
        evalApp (VFun f) = f v
        evalApp _ _ = VError

h = (HLam "x" (HLam "y" (HVar "x" :+: Hvar "y"))) :$: (HLit 3) :$: (HLit 4)
--sau
h` = (HLam "x" (HLam "y" (HVar "x" :+: Hvar "y"))) :$: (HLit 3) :$: (HLit 4)

--se apleaza hEval h [] -  7
-- hEval h [] == VInt 7 - true
main = do
print $ "Hello"
print $ "Ana"