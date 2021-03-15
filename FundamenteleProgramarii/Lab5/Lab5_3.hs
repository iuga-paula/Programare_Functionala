type M a = [a] --monada list

{-instance Monad [] where
    return x = [x]
    xs >>= k = concat $ map k xs --secventiere => se aplica fs k pe losta de obt o lista de lsite de b - uri si se concat tot intr-o lista 
afislist :: Show a => [a] -> String
afislist (x:xs) = show x ++ afislist xs

showM :: Show a => M a -> String
showM (list) = afislist list-}

type Name = String

data Term = Var Name
          | Con Integer
          | Term :+: Term
          | Lam Name Term
          | App Term Term
          | Fail
          | Amb Term Term
  deriving (Show)

data Value = Num Integer
           | Fun (Value -> M Value)
           | Wrong -- pr e gresit din pct de vedere al sintaxei

instance Show Value where
 show (Num x) = show x
 show (Fun _) = "<function>"

type Environment = [(Name, Value)]

pgm1:: Term
pgm1 = (App (Lam "x" ((Var "x") :+: (Var "x")))
        (Amb (Con 10)(Con 11)))

interp :: Term -> Environment -> M Value
interp (Var n) env = case lookup n env of
    Just v -> return  v
    Nothing -> return Wrong

interp (Con i) _ = return (Num i)

interp (t1 :+: t2) e = do
    v1 <- interp t1 e 
    v2 <- interp t2 e 
    add v1 v2

interp (Lam n t) e = return $ Fun(\v -> interp t ((n,v):e)) 

interp (App t1 t2) e = do
    v1 <- interp t1 e
    v2 <- interp t2 e 
    app v1 v2

interp Fail _ = []

interp (Amb t1 t2) e = (interp t1 e) ++ (interp t2 e) 


add :: Value -> Value -> M Value
add (Num i1) (Num i2) = return (Num (i1+i2))
add _ _ = return  Wrong


app :: Value -> Value -> M Value
app (Fun f) v = f v 
app _ _ =  return Wrong