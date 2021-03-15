--pt monada Maybe sunt implementate instantele functor si etc

type M = Maybe

showM :: (Show a) => M a -> String
showM (Just x) = show x
showM (Nothing) = "eroare"

type Name = String

data Term = Var Name
          | Con Integer
          | Term :+: Term
          | Lam Name Term
          | App Term Term
  deriving (Show)

pgm :: Term
pgm = App
  (Lam "y"
    (App
      (App
        (Lam "f"
          (Lam "y"
            (App (Var "f") (Var "y"))
          )
        )
        (Lam "x"
          (Var "x" :+: Var "y")
        )
      )
      (Con 3)
    )
  )
  (Con 4)


data Value = Num Integer
           | Fun (Value -> M Value)

--se elimina wrongul este Nothing in caz de eroare

type Environment = [(Name, Value)]


interp :: Term -> Environment -> M Value
interp (Var n) env = lookup n env
--la lookup rez e chiar de tip Maybe
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

add :: Value -> Value -> M Value
add (Num i1) (Num i2) = return (Num (i1+i2))
add _ _ =  Nothing


app :: Value -> Value -> M Value
app (Fun f) v = f v 
app _ _ =  Nothing


pgm1:: Term
pgm1 = App
          (Lam "x" ((Var "x") :+: (Var "x")))
          ((Con 10) :+:  (Con 11))

pgm2:: Term
pgm2 = App
          (Lam "x" ((Var "x") :+: (Var "y")))
          ((Con 10) :+:  (Con 11))

pgm3:: Term
pgm3 = App
          ((Var "x") :+: (Var "y"))
          ((Con 10) :+:  (Con 11))