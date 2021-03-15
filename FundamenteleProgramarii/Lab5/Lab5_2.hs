--pt monada Maybe sunt implementate instantele functor si etc

type M = Either String --componenta de eroare e left si un String
--data Either a b = Right b | Left a


showM :: (Show a) => M a -> String
showM (Left x) = show x
showM (Right x) = show x

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

--wrongul este Left str in caz de eroare

type Environment = [(Name, Value)]


interp :: Term -> Environment -> M Value
interp (Var n) env = case lookup n env of
    Just v -> return  v
    Nothing -> Left "eroare"

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
add _ _ =  Left "eroare"


app :: Value -> Value -> M Value
app (Fun f) v = f v 
app _ _ =  Left "eroare"


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