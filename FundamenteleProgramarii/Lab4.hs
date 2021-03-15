newtype Reader env a = Reader { runReader :: env -> a }
--env memorie locala care nu se modifica


instance Monad (Reader env) where
  return x = Reader (\_ -> x)
  ma >>= k = Reader f
    where f env = let a = runReader ma env
                  in  runReader (k a) env



instance Applicative (Reader env) where
  pure = return
  mf <*> ma = do
    f <- mf
    a <- ma
    return (f a)

instance Functor (Reader env) where
  fmap f ma = pure f <*> ma


ask :: Reader env env
ask = Reader id
--ask = Reader(\x -> x)
---DIN LAB 3---
local :: (r -> r) -> Reader r a -> Reader r a
local f ma = Reader $ (\r -> (runReader ma)(f r))

-- Reader Person String

data Person = Person { name :: String, age :: Int }

showPersonN :: Person -> String
showPersonN (Person nume varsta) = "NAME:" ++ nume 

--showPersonN $ Person "ada" 20
--"NAME:ada"

showPersonA :: Person -> String
showPersonA (Person nume varsta) = "Age:" ++ show varsta 

--showPersonA $ Person "ada" 20
--"Age:20"

showPerson :: Person -> String
showPerson pers = "(" ++ (showPersonN pers) ++ "," ++ (showPersonA pers) ++ ")"

--showPerson $ Person "ada" 20 
--"(NAME:ada,Age:20)"

mshowPersonN ::  Reader Person String
mshowPersonN = Reader(\p -> "Nume:" ++ name p)

---runReader mshowPersonN $ Person "ada" 20 
--"Nume:ada"

mshowPersonA ::  Reader Person String
mshowPersonA = do
    p <- ask -- intoarce persoana din readerul dat
    return ("Nume:" ++ show (age p))

    --sau
    --ask >>= \p -> return ("Age:" ++ (show (age p)))
    --env e pers data ca param

--runReader mshowPersonA $ Person "ada" 20 
--"Nume:20"

mshowPerson :: Reader Person String
mshowPerson = do
    n <- mshowPersonN
    a <- mshowPersonA
    return ("(" ++ n ++ "," ++ a ++ ")")

--runReader mshowPerson $ Person "ada" 20  
--"(Nume:ada,Nume:20)"


----DIN LAB 4----
--- Monada Identity

newtype Identity a = Identity { runIdentity :: a }



--- Limbajul si  Interpretorul

type M = Identity 

showM :: Show a => M a -> String
showM (Identity a) = show a

instance Show a => Show (Identity a) where
    show x = showM x

instance Monad Identity where
    return x = Identity x
    Identity a >>= k = k a 

instance Applicative (Identity) where
    pure = return
    mf <*> ma = do
    f <- mf
    va <- ma
    return (f va)  


instance Functor(Identity) where
    fmap f ma = pure f <*> ma   

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
           | Wrong

instance Show Value where
 show (Num x) = show x
 show (Fun _) = "<function>"
 show Wrong   = "<wrong>"

type Environment = [(Name, Value)]



interp :: Term -> Environment -> M Value
interp (Var n) env = case lookup n env of
    Just v -> return v 
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

add :: Value -> Value -> M Value
add (Num i1) (Num i2) = return (Num (i1+i2))
add _ _ = return Wrong


app :: Value -> Value -> M Value
app (Fun f) v = f v 
app _ _ = return Wrong


-- test :: Term -> String
-- test t = showM $ interp t []

pgm1:: Term
pgm1 = App
          (Lam "x" ((Var "x") :+: (Var "x")))
          ((Con 10) :+:  (Con 11))
-- test pgm
-- test pgm1

