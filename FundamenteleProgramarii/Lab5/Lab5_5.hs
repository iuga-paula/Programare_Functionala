type Name = String

type Environment = [(Name, Value)]

newtype StringWriter a = StringWriter { runStringWriter :: (a,String)}

instance Monad StringWriter where
  return x = StringWriter (x, "")
  ma >>= k = let (va,str1) = runStringWriter ma -- obtinem a-ul 
                 (vb, str2) = runStringWriter (k va)
             in  StringWriter (vb, str1 ++ str2)

instance Applicative (StringWriter) where
    pure = return
    mf <*> ma = do
    f <- mf
    va <- ma
    return (f va)  


instance Functor (StringWriter) where
    fmap f ma = pure f <*> ma   

type M = StringWriter

data Term = Var Name
          | Con Integer
          | Term :+: Term
          | Lam Name Term
          | App Term Term
          | Out Term
  deriving (Show)

data Value = Num Integer
           | Fun (Value -> M Value)
           | Wrong

instance Show Value where
 show (Num x) = show x
 show (Fun _) = "<function>"
 show Wrong   = "<wrong>"

showM :: Show a => M a -> String
--showM ma = show $ runStringWriter ma
showM ma = let (a,b) = runStringWriter ma in "Output: " ++ show b ++ " Val:" ++ show  a 

instance Show a => Show (StringWriter a) where
    show = showM

tell :: String -> StringWriter()
tell str = StringWriter((), str)


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

interp (Out u) e = do
    a <- interp u e -- evalueaza e in out
    tell(show a ++ ";")
    return a

add :: Value -> Value -> M Value
add (Num i1) (Num i2) = return (Num (i1+i2))
add _ _ = return Wrong


app :: Value -> Value -> M Value
app (Fun f) v = f v 