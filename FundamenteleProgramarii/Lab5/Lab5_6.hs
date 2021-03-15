newtype IntState a = IntState { runIntState :: Integer -> (a,Integer) }

type M = IntState

showM :: Show a => M a -> String
showM ma = let (a, new) = runIntState ma 0 --evaluarea in starea 0
            in "Value: " ++ show a ++ " Count: " ++ show new

instance (Show a) => Show(IntState a) where
    show = showM

instance Monad(IntState) where
    return x = IntState (\s -> (x, s)) --return întoarce valoarea dată si propagă starea neschimbată
    ma >>= k = IntState f
       where 
           f state = let (a, val) = runIntState ma state
                         (new, valnew) = runIntState (k a) val
                      in (new, valnew)  

instance Applicative (IntState) where
    pure = return
    mf <*> ma = do
    f <- mf
    va <- ma
    return (f va)  


instance Functor (IntState) where
    fmap f ma = pure f <*> ma   

modify :: (Integer -> Integer) -> IntState()
modify f = IntState(\s -> ((), f s))

tickS :: IntState() -- creste contorul
tickS =  IntState(\s -> ((), s + 1))

get :: IntState Integer --obt val cur a contorului
get = IntState(\s -> (s,s))

data Term = Var Name
          | Con Integer
          | Term :+: Term
          | Lam Name Term
          | App Term Term
          | Count  --nr de pasi executati pana acum
  deriving (Show)


data Value = Num Integer
           | Fun (Value -> M Value)
           | Wrong

instance Show Value where
 show (Num x) = show x
 show (Fun _) = "<function>"
 show Wrong   = "<wrong>"

type Name = String

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

interp Count env  = do
    i <- get --obt val cur a contorului si intoarce  num i
    return (Num i)



add :: Value -> Value -> M Value
add (Num i1) (Num i2) = tickS >> return (Num (i1+i2))
add _ _ = return Wrong


app :: Value -> Value -> M Value
app (Fun f) v = tickS >> f v 
app _ _ = return Wrong