
type Name = String

type Environment = [(Name, Value)]

newtype EnvReader a = Reader { runEnvReader :: Environment -> a}

instance Monad EnvReader where
  return x = Reader (\ _ -> x )
  ma >>= k = Reader f
   where 
       f env = let va = runEnvReader ma env -- obtinem a-ul 
                   vb = runEnvReader (k va) env
               in vb

{- instance Monad EnvReader where
    return x = Reader(\_ -> x)
    ma >>= k Reader f
        where
            f env = let va = runEnvReader ma env
                        vb = runEnvReader (k va) env
                    in vb -}

instance Applicative (EnvReader) where
    pure = return
    mf <*> ma = do
    f <- mf
    va <- ma
    return (f va)  


instance Functor (EnvReader) where
    fmap f ma = pure f <*> ma   

type M = EnvReader

data Term = Var Name
          | Con Integer
          | Term :+: Term
          | Lam Name Term
          | App Term Term
  deriving (Show)

showM :: Show a => M a -> String
showM ma = show $ runEnvReader ma [] --in show se transmite environmentul


ask :: EnvReader Environment
ask = Reader(\e -> e)

local :: (Environment -> Environment) -> EnvReader a -> EnvReader a 
local f ma = Reader (\e -> let e1 = f e in runEnvReader ma e1) -- se ruleaza readerul in noul mediu e1

data Value = Num Integer
           | Fun (Value -> M Value)
           | Wrong

instance Show Value where
 show (Num x) = show x
 show (Fun _) = "<function>"
 show Wrong   = "<wrong>"



interp :: Term -> M Value --environmentul este in M
interp (Var n) = do
    env <- ask --prin sageata se extrage comp de tip a => adica extragem environment
    case lookup n env of
        Just v -> return v 
        Nothing -> return Wrong

interp (Con i)  = return (Num i)

interp (t1 :+: t2) = do
    v1 <- interp t1
    v2 <- interp t2
    add v1 v2

interp (Lam n t) = do
    env <- ask
    return $ Fun(\v -> local (\_ -> ((n,v):env)) (interp t) ) --se executa t-ul folosind memoria resp modificata

interp (App t1 t2) = do
    v1 <- interp t1
    v2 <- interp t2 
    app v1 v2

add :: Value -> Value -> M Value
add (Num i1) (Num i2) = return (Num (i1+i2))
add _ _ = return Wrong


app :: Value -> Value -> M Value
app (Fun f) v = f v 
app _ _ = return Wrong

test :: Term -> String
test t = showM $ interp t

