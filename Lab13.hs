import Data.Char
-- >>= functia BIND face o computatie
-- >> ignora ce s-a intamplat inainte
prelStr strin = map toUpper strin

ioString = do
           strin <- getLine
           putStrLn $ "Intrare\n" ++ strin
           let  strout = prelStr strin
           putStrLn $ "Iesire\n" ++ strout

prelNo noin = sqrt noin

ioString' = 
        getLine >>= (\strin -> putStrLn("Intrare\n" ++ strin)
        >> let strout = prelStr  strin
        in putStrLn ("Iseire\n" ++ strout))

ioNumber = do
           noin <- readLn :: IO Double
           putStrLn $ "Intrare\n" ++ (show noin)
           let  noout = prelNo noin
           putStrLn $ "Iesire"
           print noout

inoutFile = do
              sin <- readFile "Input.txt"
              putStrLn $ "Intrare\n" ++ sin
              let sout = prelStr sin
              putStrLn $ "Iesire\n" ++ sout
              writeFile "Output.txt" sout


type Input = String
type Output = String
 
newtype MyIO a = MyIO { runIO :: Input -> (a, Input, Output)}

myGetChar :: MyIO Char
myGetChar = MyIO f
    where 
        f  :: Input -> (Char, Input, Output)
        f str =
            let
                first = head str
                rest = drop 1 str
                rez = "" 
            in
                (first, rest, rez)

myGetChar' :: MyIO Char
myGetChar' = MyIO f
    where 
        f :: Input -> (Char, Input, Output)
        f str =
            case str of 
                [] -> error "Empty input"
                (c:cs) -> (c, cs, [])


--punem c in output
myPutChar :: Char -> MyIO ()
myPutChar c = MyIO f
    where 
        f :: Input -> ((), Input, Output)
        f str = ((), str ,  [c]) 


instance Functor MyIO where
    fmap f (MyIO mIO) = MyIO g
        where 
            --mIO :: Input -> (a, Input, Output)
            --f :: a -> b
            -- g :: Input -> (b, Input, Output)
            g input =
                let (rez, newInput, output) = mIO input
                in (f rez, newInput, output)

--runIO (fmap toUpper myGetChar) "ana"

instance Applicative MyIO where
    pure a = MyIO f
        where
            -- f :: (a, Input, Output)
            f input = (a, input, [])
    MyIO fatoB <*>  MyIO fa = MyIO fb
        where
            fb input = 
                let
                    (resAToB, newInput, output) = fatoB input
                    (resA, newnewInput, newOutput) = fa newInput
                in  (resAToB resA, newnewInput, output ++ newOutput)
      --fa :: Input -> (a, Input, Output)
    --fb :: Input -> (b, Input, Output)


instance Monad MyIO where
     = pure 
    MyIO fa >>= atoMb = MyIO fb
        where
            --atoMb :: a-> MyIO b
            fb input = 
                let (rez, newInput, newOutput) = fa newInput
                    (newResult, newNewInput, newOutput) = runIO (atoMb rez) newInput
                in (newResult, newNewInput, output ++ newOutput)