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
        getLine >>= (\strin -> putStrLn("Intrare\n" ++ strin))
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