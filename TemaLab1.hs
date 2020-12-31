data Alegere = Piatra | Foarfeca | Hartie 
                deriving (Eq, Show)

data Rezultat = Victorie | Infrangere | Egalitate 
                deriving (Eq,Show)

partida :: Alegere -> Alegere -> Rezultat
partida a b 
    | a == Piatra && b== Foarfeca = Victorie
    | a == Piatra && b == Hartie = Infrangere
    | a == Foarfeca && b== Piatra = Infrangere
    | a == Foarfeca && b== Hartie = Victorie
    | a == Hartie && b== Foarfeca = Infrangere
    | a == Hartie && b== Piatra = Victorie
    | otherwise = Egalitate