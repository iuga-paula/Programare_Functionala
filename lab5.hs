--Matrice
matrice ::Num a => [[a]]
matrice = [[1,2,3,4], [0,2,3,4], [7,7,7,7]]

--1
corect :: [[a]] -> Bool
corect [] = True
corect (x:xs) = length[l |  l <- xs, length l /= length x] == 0
-- ia listele care au lungimea dif de prima lista

--2
poz :: [[a]] -> Int -> Int -> a
poz l row col = (l !! row) !! col

--3
transforma :: [[a]] -> [(a, Int, Int)]
transforma mat = [(mat !! row !! col, row, col) | row <- [0..rows-1], col <- [0..cols-1]]
   where
      rows = length mat
      cols = length (head mat)