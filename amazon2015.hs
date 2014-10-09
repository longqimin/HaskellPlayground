module Amazon where

nextSymmetric :: String -> String
nextSymmetric xs | even $ length xs = if firstHalfR > secondHalf 
									  then firstHalf ++ firstHalfR
									  else firstHalf' ++ reverse firstHalf'
	where
		(firstHalf, secondHalf) = splitAt (length xs `div` 2) xs
		firstHalfR = reverse firstHalf
		firstHalf' = show $ read firstHalf + 1
nextSymmetric xs = if xs < firstHalf ++ tail firstHalfR
				   then firstHalf ++ tail firstHalfR
				   else firstHalf' ++ (tail $ reverse firstHalf')
	where
		firstHalf = take (length xs `div` 2 + 1) xs
		firstHalfR = reverse firstHalf
		firstHalf' = show $ read firstHalf + 1


type Position = (Int, Int)

minX :: Int -> Int
minX n = (3 ^ n - 1) `div` 2

maxX :: Int -> Int
maxX n = (3 ^ (n+1) - 3) `div` 2

parentPosition :: Position -> Position
parentPosition p@(x, y) = (x-1, y `div` 3)
	
ncaByPosition :: Position -> Position -> Position
ncaByPosition pa@(da, wa) pb@(db, wb) | da < db = ncaByPosition pa (parentPosition pb)
ncaByPosition pa@(da, wa) pb@(db, wb) | da > db = ncaByPosition (parentPosition pa) pb
ncaByPosition pa@(da, wa) pb@(db, wb) | wa == wb = pa
									  | wa `div` 3 == wb `div` 3 = parentPosition pa
									  | otherwise = ncaByPosition (parentPosition pa) (parentPosition pb)

--nearest common ancestor
nca :: Int -> Int -> Int
nca a b = numAtPosition $ ncaByPosition pa pb
	where
		pa = positionOfNum a
		pb = positionOfNum b

positionOfNum :: Int -> Position
positionOfNum n = (x, y)
	where
		x = head $ filter (\k -> n <= maxX k) [0..]
		y = if even x then n - minX x else maxX x - n

numAtPosition :: Position -> Int
numAtPosition (x, y) = if even x then minX x + y else maxX x - y


