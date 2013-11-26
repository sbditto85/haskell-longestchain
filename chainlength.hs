import Data.Ord
import Data.List

testGrid = [[8,2,4]
    	   ,[0,7,1]
           ,[3,7,9]]
		   
fromGrid :: [[Int]] -> (Int,Int) -> Int
fromGrid l (x,y) = l !! y !! x

longestFirst :: [a] -> [b] -> Ordering
longestFirst lista listb
	| la < lb = GT
	| la > lb = LT
	| la == lb = EQ
	where
		la = length lista
		lb = length listb
		
--longestChain :: [[Int]] -> [(Int,Int)]
longestChain :: [[Int]] -> Int
longestChain grid = 
	let l = sortBy longestFirst $ findChains grid in
	--if length l > 0 then head l else []
	if length l > 0 then length $ head l else 0
	where
		findChains :: [[Int]] -> [[(Int,Int)]]
		findChains grid = 
			let startPos = [(x,y) | x <- [0..lenX-1], y <- [0..lenY-1]] in
			foldl (\acc (x',y') -> (fromPosition' grid (x',y') []) : acc ) [] startPos
			where
				lenY = length grid
				lenX = length $ grid !! 0
	
fromPosition' :: [[Int]] -> (Int,Int) -> [(Int,Int)] -> [(Int,Int)]
fromPosition' grid (x,y) sofar =
	let
		val = fromGrid grid (x,y)
		sofar' = sofar ++ [(x,y)]
		t = if isValidLink grid (x+1,y) val sofar then fromPosition' grid (x+1,y) sofar' else sofar'
		b = if isValidLink grid (x-1,y) val sofar then fromPosition' grid (x-1,y) sofar' else sofar'
		l = if isValidLink grid (x,y-1) val sofar then fromPosition' grid (x,y-1) sofar' else sofar'
		r = if isValidLink grid (x,y+1) val sofar then fromPosition' grid (x,y+1) sofar' else sofar'
		tl = if isValidLink grid (x+1,y-1) val sofar then fromPosition' grid (x+1,y-1) sofar' else sofar';
		tr = if isValidLink grid (x+1,y+1) val sofar then fromPosition' grid (x+1,y+1) sofar' else sofar';
		bl = if isValidLink grid (x-1,y-1) val sofar then fromPosition' grid (x-1,y-1) sofar' else sofar';
		br = if isValidLink grid (x-1,y+1) val sofar then fromPosition' grid (x-1,y+1) sofar' else sofar';
	in
		head $ sortBy longestFirst [t,b,l,r,tl,tr,bl,br]
	where
		isValidLink :: [[Int]] -> (Int,Int) -> Int -> [(Int,Int)] -> Bool
		isValidLink grid (x,y) val sofar =
			if inBounds grid (x,y) && notElem (x,y) sofar && (fromGrid grid (x,y)) >= val then True else False
			where
				inBounds :: [[Int]] -> (Int,Int) -> Bool
				inBounds grid (x'',y'') = if x'' >= 0 && x'' < lenX && y'' >= 0 && y'' < lenY then True else False
					where
						lenY = length grid
						lenX = length $ grid !! 0
					
main :: IO ()
--main = print $ map (\(x,y) -> fromGrid testGrid (x,y)) $ longestChain testGrid
main = print $ longestChain testGrid