import System.IO
import Control.Monad

-- used to simulate a path of infinit length
infinity :: Float
infinity = 999999.0

-- list containing of edges, from node x to y with distance z
edgeList :: [(Int, Int)] -> [(Int, Int)] -> Float -> [(Int, Int, Float)]
edgeList [] _ capacity = []
edgeList _ [] capacity = []
edgeList (x:xs) locations capacity= [(a, b, w) | n <- xs, n /= x, let w = getDistance x n, w <= (capacity / 2), let a = getIndex x locations 0, let b = getIndex n locations 0] ++ edgeList xs locations capacity

-- get the index of the location in the list (needed to create a 2D array)
getIndex :: (Int, Int) -> [(Int, Int)] -> Int -> Int
getIndex (v, w) (x:xs) i = if((v, w) == x) then i else getIndex (v, w) xs (i+1)

-- get the distance between two locations.
getDistance :: (Int, Int) -> (Int, Int) -> Float
getDistance (a, b) (c, d) = fromInteger(round(sqrt(fromIntegral((c - a)^2 + (d - b)^2))*100.0))/100.0

-- find the number of units of food required to cross the desert
bfs :: [(Int, Int, Float)] -> [(Int, Float)] -> Float -> Float
bfs [] storage capacity= fromIntegral(ceiling(checkStorage storage 0))
bfs _ [] capacity = infinity
bfs ((v, w, d):xs) storage capacity = 
	if (2.0*d >= capacity)
	then bfs xs storage capacity
	else if(((checkStorage storage w) + 2.0*d) <= capacity)
		then bfs xs (updateStorage storage v ((checkStorage storage w) + d)) capacity
		else if(3.0*d >= capacity)
     		then bfs xs storage capacity
     		else bfs xs (updateStorage storage v ((capacity - d)*fromIntegral(ceiling((capacity - 3.0*d)/(checkStorage storage w))) + ((checkStorage storage w) + d - (capacity - 3.0 * d) * fromIntegral(ceiling((capacity - 3.0*d)/(checkStorage storage w)))))) capacity	 		  

-- a list containing nodes with their current food storage. (initially all are infinit except for the destination)
foodStorage :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Float)]
foodStorage [] [] = []
foodStorage (x:xs) locations = [(i, s) | let i = getIndex x locations 0, let s = if(xs /= []) then infinity else 0] ++ foodStorage xs locations

-- check how much food is stored at a certain location
checkStorage :: [(Int,Float)] -> Int -> Float
checkStorage ((x, s):xs) i = if(x == i) then s else checkStorage xs i 

-- update the amount of food that is stored at a certain node when new amount is smaller than old amount.
updateStorage :: [(Int,Float)] -> Int -> Float -> [(Int,Float)]
updateStorage [] _ _ = []
updateStorage ((x, s):xs) i ns = if(x == i && ns < s) then [(x, ns)] ++ xs else [(x, s)] ++ updateStorage xs i ns

coordinates [x,y] = (readInt x, readInt y)

crossDesert [] = return ()
crossDesert rows = do
	putStrLn "New trial"

	-- total number of significant locations
	let noSigLoc = fst $ head rows
	let cap = fromIntegral $ snd $ head rows 
	putStrLn "Capacity:"
	print cap

	putStrLn ""

	-- list containing the significant locations for this trip
	let sigLocs = take noSigLoc (drop 1 rows)

	let elist = edgeList sigLocs sigLocs cap;
	let fstorage = foodStorage sigLocs sigLocs;
	let food = bfs (reverse elist) fstorage cap;

 	if food == infinity
		then
			print("Impossible")
		else do
			putStrLn "Units of food needed to cross the desert:"
			print(food)

	putStrLn ""

	crossDesert $ drop (noSigLoc + 1) rows

main :: IO ()
main = do
	putStrLn "Enter number of locatons and the max. capacity (e.g. 4 100):"
	line <- getLine

	if line == "0 0"
		then do
			putStrLn "Terminating";
			return()
		else do
			let count = readInt $ head $ words line
			lines <- replicateM (count) $ do
				putStrLn "Enter the location of an oasis (e.g. 10 -20):"

				newLine <- getLine
				return newLine

			crossDesert $ map (coordinates . words) $ line : lines
			main

readInt :: String -> Int
readInt = read