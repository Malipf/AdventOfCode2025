import System.Environment (getArgs)

getCell :: [String] -> (Int, Int) -> Char
getCell grid (x,y) = grid !! y !! x

getAdjacents :: [String] -> (Int, Int) -> (Int, Int) -> String
getAdjacents grid bounds = map (getCell grid) . adjacents bounds

adjacents :: (Int,Int) -> (Int,Int) -> [(Int,Int)]
adjacents (ax,bx) (a,b) =
  [ (x,y)
  | x <- [a-1..a+1], x<=ax && x>=0
  , y <- [b-1..b+1], y<=bx && y>=0
  ]

countRolls :: String -> Int
countRolls = length . filter (== '@')

bounds :: [String] -> (Int, Int)
bounds grid = (length (head grid) - 1, length grid - 1)

getRolls :: (Int, Int) -> [String] -> [(Int, Int)]
getRolls (a,b) grid =
  [ (x,y)
  | x <- [0..a]
  , y <- [0..b]
  , getCell grid (x,y) == '@'
  ]

getAdjacentCounts  :: [String] -> [Int]
getAdjacentCounts =
  liftA2 map
    (\g -> countRolls . getAdjacents g (bounds g))
    (\g -> getRolls (bounds g) g)

inverse :: ([a],[b]) -> [(a,b)]
inverse ([],[]) = []
inverse (xs, ys) = zip xs ys

removables :: [String] -> [(Int,Int)]
removables =
    map fst
      . filter ((< 5) . snd)
      . inverse
      . liftA2 (,) (\g -> getRolls (bounds g) g) getAdjacentCounts

removeRoll :: (Int,Int) -> [String] -> [String]
removeRoll (x,y) grid  =
  let (fr, row : lr) = splitAt y grid
      (fc, _   : lc) = splitAt x row
   in (fr ++ (fc ++ '.' : lc) : lr)

removeRolls :: [(Int,Int)] -> [String] -> [String]
removeRolls rs grid = foldl (flip removeRoll) grid rs

loop :: [String] -> Int -> ([String], Int)
loop grid sum =
  let rs = removables grid
   in case length rs of
        0 -> ([], sum)
        x -> loop (removeRolls rs grid) (sum + x)

main :: IO ()
main = do
  args <- getArgs
  content <- readFile . head $ args
  let grid = lines content
  putStrLn ("part 1: " ++ (show . length . removables $ grid))
  putStrLn ("part 2: " ++ (show . snd . loop grid $ 0))

