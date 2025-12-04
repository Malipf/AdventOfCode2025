import System.Environment (getArgs)

getCell :: [String] -> (Int, Int) -> Char
getCell grid (x,y) = grid!!y!!x

getAdjecents :: [String] -> (Int, Int) -> (Int, Int) -> String
getAdjecents grid borders = map (getCell grid) . adjecents borders

adjecents :: (Int,Int) -> (Int,Int) -> [(Int,Int)]
adjecents (ax,bx) (a,b) = [(x,y)|x <- [a-1..a+1], x<=ax && x>=0, y <- [b-1..b+1], y<=bx, y>=0]

countRolls :: String -> Int
countRolls [] = 0
countRolls (x:xs) = if x == '@' then 1 + countRolls xs else countRolls xs

borders :: [String] -> (Int, Int)
borders grid = (length (head grid) - 1, length grid - 1)

getRolls :: (Int, Int) -> [String] -> [(Int, Int)]
getRolls (a,b) grid = [(x,y)|x <- [0..a], y <- [0..b], getCell grid (x,y) == '@']

getAdjecentCounts  :: [String] -> [Int]
getAdjecentCounts =
  liftA2 map
    (\g -> countRolls . getAdjecents g (borders g))
    (\g -> getRolls (borders g) g)

inverse :: ([a],[b]) -> [(a,b)]
inverse ([],[]) = []
inverse (x:xs, y:ys) = (x,y) : inverse (xs, ys)

removables :: [String] -> [(Int,Int)]
removables =
    map fst
      . filter ((< 5) . snd)
      . inverse
      . liftA2 (,) (\g -> getRolls (borders g) g) getAdjecentCounts

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
  let l = lines content
  putStrLn ("part 1: " ++ (show . length . removables $ l))
  putStrLn ("part 2: " ++ (show . snd . loop l $ 0))

