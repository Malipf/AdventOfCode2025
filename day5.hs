import System.Environment (getArgs)
import Data.List (sort)

getRanges :: [String] -> [(Int,Int)]
getRanges = map separateDash

separateDash :: String -> (Int, Int)
separateDash x =
  let start = read (takeWhile (/='-') x)
      end   = read (tail (dropWhile (/='-') x))
   in (start, end)

separateLines :: [String] -> ([String], [Int])
separateLines ls =
  let ranges = takeWhile (/="") ls
      ids = map read . tail . dropWhile (/="") $ ls
   in (ranges, ids)

checkRanges :: [(Int,Int)] -> Int -> Bool
checkRanges [] _ = False
checkRanges ((s,e):xs) id = (id >= s && id <= e) || checkRanges xs id

countFreshs :: ([String], [Int]) -> Int
countFreshs (ranges, ids) = length . filter (checkRanges (getRanges ranges)) $ ids

concatRanges :: [(Int,Int)] -> [(Int,Int)]
concatRanges [] = []
concatRanges [x] = [x]
concatRanges (x1:x2:xs) =
  if checkRanges [x1] (fst x2)
    then concatRanges ((fst x1, max (snd x1) (snd x2)) : xs)
    else x1 : concatRanges (x2:xs)

countRanges :: [(Int,Int)] -> Int
countRanges [] = 0
countRanges ((a,b):xs) = b-a+1 + countRanges xs

result1 = show . countFreshs . separateLines . lines
result2 = show . countRanges . concatRanges . sort . getRanges . fst  . separateLines . lines

main :: IO ()
main = do
  args <- getArgs
  content <- readFile (head args)
  putStrLn ("part 1: " ++ result1 content)
  putStrLn ("part 2: " ++ result2 content)
