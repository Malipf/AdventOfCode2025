import System.Environment

addInvalids :: [Int] -> Int -> (Int -> Bool) -> Int
addInvalids [] sum _ = sum
addInvalids (x:xs) sum f =
  if f x
    then addInvalids xs (x + sum) f
    else addInvalids xs sum f

invalid1 :: Int -> Bool
invalid1 x =
  let x2 = show x 
      l = length x2 `div` 2
   in take l x2  == drop l x2

invalid2 :: Int -> Bool
invalid2 x =
  let x2 = show x
      l = length x2 `div` 2
   in any (checkNDigits x2) [1..l]

checkNDigits :: String -> Int -> Bool
checkNDigits x = allSame . filter (not . null) . splitString x

allSame :: [String] -> Bool
allSame [] = True
allSame [x] = True
allSame (x:y:ys) = (x == y) && allSame (y:ys)

splitString :: String -> Int -> [String]
splitString x n =
  if n > length x
    then [x]
    else take n x : splitString (drop n x) n

seperateCommas :: String -> [String]
seperateCommas s =
  case dropWhile (==',') s of
    "" -> []
    s' -> w : seperateCommas s''
      where (w, s'') = break (==',') s'

convertInterval :: String -> [Int]
convertInterval x = [read (takeWhile (/='-') x) .. read (tail (dropWhile (/='-') x))]

main :: IO ()
main = do
  args <- getArgs
  contents <- readFile . head $ args
  let l = seperateCommas contents
  let l2 = concatMap convertInterval l
  putStrLn ("part 1: " ++ show (addInvalids l2 0 invalid1))
  putStrLn ("part 2: " ++ show (addInvalids l2 0 invalid2))
