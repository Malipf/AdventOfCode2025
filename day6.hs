import System.Environment (getArgs)

evaluate1 :: [String] -> Int
evaluate1 [l1,l2,l3,l4,l5] =
  zip5
    (map readOp5 . words $ l5)
    (map read . words $ l1)
    (map read . words $ l2)
    (map read . words $ l3)
    (map read . words $ l4)

readOp5 :: String -> (Int -> Int -> Int -> Int -> Int)
readOp5 "+" = \ a b c d -> a + b + c + d
readOp5 "*" = \ a b c d -> a * b * c * d

zip5 :: [Int -> Int -> Int -> Int -> Int] -> [Int] -> [Int] -> [Int] -> [Int] -> Int
zip5 [] _ _ _ _ = 0
zip5 (f:fs) (l1:l1s) (l2:l2s) (l3:l3s) (l4:l4s) = f l1 l2 l3 l4 + zip5 fs l1s l2s l3s l4s

blocks :: String -> [String]
blocks s =
  case dropWhile (\ a -> a == '+' || a == '*') s of
    "" -> []
    s' -> w : blocks s''
      where (w, s'') = break (\ a -> a == '+' || a == '*') s'

seperateBlocks :: [String] -> [Int] -> [[String]]
seperateBlocks _ [] = []
seperateBlocks [l1,l2,l3,l4,l5] (x:xs) =
  [take x l1, take x l2, take x l3, take x l4, take x l5] :
  seperateBlocks [drop (x+1) l1, drop (x+1) l2, drop (x+1) l3, drop (x+1) l4, drop (x+1) l5] xs

evaluateBlock :: [String] -> Int
evaluateBlock x =
  case head (x!!4) of
    '+' -> sum nums
    '*' -> product nums
    where nums = map read . takeNumbers $ x

evaluateAll :: [[String]] -> Int
evaluateAll = sum . map evaluateBlock

result2 :: [String] -> Int
result2 x =
  let s = map length . blocks $ (x!!4 ++ " ")
      b = seperateBlocks x s
   in evaluateAll b 

takeNumber :: [String] -> Int -> String
takeNumber [_] _ = ""
takeNumber (x:xs) n = x!!n : takeNumber xs n

takeNumbers :: [String] -> [String]
takeNumbers x = map (takeNumber x) [0..length (head x) - 1]

main :: IO ()
main = do
  args <- getArgs
  content <- readFile . head $ args
  let l = lines content
  putStrLn ("part 1: " ++ (show . evaluate1 $ l))
  putStrLn ("part 2: " ++ (show . result2 $ l))
