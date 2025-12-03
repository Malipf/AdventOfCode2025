import System.Environment (getArgs)

findN :: Int -> String -> String
findN 0 x = ""
findN n x =
  let first = findBiggest . reverse . drop (n-1) . reverse $ x
      rest = findN (n-1) (tail (dropWhile (/=first) x))
   in first : rest

findBiggest :: String -> Char
findBiggest "" = '0'
findBiggest (x:xs) = max x (findBiggest xs)

result x = show . sum . map (read . findN x)

main :: IO ()
main = do
  args <- getArgs
  content <- readFile . head $ args
  let l = lines content 
  putStrLn ("part 1: " ++ result 2 l)
  putStrLn ("part 2: " ++ result 12 l)
