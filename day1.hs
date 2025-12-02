import System.Environment 

rotate1 :: [String] -> Int -> Int -> ([String], Int, Int)
rotate1 [] c state = ([], c, state)
rotate1 (x:xs) c state =
  let num    = read . tail $ x
      isPlus = head x == 'R'
      state2 = if isPlus then mod (state + num) 100 else mod (state - num) 100
      c2     = if state2 == 0 then c + 1 else c
   in rotate1 xs c2 state2

rotate2 :: [String] -> Int -> Int -> ([String], Int, Int)
rotate2 [] c state = ([], c, state)
rotate2 (x:xs) c state =
  let num    = read . tail $ x
      isPlus = head x == 'R'
      num2   = if isPlus then state + num else state - num
      state2 = mod num2 100
      c2     = if num2 == 0 then c + 1 else c
      c3     = if state /= 0 && num2 < 0 then c2 + 1 else c2
      num3   = abs num2 `div` 100
      c4     = c3 + num3
   in rotate2 xs c4 state2

result f x = show ((\(a,b,c) -> b) (f (map (head . words) . lines $ x) 0 50))
      
main = do
  path <- getArgs
  contents <- readFile . head $ path
  putStrLn ("part 1: " ++ result rotate1 contents)
  putStrLn ("part 2: " ++ result rotate2 contents)

