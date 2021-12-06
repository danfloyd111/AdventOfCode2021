import System.IO

-- Collect sliding window measurements
collectSlidWinMeasurements [x, y, z] acc = acc ++ [x + y + z]
collectSlidWinMeasurements (x : y : z : xs) acc =
  collectSlidWinMeasurements (y : z : xs) (acc ++ [x + y + z])
collectSlidWinMeasurements _ _ = error "collectSlidWinMeasurements has been called with wrong arguments"

-- Core function
countDepthIncreases _ [] acc = acc
countDepthIncreases n (x : xs) acc =
  if x > n
    then countDepthIncreases x xs (acc + 1)
    else countDepthIncreases x xs acc

-- Specify the type to help the compiler
readInt :: String -> Int
readInt = read

main = do
  contents <- readFile "./input.txt"

  let hd : tl = map readInt . words $ contents -- head and tail
  let increases = countDepthIncreases hd tl 0

  let sh : th = collectSlidWinMeasurements (hd : tl) []
  let refinedIncreases = countDepthIncreases sh th 0

  putStrLn $ "Number of increases: " ++ show increases
  putStrLn $ "Number of increases using sliding windows: " ++ show refinedIncreases