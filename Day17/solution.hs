import System.IO

readInt :: String -> Int
readInt = read

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
  "" -> []
  s' -> w : wordsWhen p s''
    where
      (w, s'') = break p s'

-- takes a string in the format "x=A..B" or "y=A..B" where A and B are integers and returns (A,B)
parseStartAndEndCoords :: String -> (Int, Int)
parseStartAndEndCoords rawSpan =
  let span = wordsWhen (== '.') (wordsWhen (== '=') rawSpan !! 1)
      start = readInt $ head span
      end = readInt $ last span
   in (start, end)

-- calculates the sum of the first n integers
gaussSum :: Int -> Int
gaussSum n = (n * (n + 1)) `div` 2

main = do
  inputLine <- readFile "input.txt"

  let inputTokens = wordsWhen (== ',') inputLine
      (xTargetMin, xTargetMax) = parseStartAndEndCoords (words (head inputTokens) !! 2)
      (yTargetMin, yTargetMax) = parseStartAndEndCoords (last inputTokens)

  let maxHeight = gaussSum (- yTargetMin -1)

  putStrLn $ "Maximum height: " ++ show maxHeight