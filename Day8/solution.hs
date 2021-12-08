import System.IO

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
  "" -> []
  s' -> w : wordsWhen p s''
    where
      (w, s'') = break p s'

processLine :: String -> (String, String)
processLine line =
  case wordsWhen (== '|') line of
    [observations, output] -> (observations, output)
    _ -> error $ "processLine - input error: line doesn't contains '|' separator, line: " ++ show (words line)

isAOne :: String -> Bool
isAOne segment = length segment == 2

isAFour :: String -> Bool
isAFour segment = length segment == 4

isASeven :: String -> Bool
isASeven segment = length segment == 3

isAEight :: String -> Bool
isAEight segment = length segment == 7

isRecognizableDigit :: String -> Bool
isRecognizableDigit d = isAOne d || isAFour d || isASeven d || isAEight d

countRecognizableDigits :: (String, String) -> Int
countRecognizableDigits (_, output) =
  let ws = filter isRecognizableDigit (words output)
   in length ws

main = do
  contents <- readFile "input.txt"
  let digitCount = sum $ map (countRecognizableDigits . processLine) (lines contents)

  putStrLn $ "Digit count: " ++ show digitCount
