import Data.List (elemIndex, sort)
import Data.Text (pack, replace, unpack)
import System.IO

readInt :: String -> Int
readInt = read

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

-- second part

-- returns (middle, topL)
findMiddleAndTopL :: [String] -> Char -> Char -> (Char, Char, String)
findMiddleAndTopL digits a b =
  let notContainsBoth str = a `notElem` str || b `notElem` str
   in let digit0 = sort $ head $ filter notContainsBoth digits
       in if a `elem` digit0 then (b, a, digit0) else (a, b, digit0)

-- removeSubstringFromString :: String -> String -> String
-- removeSubstringFromString sub str = unpack $ replace (pack sub) (pack "") (pack str)

removeStr1FromStr2 :: String -> String -> String
removeStr1FromStr2 str1 = filter (`notElem` str1) --unpack $ replace (pack sub) (pack "") (pack str)

decodeDigits :: [String] -> [String]
decodeDigits observations =
  let digit1 = sort $ head $ filter isAOne observations -- which is also topRbottomR or bottomRtopR
      digit7 = sort $ head $ filter isASeven observations
      digit8 = sort $ head $ filter isAEight observations

      top = head $ removeStr1FromStr2 digit1 digit7

      digit4 = sort $ head $ filter isAFour observations

      middleAndTopL = removeStr1FromStr2 digit1 digit4

      len66 = filter (\x -> length x == 6) observations -- contains digits "0", "6" and "9"
      len6 = map sort len66
      (middle, topL, digitZero) = findMiddleAndTopL len6 (head middleAndTopL) (last middleAndTopL)
      digit0 = sort digitZero

      digits6and9 = filter (/= digit0) len6

      findTopRAndBottomR = findMiddleAndTopL -- we use the same process
      (topR, bottomR, digitSix) = findTopRAndBottomR digits6and9 (head digit1) (last digit1)
      digit6 = sort digitSix

      subs1 = sort $ [top] ++ [topR] ++ [topL] ++ [middle] ++ [bottomR]

      digit9 = sort . head $ filter (/= digit6) digits6and9
      bottom = head $ removeStr1FromStr2 subs1 digit9

      subs2 = sort $ subs1 ++ [bottom]

      bottomL =
        head $ removeStr1FromStr2 subs2 "abcdefg"

      digit2 = sort $ [top] ++ [topR] ++ [middle] ++ [bottomL] ++ [bottom]
      digit3 = sort $ [top] ++ [topR] ++ [middle] ++ [bottomR] ++ [bottom]
      digit5 = sort $ [top] ++ [topL] ++ [middle] ++ [bottomR] ++ [bottom]
   in [digit0, digit1, digit2, digit3, digit4, digit5, digit6, digit7, digit8, digit9]

fun :: [String] -> String -> Char
fun ds d =
  let i = elemIndex d ds
   in case i of
        Just 0 -> '0'
        Just 1 -> '1'
        Just 2 -> '2'
        Just 3 -> '3'
        Just 4 -> '4'
        Just 5 -> '5'
        Just 6 -> '6'
        Just 7 -> '7'
        Just 8 -> '8'
        Just 9 -> '9'
        _ -> error $ "not a digit!" ++ show (i, d, ds)

computeOutput :: [String] -> [String] -> [Char]
computeOutput digits = map (fun digits)

decodeOutput :: (String, String) -> Int
decodeOutput (obs, out) =
  let sortedOut = map sort (words out)
      digits = decodeDigits $ words obs
   in readInt $ computeOutput digits sortedOut

-- TODO: Refactor all this mess!!!

{-
  refined algorithm:
  length 2 -> 1
  length 3 -> 7
  length 4 -> 4
  length 5 ->
  if 2 letters match with 1 it's 3,
  else if 3 letters match with 4 it's 5,
  else it's 2
  length 6 ->
  if 1 letter matches with 1 it's 6
  else if 4 letters match with 4 it's 9
  else it's 0
  length 7 -> 8
-}

main = do
  contents <- readFile "input.txt"

  -- first part
  let ls = map processLine (lines contents)
  let digitCount = sum $ map countRecognizableDigits ls

  -- second part
  let (obs, out) = processLine "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"
  let porcocazzo = map processLine ["acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"]
  let dgs = decodeDigits $ words obs
  let sortedOut = map sort (words out)
  let computedNum = readInt (computeOutput dgs sortedOut)

  -- let digits = decodeDigits $ words obs
  let outputSum = sum $ map decodeOutput ls

  putStrLn $ "Digit count: " ++ show digitCount
  putStrLn $ "Sum of decoded outputs list: " ++ show outputSum

{-

  - test: acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab

  - the segment with length 2 is the digit "1", so if it is "ab" then segments a and b are on the right
  - the segment with length 3 is the digit "7", so if it is "dab" we can remove "ab" because they were the segments
    associated with "1" and we obtain "d" which must be the top segment
  - the segment with length 4 is the digit "4", so if its "eafb" with the same reasoning we know that "e" or "f" are
    top left and middle or viceversa
  - length 6 are "0", "6" or "9" so "cefabd", "cdfgeb", "cagedb" 6 and 9 have both the top left segment and the
    middle one, but zero doesn't have the middle one! So who's missing "e" or "f"? "cagedb" has "e" but not "f" so
    we know that "cagedb" is 0, and "f" is the middle segment, while "g" is top-left segment!
  - situation:  top: "d"
                top-right: "a" / "b"
                top-left: "e"
                middle: "f"
                bottom: ?
                bottom-right: "a" / "b"
                bottom-left: ?
    digits found: 1,4,7,8,
  - with a similar reasoning, digit "6" misses just one of "a" or "b" segments and that must be the bottom right so
    "cefabd" has both while "cdfgeb" misses "a" ! So "a" is top-right, "b" is bottom-right "cdfegb" is digit "6" and
    by exclusion "cefabd" is 9!
  - situation:  top: "d"
                top-right: "a"
                top-left: "e"
                middle: "f"
                bottom: ?
                bottom-right: "b"
                bottom-left: ?
    digits found: 0,1,4,6,7,8,9
  - digit "3" "2" and "5" missing
  - "9" is "cefabd" so there's only one possibility for the segment "c" and is the bottom! So by exclusion "g" is bottom
    left"
  - situation:  top: "d"
              top-right: "a"
              top-left: "e"
              middle: "f"
              bottom: "c"
              bottom-right: "b"
              bottom-left: "g"
  digits found: 0,1,2,3,4,5,6,7,8,9
-}
