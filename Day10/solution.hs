import Data.List (sort)
import System.IO

initTokens = ['(', '[', '{', '<']

termTokens = [')', ']', '}', '>']

isTermToken :: Char -> Bool
isTermToken t = t `elem` termTokens

isInitToken :: Char -> Bool
isInitToken t = t `elem` initTokens

-- returns the closing token for t
getTermToken :: Char -> Char
getTermToken t = case t of
  '(' -> ')'
  '[' -> ']'
  '{' -> '}'
  '<' -> '>'
  _ -> error "getTermToken - Not a valid init token"

getErrorPoints :: Char -> Int
getErrorPoints t = case t of
  ')' -> 3
  ']' -> 57
  '}' -> 1197
  '>' -> 25137
  _ -> error "getErrorPoints - Not a valid init token"

getCompletionPoints :: Char -> Int
getCompletionPoints t = case t of
  ')' -> 1
  ']' -> 2
  '}' -> 3
  '>' -> 4
  _ -> error "getErrorPoints - Not a valid init token"

calcCorruptionPoints :: [Char] -> [Char] -> Int
calcCorruptionPoints [] expected = 0 -- this is an incomplete line but still valid
calcCorruptionPoints str [] = 0 -- this is a valid string
calcCorruptionPoints (x : xs) expected@(y : ys)
  | isInitToken x = calcCorruptionPoints xs (getTermToken x : expected)
  | x == y = calcCorruptionPoints xs ys -- a chunk is being closed
  | otherwise = getErrorPoints x

-- TODO: refactoring: all can be done with just this one and exception catching
getCompletionSegment :: [Char] -> [Char] -> [Char]
getCompletionSegment [] [] = [] -- this is just a valid string
getCompletionSegment (x : xs) [] = getCompletionSegment xs [getTermToken x]
getCompletionSegment [] expected = expected -- this is an incomplete line but still valid
getCompletionSegment (x : xs) expected@(y : ys)
  | isInitToken x = getCompletionSegment xs (getTermToken x : expected)
  | x == y = getCompletionSegment xs ys -- a chunk is being closed
getCompletionSegment _ _ = error "getCompletionSegment - Corrupted line"

getErrorScore :: [Char] -> Int
getErrorScore [] = error "getErrorScore - Empty line"
getErrorScore (l : ls) = calcCorruptionPoints ls [getTermToken l]

calcCompletionSegmentScore :: [Char] -> Int -> Int
calcCompletionSegmentScore xs score =
  foldl (\score x -> (score * 5) + getCompletionPoints x) score xs

getAutocompleteScore :: [Char] -> Int
getAutocompleteScore [] = error "getAutocompleteScore - Empty line"
getAutocompleteScore (l : ls) =
  let completionSegment = getCompletionSegment ls [getTermToken l]
   in calcCompletionSegmentScore completionSegment 0

main = do
  contents <- readFile "input.txt"

  let lns = lines contents

  let errors = map getErrorScore lns
  let errorScore = sum errors

  putStrLn $ "Error score: " ++ show errorScore

  -- part 2

  let incompletes = filter (\l -> getErrorScore l == 0) lns
  let autoCompleteScores = sort $ map getAutocompleteScore incompletes
  let middleScore = autoCompleteScores !! (length autoCompleteScores `div` 2)

  putStrLn $ "Autocomplete score: " ++ show middleScore
