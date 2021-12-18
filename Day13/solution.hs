import Data.List
import System.IO

type Point = (Int, Int)

type FoldAxis = (String, Int)

readInt :: String -> Int
readInt = read

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
  "" -> []
  s' -> w : wordsWhen p s''
    where
      (w, s'') = break p s'

replace :: [a] -> Int -> a -> [a]
replace xs i e = case splitAt i xs of
  (before, _ : after) -> before ++ e : after
  _ -> xs

fromListToPoint :: [String] -> Point
fromListToPoint [] = error "fromListToPoint - empty list!"
fromListToPoint [a, b] = (readInt a, readInt b)
fromListToPoint _ = error "fromListToPoint - list with more than two elements"

fromInstructionToFoldAxis :: String -> FoldAxis
fromInstructionToFoldAxis instr =
  let tokens = words instr
      rawAxis = tokens !! 2 -- "fold along axis=n"
      tokenAxis = wordsWhen (== '=') rawAxis
   in (head tokenAxis, readInt (tokenAxis !! 1))

foldingStep :: [Point] -> FoldAxis -> [Point]
foldingStep points (axis, offset) =
  let reflectPoint (x, y)
        | axis == "x" = if x > offset then (2 * offset - x, y) else (x, y)
        | axis == "y" = if y > offset then (x, 2 * offset - y) else (x, y)
        | otherwise = error $ "foldingStep - invalid folding axis" ++ show (axis, offset)
   in map reflectPoint points

foldProcess :: [Point] -> [FoldAxis] -> [Point]
foldProcess = foldl foldingStep

findEdgePoint :: [Point] -> Point -> Point
findEdgePoint [] (xMax, yMax) = (xMax, yMax)
findEdgePoint ((x, y) : xs) (xMax, yMax) =
  let newXMax = if x > xMax then x else xMax
      newYMax = if y > yMax then y else yMax
   in findEdgePoint xs (newXMax, newYMax)

buildEmptyGrid y (xMax, yMax) acc =
  let newRow = concat $ replicate (xMax + 1) ". "
   in if y == yMax then newRow : acc else buildEmptyGrid (y + 1) (xMax, yMax) (newRow : acc)

stringifyGrid :: [String] -> String
stringifyGrid = intercalate "\n"

printPointInGrid :: [String] -> Point -> [String]
printPointInGrid grid (x, y) =
  let newRow = replace (grid !! y) (x * 2) '#' -- remember the spaces in the grid!
   in replace grid y newRow

printPoints :: [String] -> [Point] -> [String]
printPoints = foldl printPointInGrid

main = do
  contents <- readFile "input.txt"
  let rawLines = lines contents
  let instructions = map fromInstructionToFoldAxis (filter (isPrefixOf "fold") rawLines)
  let points = map (fromListToPoint . wordsWhen (== ',')) (filter (\l -> not ("fold" `isPrefixOf` l) && not (null l)) rawLines)
  let stateAfterFirstFold = nub $ foldingStep points (head instructions)
  let finalState = nub $ foldProcess points instructions
  let edgePoint = findEdgePoint finalState (0, 0)
  let grid = buildEmptyGrid 0 edgePoint []
  let finalGrid = printPoints grid finalState
  let secretCode = stringifyGrid finalGrid
  putStrLn $ "Number of visible points after the first fold: " ++ show (length stateAfterFirstFold)
  putStrLn $ "Secret code:\n" ++ secretCode