import Data.List (transpose)
import System.IO

type Point = (Int, Int)

readInt :: String -> Int
readInt = read

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
  "" -> []
  s' -> w : wordsWhen p s''
    where
      (w, s'') = break p s'

readPoint :: String -> Point
readPoint str =
  let ls = map readInt (wordsWhen (== ',') str)
   in (head ls, last ls)

filterOutArrow :: [String] -> [String]
filterOutArrow = filter (/= "->")

buildPointsList :: [String] -> [Point]
buildPointsList = map readPoint

unzipPointsList :: [[Point]] -> [Point] -> [Point] -> ([Point], [Point])
unzipPointsList [] startPts endPts = (startPts, endPts)
unzipPointsList (x : xs) startPts endPts =
  let ss = startPts ++ [head x]
      es = endPts ++ [last x]
   in unzipPointsList xs ss es

replace :: [a] -> Int -> a -> [a]
replace xs i e = case splitAt i xs of
  (before, _ : after) -> before ++ e : after
  _ -> xs

increasePointValue :: [Int] -> Int -> [Int]
increasePointValue xs i = case splitAt i xs of
  (before, e : after) -> before ++ (e + 1) : after
  _ -> xs

findEdgeX :: [Point] -> Int -> Int
findEdgeX pts max =
  foldl
    (\max pt -> let x = fst pt in if x > max then x else max)
    max
    pts

findEdgeY :: [Point] -> Int -> Int
findEdgeY pts max =
  foldl
    (\max pt -> let y = snd pt in if y > max then y else max)
    max
    pts

isNotDiagonal :: [Point] -> Bool
isNotDiagonal [ptA, ptB] = fst ptA == fst ptB || snd ptA == snd ptB
isNotDiagonal _ = error "isNotDiagonal - Invalid input: not a line"

isAt45Deg :: [Point] -> Bool
isAt45Deg [ptA, ptB] =
  let distX = abs (fst ptA - fst ptB)
      distY = abs (snd ptA - snd ptB)
   in distX == distY
isAt45Deg _ = error "isAt45Deg - Invalid input: not a line"

drawPoint :: Point -> [[Int]] -> [[Int]]
drawPoint (x, y) matrix = replace matrix x (increasePointValue (matrix !! x) y)

-- this draws just horizontal or vertical lines
-- [start, end] matrix -> matrix
drawHVLine :: [Point] -> [[Int]] -> [[Int]]
drawHVLine [start, end] matrix =
  if start == end
    then drawPoint start matrix
    else
      let (sx, sy) = start
          (ex, ey) = end
          newMatrix = drawPoint start matrix
       in if sx == ex
            then
              if sy > ey
                then drawHVLine [(sx, sy - 1), end] newMatrix
                else drawHVLine [(sx, sy + 1), end] newMatrix
            else
              if sx > ex
                then drawHVLine [(sx - 1, sy), end] newMatrix
                else drawHVLine [(sx + 1, sy), end] newMatrix
drawHVLine _ _ = error "drawHVLine - input error: not horizontal nor vertical line"

-- this draws just 45deg lines
-- [start, end] matrix -> matrix
draw45DegLine :: [Point] -> [[Int]] -> [[Int]]
draw45DegLine [start, end] matrix =
  if start == end
    then drawPoint start matrix
    else
      let (sx, sy) = start
          (ex, ey) = end
          newMatrix = drawPoint start matrix
       in if sx > ex
            then
              if sy < ey
                then draw45DegLine [(sx - 1, sy + 1), end] newMatrix
                else draw45DegLine [(sx - 1, sy - 1), end] newMatrix
            else
              if sy > ey
                then draw45DegLine [(sx + 1, sy - 1), end] newMatrix
                else draw45DegLine [(sx + 1, sy + 1), end] newMatrix
draw45DegLine _ _ = error "draw45DegLine - input error: not 45deg line"

drawHVLines :: [[Point]] -> [[Int]] -> [[Int]]
drawHVLines ts matrix = foldl (flip drawHVLine) matrix ts

drawIncLines :: [[Point]] -> [[Int]] -> [[Int]]
drawIncLines ts matrix = foldl (flip draw45DegLine) matrix ts

{-
  #WARNING, the specs assume that a (x, y) means (column-index, row-index). Here in this program the computations
  are done using inverted index, because the specs are really bad written, I have to transpose the resulting matrix
  at the end of computations to get the result in the same format as the one specified (badly).
-}

main = do
  contents <- readFile "input.txt"
  let directives = lines contents

  let dirTuples = map (buildPointsList . filterOutArrow . words) directives

  -- for the first part let's keep just horizontal and vertical lines
  let hvDirTuples = filter isNotDiagonal dirTuples

  -- for the second part we need also the 45deg lines (inclined)
  let incDirTuples = filter isAt45Deg dirTuples

  let (startPts, endPts) = unzipPointsList dirTuples [] [] -- using the unconstrained tuple list to build the board
  let startEdgeX = findEdgeX startPts 0
      startEdgeY = findEdgeY startPts 0
      endEdgeX = findEdgeX endPts 0
      endEdgeY = findEdgeY endPts 0

  putStrLn $ "X edges from startpoints/from endpoints " ++ show startEdgeX ++ " / " ++ show endEdgeX
  putStrLn $ "Y edges from startpoints/from endpoints " ++ show startEdgeY ++ " / " ++ show endEdgeY

  -- let's select the higher edge (dimension) and build a square matrix full of zeroes
  let dim = maximum [startEdgeX, startEdgeY, endEdgeX, endEdgeY] + 1
  putStrLn $ "Building a " ++ show dim ++ " * " ++ show dim ++ " matrix..."

  let matrix = [[0 | j <- [1 .. dim]] | i <- [1 .. dim]]

  putStrLn "Calculating the solution (it may takes some minutes)..."

  let matrixWithLines = drawHVLines hvDirTuples matrix
  let matrixWithAlsoInclinedLines = drawIncLines incDirTuples matrixWithLines

  let transposed = transpose matrixWithAlsoInclinedLines
  let dangerousPoints = filter (>= 2) (concat transposed)

  putStrLn $ "Number of dangerous points: " ++ show (length dangerousPoints)