import Data.Char (digitToInt)
import System.IO

type Point = (Int, Int)

readInt :: String -> Int
readInt = read

-- e.g. "12345" to [1,2,3,4,5]
toIntList :: String -> [Int]
toIntList = map digitToInt

replace :: [a] -> Int -> a -> [a]
replace xs i e = case splitAt i xs of
  (before, _ : after) -> before ++ e : after
  _ -> xs

getNeighbors :: [[Int]] -> Point -> [Point]
getNeighbors g point@(x, y)
  | point == (0, 0) = [right, down]
  | point == (0, yMax) = [left, down]
  | point == (xMax, 0) = [up, right]
  | point == (xMax, yMax) = [up, left]
  | x == 0 = [left, down, right]
  | x == xMax = [left, up, right]
  | y == 0 = [up, down, right]
  | y == yMax = [up, down, left]
  | otherwise = [up, right, down, left]
  where
    xMax = length g - 1
    yMax = length (head g) -1
    right = (x, y + 1)
    down = (x + 1, y)
    left = (x, y -1)
    up = (x -1, y)

replaceValueInSquareMatrix :: [[a]] -> Point -> a -> [[a]]
replaceValueInSquareMatrix m (x, y) val =
  let updatedRow = replace (m !! x) y val
   in replace m x updatedRow

buildSquareMatrix :: a -> Int -> [[a]]
buildSquareMatrix baseValue n =
  let row = replicate n baseValue
   in replicate n row

getLessRiskyUnvisitedPoint :: [[(Bool, Int)]] -> Point
getLessRiskyUnvisitedPoint m =
  let minValue = (maxBound :: Int)
      n = length m
      f minval minpoint (r, c)
        | (r, c) == (n - 1, n - 1) =
          if v < minval && not visited
            then (r, c)
            else minpoint
        | c == n - 1 =
          if v < minval && not visited
            then f v (r, c) (r + 1, 0)
            else f minval minpoint (r + 1, 0)
        | otherwise =
          if v < minval && not visited
            then f v (r, c) (r, c + 1)
            else f minval minpoint (r, c + 1)
        where
          visited = fst $ m !! r !! c
          v = snd $ m !! r !! c
   in f minValue (0, 0) (0, 0)

updateDangerLevel [] chitonsGrid dangerGrid point = dangerGrid
updateDangerLevel neighbors@((x, y) : ns) chitonsGrid dangerGrid (r, c) =
  let alt = snd (dangerGrid !! r !! c) + (chitonsGrid !! x !! y)
      (visited, currentDanger) = dangerGrid !! x !! y
      dangerGrid' =
        if alt < currentDanger
          then replaceValueInSquareMatrix dangerGrid (x, y) (visited, alt)
          else dangerGrid
   in updateDangerLevel ns chitonsGrid dangerGrid' (r, c)

searchLessDangerousPath :: [[Int]] -> [[(Bool, Int)]] -> (Int, Int) -> Int
searchLessDangerousPath chitonsGrid dangerGrid (r, c)
  | (r, c) == (n -1, n -1) = snd (dangerGrid !! r !! c)
  | otherwise =
    let dangerVal = snd $ dangerGrid !! r !! c
        dangerGrid' = replaceValueInSquareMatrix dangerGrid (r, c) (True, dangerVal) -- mark visited
        -- get unvisited neighbors
        neighbors = filter (\(x, y) -> not (fst $ dangerGrid' !! x !! y)) (getNeighbors chitonsGrid (r, c))
        dangerGrid'' = updateDangerLevel neighbors chitonsGrid dangerGrid' (r, c)
        nextPoint = getLessRiskyUnvisitedPoint dangerGrid''
     in searchLessDangerousPath chitonsGrid dangerGrid'' nextPoint
  where
    n = length dangerGrid

main = do
  content <- readFile "input.txt"
  let chitons = map toIntList (lines content)
  -- specs document says that input is a n*n grid so it's safe to read just a "dimension", number of rows in this case
  let n = length chitons
  -- danger matrix tracks both risk level and if the position has been visited
  let danger = replaceValueInSquareMatrix (buildSquareMatrix (False, maxBound :: Int) n) (0, 0) (False, 0)

  let minimumDanger = searchLessDangerousPath chitons danger (0, 0)
  putStrLn $ "Less dangerous path has a risk level of: " ++ show minimumDanger