import Data.List (sort, transpose)
import Data.Map (fromList, fromListWith, toList)
import Data.Maybe (isJust)
import System.IO

readInt :: String -> Int
readInt = read

-- (:) :: a -> [a] -> [a]
fromStringToSingleDigits :: String -> [Int]
fromStringToSingleDigits = map (readInt . (: ""))

processColumn xMax yMax heatmap x y =
  let val = (heatmap !! x) !! y
   in if val == 9
        then Nothing -- little optimization, if the value is 9 for sure is not a lower point
        else
          let top = if x == 0 then 9 else (heatmap !! (x -1)) !! y
              right = if y == yMax then 9 else (heatmap !! x) !! (y + 1)
              bottom = if x == xMax then 9 else (heatmap !! (x + 1)) !! y
              left = if y == 0 then 9 else (heatmap !! x) !! (y -1)
           in if all (> val) [top, right, bottom, left] then Just (x, y, val) else Nothing

-- part 2

type Point = (Int, Int)

getAdjacentPoints :: Int -> Int -> Point -> [Point]
getAdjacentPoints xMax yMax point@(x, y)
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
    right = (x, y + 1)
    down = (x + 1, y)
    left = (x, y -1)
    up = (x -1, y)

isNotAnEdgePoint :: [[Int]] -> Point -> Bool
isNotAnEdgePoint [] _ = error "Empty heatmap"
isNotAnEdgePoint hmap (x, y)
  | x < 0 || y < 0 || x >= length hmap || y >= length (head hmap) = error "Invalid point"
  | otherwise = (hmap !! x) !! y /= 9

getBasin :: [[Int]] -> Int -> Int -> [Point] -> [Point] -> [Point]
getBasin _ _ _ visited [] = visited
getBasin hmap xMax yMax visited toVisit@(x : xs) =
  let adjs = filter (\p -> isNotAnEdgePoint hmap p && (p `notElem` visited)) (getAdjacentPoints xMax yMax x)
   in if x `notElem` visited
        then getBasin hmap xMax yMax (visited ++ [x]) (xs ++ adjs)
        else getBasin hmap xMax yMax visited (xs ++ adjs)

main = do
  contents <- readFile "input.txt"

  let heatmap = map fromStringToSingleDigits (lines contents)
      xMax = length heatmap - 1
      yMax = length (head heatmap) -1 -- all the rows have the same length
      xs = [0 .. xMax]
      ys = [0 .. yMax]

  let processRow x = map (processColumn xMax yMax heatmap x) ys

  let lowerPoints = filter isJust (concatMap processRow xs)

  let f (Just (x, y, val)) = val + 1
      f Nothing = error "Not a lower point!"

  let dangerLevel = sum $ map f lowerPoints

  putStrLn $ "Danger level: " ++ show dangerLevel

  -- part2
  let lpMapping (Just (x, y, val)) = (x, y)
      lpMapping Nothing = error "Not a lower point!"

  let justPoints = map lpMapping lowerPoints
      basins = map (\p -> getBasin heatmap xMax yMax [] [p]) justPoints
      areas = reverse . sort $ map length basins

  let lbf = product $ take 3 areas

  putStrLn $ "Largest basins factor: " ++ show lbf
