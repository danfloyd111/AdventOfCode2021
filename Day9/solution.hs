import Data.List (transpose)
import Data.Map (fromList, fromListWith, toList)
import Data.Maybe (isJust)
import System.IO

readInt :: String -> Int
readInt = read

-- (:) :: a -> [a] -> [a]
fromStringToSingleDigits :: String -> [Int]
fromStringToSingleDigits = map (readInt . (: ""))

processColumn xMax yMax heatmap x y =
  let top = if x == 0 then 9 else (heatmap !! (x -1)) !! y
      right = if y == yMax then 9 else (heatmap !! x) !! (y + 1)
      bottom = if x == xMax then 9 else (heatmap !! (x + 1)) !! y
      left = if y == 0 then 9 else (heatmap !! x) !! (y -1)
      val = (heatmap !! x) !! y
   in if all (> val) [top, right, bottom, left] then Just (x, y, val) else Nothing

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
