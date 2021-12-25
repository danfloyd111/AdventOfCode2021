import Data.Char (digitToInt)
import qualified Data.Set as Set
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

updateRiskLevel ::
  (Num a, Ord a) =>
  [[a]] -> -- chitons
  (Int, Int) -> -- point
  [(Int, Int)] -> -- neighbors of the point
  [[a]] -> -- risk matrix
  Set.Set (a, (Int, Int)) -> -- set of (risk of the node, node) to be visited
  ([[a]], Set.Set (a, (Int, Int))) -- updated risk matrix and set of nodes to be visited
updateRiskLevel _ _ [] riskMatrix toBeVisited = (riskMatrix, toBeVisited)
updateRiskLevel chitons (r, c) ((x, y) : ns) riskMatrix toBeVisited =
  let alt = (riskMatrix !! r !! c) + (chitons !! x !! y)
      currentRisk = riskMatrix !! x !! y
      (riskMatrix', toBeVisited') =
        if alt < currentRisk
          then (replaceValueInSquareMatrix riskMatrix (x, y) alt, Set.insert (alt, (x, y)) toBeVisited)
          else (riskMatrix, toBeVisited)
   in updateRiskLevel chitons (r, c) ns riskMatrix' toBeVisited'

-- chitons matrix, risk matrix, set of (risk, point) to be visited, end point -> danger of less risky path
searchLessRiskyPath :: [[Int]] -> [[Int]] -> Set.Set (Int, Point) -> Point -> Int
searchLessRiskyPath chitons riskMatrix toBeVisited endPoint =
  if targetPoint == endPoint
    then targetRisk
    else searchLessRiskyPath chitons riskMatrix' toBeVisited'' endPoint
  where
    (targetRisk, targetPoint) = Set.findMin toBeVisited
    toBeVisited' = Set.deleteMin toBeVisited
    neighbors = getNeighbors chitons targetPoint
    (riskMatrix', toBeVisited'') = updateRiskLevel chitons targetPoint neighbors riskMatrix toBeVisited'

-- Second part functions
increaseRisk :: Int -> Int
increaseRisk n = if n == 9 then 1 else n + 1

increaseRow :: [Int] -> [Int]
increaseRow = map increaseRisk

deployRow :: Int -> [Int] -> [Int] -> [Int]
deployRow 1 _ acc = acc
deployRow n row acc =
  let row' = increaseRow row
   in deployRow (n -1) row' (acc ++ row')

buildFullRow :: [Int] -> [Int]
buildFullRow row = deployRow 5 row row

buildSubMatricesList 1 _ acc = acc
buildSubMatricesList n m acc =
  let m' = map increaseRow m
   in buildSubMatricesList (n -1) m' (acc ++ m')

buildFullMatrix chitons =
  let partial = buildSubMatricesList 5 chitons chitons
   in map buildFullRow partial

-- Using Djikstra algorithm to find the less dangerous path across the matrix
-- A risk :: [[Int]] will store risk level for node (x,y) at position row: x , column: y of risk -> risk !! x !! y
-- risk :: [[Int]] will optimize increase and decrease operations on risk values
-- A Set of (risk, Point) will be used as a priority queue to get the less dangerous node to visit, a set is ordered
-- so finding the minimum is trivial and speedy

main = do
  content <- readFile "input.txt"
  let chitons = map toIntList (lines content)
  -- specs document says that input is a n*n grid so it's safe to read just a "dimension": the number of rows
  let n = length chitons
  let riskMatrix = replaceValueInSquareMatrix (buildSquareMatrix (maxBound :: Int) n) (0, 0) 0

  let minimumDanger = searchLessRiskyPath chitons riskMatrix (Set.fromList [(0, (0, 0))]) (n -1, n -1)
  putStrLn $ "First part: less dangerous path has a risk level of " ++ show minimumDanger

  -- second part
  let fullChitons = buildFullMatrix chitons
  let w = length fullChitons
  let fullRiskMatrix = replaceValueInSquareMatrix (buildSquareMatrix (maxBound :: Int) w) (0, 0) 0
  let fullMinimumDanger = searchLessRiskyPath fullChitons fullRiskMatrix (Set.fromList [(0, (0, 0))]) (w -1, w -1)

  putStrLn $ "Second part: less dangerous path has a risk level of " ++ show fullMinimumDanger