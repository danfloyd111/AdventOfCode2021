import Debug.Trace
import System.IO

type Octopus = Int

type Grid = [[Octopus]]

type Point = (Int, Int)

replace :: [a] -> Int -> a -> [a]
replace xs i e = case splitAt i xs of
  (before, _ : after) -> before ++ e : after
  _ -> xs

-- increase energy of the octopus at given point on the given grid
-- returns the updated grid
increaseOctopusEnergy :: Grid -> Point -> Grid
increaseOctopusEnergy grid (x, y) =
  let currentEnergy = grid !! x !! y
      currentRow = grid !! x
      updatedRow = replace currentRow y (currentEnergy + 1)
   in replace grid x updatedRow

increaseOctopusesEnergy :: Grid -> [Point] -> Grid
increaseOctopusesEnergy = foldl increaseOctopusEnergy

readOctopus :: String -> Octopus
readOctopus = read

resetFlashedOctopuses :: Grid -> Grid
resetFlashedOctopuses =
  let resetOctopus oct = if oct > 9 then 0 else oct
      resetRow = map resetOctopus
   in map resetRow

fromStringToOctopuses :: String -> [Octopus]
fromStringToOctopuses = map (readOctopus . (: ""))

parseGrid :: String -> Grid
parseGrid str = map fromStringToOctopuses (lines str)

stringifyGrid :: Grid -> String
stringifyGrid g =
  let stringifyRow row = concatMap (\n -> show n ++ "\t") row ++ "\n"
   in concatMap stringifyRow g

haveFlashed :: Grid -> Point -> Bool
haveFlashed g (x, y) = ((g !! x) !! y) > 9

haveGridFlashed :: Grid -> Bool
haveGridFlashed grid =
  let haveRowFlashed = all (== 0)
   in all haveRowFlashed grid

getNeighbors :: Grid -> Point -> [Point]
getNeighbors g point@(x, y)
  | point == (0, 0) = [right, down, downright]
  | point == (0, yMax) = [left, down, downleft]
  | point == (xMax, 0) = [up, right, upright]
  | point == (xMax, yMax) = [up, left, upleft]
  | x == 0 = [left, down, right, downleft, downright]
  | x == xMax = [left, up, right, upleft, upright]
  | y == 0 = [up, down, right, upright, downright]
  | y == yMax = [up, down, left, upleft, downleft]
  | otherwise = [up, right, down, left, upright, upleft, downright, downleft]
  where
    xMax = length g - 1
    yMax = length (head g) -1
    right = (x, y + 1)
    down = (x + 1, y)
    left = (x, y -1)
    up = (x -1, y)
    upleft = (x -1, y -1)
    upright = (x -1, y + 1)
    downright = (x + 1, y + 1)
    downleft = (x + 1, y -1)

-- returns the list of points corresponding to the position of the
-- octopuses that have flashed and the updated grid
increaseGridEnergy :: Grid -> (Grid, [Point])
increaseGridEnergy grid =
  let increaseOctopus o = 1 + o
      increaseRow = map increaseOctopus
      g' = map increaseRow grid
      xMax = length grid - 1
      yMax = length (head grid) - 1
      fun x y pts =
        let pts' = if haveFlashed g' (x, y) then (x, y) : pts else pts
         in if x == xMax && y == yMax
              then pts'
              else
                if y == yMax
                  then fun (x + 1) 0 pts'
                  else fun x (y + 1) pts'
   in (g', fun 0 0 [])

-- take a grid and the list of coordinates of the flashing octopuses and
-- returns the updated grid after the flash propagation togheter with the flash count
propagateFlash :: Grid -> [Point] -> [Point] -> Int -> (Grid, Int)
propagateFlash grid [] _ sum = (grid, sum)
propagateFlash grid toCheck@(x : xs) flashed sum =
  let neighbors = getNeighbors grid x
      energizedGrid = increaseOctopusesEnergy grid neighbors
      newlyFlashed = filter (\p -> haveFlashed energizedGrid p && (p `notElem` flashed)) neighbors
   in propagateFlash energizedGrid (xs ++ newlyFlashed) (flashed ++ newlyFlashed) (sum + length newlyFlashed)

simulationStep :: Grid -> (Grid, Int)
simulationStep g =
  let (energizedGrid, flashed) = increaseGridEnergy g -- increase the energy of every octopus
      flashCount = length flashed -- count how many of them have flashed
      (propGrid, propFlashCount) = propagateFlash energizedGrid flashed flashed 0 -- propagate the flash and count again
      nextGrid = resetFlashedOctopuses propGrid -- reset all the flashed octopuses
   in (nextGrid, flashCount + propFlashCount)

simulateFlashingOctopuses :: Grid -> Int -> Int -> Int
simulateFlashingOctopuses _ 0 sum = sum
simulateFlashingOctopuses grid steps sum =
  let (g', partial) = simulationStep grid
   in simulateFlashingOctopuses g' (steps -1) (sum + partial)

findSyncStep :: Grid -> Int
findSyncStep grid =
  let f g it =
        let (g', _) = simulationStep g
         in if haveGridFlashed g'
              then it
              else f g' (it + 1)
   in f grid 1

main = do
  str <- readFile "input.txt"
  let grid = parseGrid str

  let totalFlashes = simulateFlashingOctopuses grid 100 0

  putStrLn $ "Total flashes after 100 steps: " ++ show totalFlashes

  let syncStep = findSyncStep grid

  putStrLn $ "The grid syncronizes on step n. " ++ show syncStep