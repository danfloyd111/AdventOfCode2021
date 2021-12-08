import Data.List
import System.IO

readInt :: String -> Int
readInt = read

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
  "" -> []
  s' -> w : wordsWhen p s''
    where
      (w, s'') = break p s'

pack :: (Eq a) => [a] -> [[a]]
pack = foldr func []
  where
    func x [] = [[x]]
    func x (y : xs) = if x == head y then (x : y) : xs else [x] : y : xs

findMaxCrabFrequency x [] = x
findMaxCrabFrequency (pos, freq) ((p, f) : ls) =
  if freq < f then findMaxCrabFrequency (p, f) ls else findMaxCrabFrequency (pos, freq) ls

gaussSum n = (n * (n + 1)) `div` 2

computeConstantFuelConsumption pos acc [] = acc
computeConstantFuelConsumption pos acc (l : ls) =
  let c = abs (l - pos) in computeConstantFuelConsumption pos (acc + c) ls

computeGaussianFuelConsumption pos acc [] = acc
computeGaussianFuelConsumption pos acc (l : ls) =
  let c = gaussSum $ abs (l - pos) in computeGaussianFuelConsumption pos (acc + c) ls

computeBruteForceList acc 0 ls = acc
computeBruteForceList acc pos ls =
  computeBruteForceList (acc ++ [computeConstantFuelConsumption pos 0 ls]) (pos -1) ls

computeBruteForceListG acc 0 ls = acc
computeBruteForceListG acc pos ls =
  computeBruteForceListG (acc ++ [computeGaussianFuelConsumption pos 0 ls]) (pos -1) ls

-- TODO: refactor all of this mess!!!

main = do
  contents <- readFile "input.txt"
  let crabs = sort $ map readInt (wordsWhen (== ',') contents)
  let packedCrabs = pack crabs
  let tuples = map (\c -> (head c, length c)) packedCrabs
  let (head : tail) = tuples
  let (pos, freq) = findMaxCrabFrequency head tail
  let fuelConsumption = computeConstantFuelConsumption pos 0 crabs

  -- brute force
  let edge = maximum crabs
  let mini = minimum (computeBruteForceList [] edge crabs)
  let miniGauss = minimum (computeBruteForceListG [] edge crabs)

  putStrLn $ "Minimum fuel consumption with constant consumption " ++ show mini
  putStrLn $ "Minimum fuel consumption with gaussian consumption " ++ show miniGauss
