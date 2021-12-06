import Data.Char (digitToInt)
import Data.List (foldl', transpose)
import System.IO

readInt :: String -> Int
readInt = read

-- e.g. "01011" to [0,1,0,1,1]
toBitList :: String -> [Int]
toBitList = map digitToInt

getGammaBit :: Int -> Int -> Char
getGammaBit treshold count
  | count > treshold = '1'
  | count < treshold = '0'
  | otherwise = error "getGammaBit - invalid input"

invertBit :: Char -> Char
invertBit b
  | b == '0' = '1'
  | b == '1' = '0'
  | otherwise = error "getEpsilonRateBitString - invalid input"

invertBitString :: String -> String
invertBitString = map invertBit

-- e.g. from 5 [10,9,3,2,6] we obtain "11001"
getGammaRateBitString :: Int -> [Int] -> String
getGammaRateBitString treshold = map (getGammaBit treshold)

-- converts a binary number represented with a string to a decimal number
toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

toBitString :: [Int] -> String -> String
toBitString [] str = str
toBitString (x : xs) str = if x == 0 then toBitString xs (str ++ "0") else toBitString xs (str ++ "1")

--
-- part 2
--

calcAcc :: Int -> Int -> Int -> [Int] -> Int
calcAcc dgt pos acc ls = if ls !! pos == dgt then acc + 1 else acc

countDigitAtPos :: Int -> Int -> Int -> [[Int]] -> Int
countDigitAtPos dgt pos acc [l] = calcAcc dgt pos acc l
countDigitAtPos dgt pos acc (l : ls) =
  let newAcc = calcAcc dgt pos acc l
   in countDigitAtPos dgt pos newAcc ls
countDigitAtPos _ _ _ _ = error "countDigitAtPos - invalid input"

countZeroesAtPos = countDigitAtPos 0

countOnesAtPos = countDigitAtPos 1

filterMatrix :: Int -> Int -> [[Int]] -> [[Int]]
filterMatrix val pos = filter (\row -> row !! pos == val)

itStepOxy :: Int -> [[Int]] -> [[Int]]
itStepOxy pos matrix =
  let zeroCount = countZeroesAtPos pos 0 matrix
      oneCount = countOnesAtPos pos 0 matrix
   in if zeroCount > oneCount
        then filterMatrix 0 pos matrix
        else filterMatrix 1 pos matrix

itStepCO2 :: Int -> [[Int]] -> [[Int]]
itStepCO2 pos matrix =
  let zeroCount = countZeroesAtPos pos 0 matrix
      oneCount = countOnesAtPos pos 0 matrix
   in if oneCount < zeroCount
        then filterMatrix 1 pos matrix
        else filterMatrix 0 pos matrix

reduceBitMatrixOxy :: Int -> Int -> [[Int]] -> [[Int]]
reduceBitMatrixOxy _ _ [row] = [row]
reduceBitMatrixOxy 0 l matrix = matrix
reduceBitMatrixOxy n l matrix =
  let reducedMatrix = itStepOxy (l - n) matrix
   in reduceBitMatrixOxy (n -1) l reducedMatrix

reduceBitMatrixCO2 :: Int -> Int -> [[Int]] -> [[Int]]
reduceBitMatrixCO2 _ _ [row] = [row]
reduceBitMatrixCO2 0 l matrix = matrix
reduceBitMatrixCO2 n l matrix =
  let reducedMatrix = itStepCO2 (l - n) matrix
   in reduceBitMatrixCO2 (n -1) l reducedMatrix

main = do
  contents <- readFile "input.txt"
  let ws = words contents -- e.g. ["010","111","011",...]
  let bitMatrix = map toBitList ws -- e.g. [[0,1,0],[1,1,1],[0,1,1],...]
  let bitMatrixTransposed = transpose bitMatrix
  let counters = map sum bitMatrixTransposed
  let gammaRateStr = getGammaRateBitString (length ws `div` 2) counters
  let epsilonRateBitStr = invertBitString gammaRateStr

  let gammaRate = toDec gammaRateStr
  let epsilonRate = toDec epsilonRateBitStr

  putStrLn $ "Gamma rate: " ++ show gammaRate
  putStrLn $ "Epsilon rate: " ++ show epsilonRate
  putStrLn $ "Power consumption: " ++ show (gammaRate * epsilonRate)

  -- part 2

  let oxygenMatrix = reduceBitMatrixOxy 12 12 bitMatrix
  let cO2Matrix = reduceBitMatrixCO2 12 12 bitMatrix
  let oxyString = toBitString (head oxygenMatrix) ""
  let cO2String = toBitString (head cO2Matrix) ""
  let oxyRating = toDec oxyString
  let cO2Rating = toDec cO2String

  putStrLn $ "Oxygen rating " ++ show oxyRating
  putStrLn $ "CO2 rating " ++ show cO2Rating
  putStrLn $ "Life support rating " ++ show (oxyRating * cO2Rating)
