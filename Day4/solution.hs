import Data.List (transpose)
import System.IO

type Board = [[Int]]

type Row = [Int]

readInt :: String -> Int
readInt = read

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
  "" -> []
  s' -> w : wordsWhen p s''
    where
      (w, s'') = break p s'

splitWhen :: (String -> Bool) -> [String] -> [[String]]
splitWhen p ls = case dropWhile p ls of
  [] -> []
  s' -> w : splitWhen p s''
    where
      (w, s'') = break p s'

toBingoBoard :: [String] -> Board
toBingoBoard [] = []
toBingoBoard (x : xs) = ls : toBingoBoard xs where ls = map readInt (words x)

isAWinningRow :: Row -> Bool
isAWinningRow = all (\x -> x == -1)

isAWinningMatrix :: [Row] -> Bool
isAWinningMatrix = any isAWinningRow

isAWinningBoard :: Board -> Bool
isAWinningBoard board =
  let tsBoard = transpose board
   in isAWinningMatrix board || isAWinningMatrix tsBoard

findWinningBoards :: [Board] -> [Board]
findWinningBoards = filter isAWinningBoard

-- returns [] if there's no winning boards
findWinningBoard :: [Board] -> Board
findWinningBoard boards =
  let winners = findWinningBoards boards
   in if null winners then [] else head winners

getRowScore :: Row -> Int
getRowScore row = sum $ filter (\x -> x /= -1) row

getBoardScore :: Int -> Board -> Int
getBoardScore n board = n * sum (map getRowScore board)

updateBoards :: Int -> [Board] -> [Board]
updateBoards num = map updateBoard
  where
    updateBoard board = map updateRow board
      where
        updateRow row = map (\x -> if x == num then -1 else x) row

getWinningScore :: [Int] -> [Board] -> Int
getWinningScore [] _ = error "getWinningScore - All numbers extracted without any winner"
getWinningScore (x : xs) boards =
  let boardsUpdated = updateBoards x boards
      winningBoard = findWinningBoard boardsUpdated
   in case winningBoard of
        [] -> getWinningScore xs boardsUpdated
        b' -> getBoardScore x b'

-- second part

getBoardsScores :: Int -> [Board] -> [Int]
getBoardsScores n [] = []
getBoardsScores n bs = map (getBoardScore n) bs

removeWinningBoards :: [Board] -> [Board] -> [Board]
removeWinningBoards boards winners = filter (`notElem` winners) boards

getWinnersScores :: [Int] -> [Board] -> [Int] -> [Int]
getWinnersScores [] _ acc = acc
getWinnersScores (x : xs) boards acc =
  let boardsUpdated = updateBoards x boards
      winningBoards = findWinningBoards boardsUpdated
      boardsWithoutWinners = removeWinningBoards boardsUpdated winningBoards
      newAcc = if null winningBoards then acc else acc ++ getBoardsScores x winningBoards
   in getWinnersScores xs boardsWithoutWinners newAcc

main = do
  contents <- readFile "input.txt"
  let contentLines = lines contents
  let extractedNums = map readInt (wordsWhen (== ',') (head contentLines))
  let rawBoards = drop 1 contentLines -- boards contains now all the lines, boards are separated by empty strings
  let strBoards = splitWhen (== "") rawBoards -- now strBoards contains all the boards represented as lists of rows
  let boards = map toBingoBoard strBoards

  let winningScore = getWinningScore extractedNums boards

  putStrLn $ "Winning score: " ++ show winningScore

  let winnersScores = getWinnersScores extractedNums boards []

  let winLen = length winnersScores
  let boardsLen = length boards

  putStrLn $ "Last winning board score " ++ show (last winnersScores)
