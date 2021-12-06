import System.IO

{-
 initial state: 3,4,3,1,2

 states     0  1  2  3  4  5  6  7  8

 cunters    0  1  1  2  1  0  0  0  0 - t0: 3,4,3,1,2
            1  1  2  1  0  0  0  0  0 - t1: 2,3,2,0,1
            1  2  1  0  0  0  1  0  1 - t2: 1,2,1,6,0,8
            2  1  0  0  0  1  1  1  1 - t3: 0,1,0,5,6,7,8
            1  0  0  0  1  1  3  1  2 - t4: 6,0,6,4,5,6,7,8,8
            0  0  0  1  1  3  2  2  1 - T5: 5,6,5,3,4,5,6,7,7,8
            ... and so on!

We only have 9 possible states, for every iteration of the next state calculation we only have to modify the number
of fishes for every state. Every state in position p at time tn+1 inherits the value of the state in position p+1 at
time tn, with the only exception of state[6] and state[8].
- state[6] at time tn+1 inherits the state in position 7 in a similar way of the others but we have to sum to it also
  the state[0] at time tn! (every in state 0 respawn, so we get +1 in state 6 for every fish in state 0)
- state[8] at time tn+1 inherits the state in position 0 at time tn (every fish in state 0 spawn a new fish-child with
  state 8)

That's it! Exploiting an exponential solution in a linear time (just because we care of the number of the fishes and
not the final state obviously)

-}

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

getNextState :: [Integer] -> [Integer]
getNextState [a, b, c, d, e, f, g, h, i] =
  [b, c, d, e, f, g, h', i, a]
  where
    h' = h + a
getNextState _ = error "getNextState - input error: this function only accept an 9 elements [Integer]"

getStateAfterNCycles :: Int -> [Integer] -> [Integer]
getStateAfterNCycles 0 state = state
getStateAfterNCycles n state = getStateAfterNCycles (n -1) (getNextState state)

getInitialState :: [Int] -> [Integer]
getInitialState fishes =
  let state = [0, 0, 0, 0, 0, 0, 0, 0, 0]
      getInitStateWithAcc acc [] = acc
      getInitStateWithAcc acc (f : fs) =
        let newAcc = replace acc f (acc !! f + 1)
         in getInitStateWithAcc newAcc fs
   in getInitStateWithAcc state fishes

main = do
  contents <- readFile "input.txt"
  let fishes = map readInt (wordsWhen (== ',') contents)

  let initialState = getInitialState fishes

  let firstPartCycleCount = 80
  let firstPartTerminalState = getStateAfterNCycles firstPartCycleCount initialState

  let secondPartCycleCount = 256
  let secondPartTerminalState = getStateAfterNCycles secondPartCycleCount initialState

  putStrLn $ "Fish count after " ++ show firstPartCycleCount ++ " cycles: " ++ show (sum firstPartTerminalState)
  putStrLn $ "Fish count after " ++ show secondPartCycleCount ++ " cycles: " ++ show (sum secondPartTerminalState)