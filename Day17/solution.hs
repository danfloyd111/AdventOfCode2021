import System.IO

readInt :: String -> Int
readInt = read

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
  "" -> []
  s' -> w : wordsWhen p s''
    where
      (w, s'') = break p s'

-- takes a string in the format "x=A..B" or "y=A..B" where A and B are integers and returns (A,B)
parseStartAndEndCoords :: String -> (Int, Int)
parseStartAndEndCoords rawSpan =
  let span = wordsWhen (== '.') (wordsWhen (== '=') rawSpan !! 1)
      start = readInt $ head span
      end = readInt $ last span
   in (start, end)

-- calculates the sum of the first n integers
gaussSum :: Int -> Int
gaussSum n = (n * (n + 1)) `div` 2

main = do
  inputLine <- readFile "input.txt"

  let inputTokens = wordsWhen (== ',') inputLine
      (xTargetMin, xTargetMax) = parseStartAndEndCoords (words (head inputTokens) !! 2)
      (yTargetMin, yTargetMax) = parseStartAndEndCoords (last inputTokens)

  let maxHeight = gaussSum (- yTargetMin -1)

  putStrLn $ "Maximum height: " ++ show maxHeight

{-
  Part 1 demonstration:

  - If the velocity decreases by 1 every step, assuming that vi is the initial velocity we reach the maximum height ym
    after vi steps and ym = gauss(vi) where "gauss" is the Gauss function to calculate the sum of the first n natural
    numbers: gauss(n) = (n * (n + 1)) / 2

    - ym is reached after vi steps because vi decreases by one after every step. So after vi steps, vi will eventually
      be 0 and ym will not grow anymore
    - ym is just the sum of all increments made at every step. After the first step the sum is just vi, after the second
      we increment it by vi-1 (the velocity decreases after every step) and so on until step vi - 1 where we increment
      by 1 and then the grow stops. If we take the increments backwards we get the progression: 0, 1, 2, 3, 4, ..., vi
      And since we want the sum of all these values here the Gauss formula help us, that's just the first vi natural
      numbers, so the sum equals to gauss(vi)

  - The more vi is bigger, the more ym will be bigger, since we want to find the max height we have to maximixe vi.
    But how big can it be? We know that the target height yt < 0 (specs condition), so it has to be true that
    (- vi - 1 )>= yt because at step 2vi + 1 the probe will be again at heigth 0 with a velocity of (- vi - 1) and
    since yt is negative, if (- vi - 1) were less than yt the probe will get past yt after the next step!

    - the probe will be at height 0 with a velocity of (- vi - 1) at step 2vi + 1 because it descibes a symmetrical
      parabola: at step 0 velocity is vi and height is 0, we already demonstrated that at step vi the velocity will be
      0 and the height will be at its maximum, then the step after (step vi + 1) the height will stay the same
      (velocity was 0) and the velocity will be - 1. From now on the velocity will decrease by 1 at every step so at
      step vi + vi the velocity will be - vi and after the next step (2vi + 1) the velocity will be - vi - 1. In
      addition, since the probe requires vi steps to reach ym from 0 and this is true also backwards, and since
      we were at ym at step vi + 1 we will be again at 0 after step 2vi + 1

  So: we have to maximixe vi without breaking - vi - 1 >= yt -> vi <= - yt - 1 so the maximum value for vi is exactly
  vi  = - yt - 1 and then ym = gauss(- yt - 1).

  We don't have a single yt but a range like -10..-20 so to maximize the height (which is the gaussian sum) we just
  have to pick the bigger one in absolute value (or the smaller one in the range if you prefer).
-}