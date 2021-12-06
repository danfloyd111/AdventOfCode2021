import System.IO

-- Specify the type to help the compiler
readInt :: String -> Int
readInt = read

-- First part
calculatePosition :: [String] -> Int -> Int -> (Int, Int)
calculatePosition [] initDepth initHPos = (initDepth, initHPos)
calculatePosition (cmd : cmds) initDepth initHPos =
  let [direction, amountStr] = words cmd
      amount = readInt amountStr
   in case direction of
        "up" -> calculatePosition cmds (initDepth - amount) initHPos
        "down" -> calculatePosition cmds (initDepth + amount) initHPos
        "forward" -> calculatePosition cmds initDepth (initHPos + amount)
        _ -> error $ "Direction not supported: " ++ show direction

-- Second part
calculateRefinedPosition :: [String] -> Int -> Int -> Int -> (Int, Int)
calculateRefinedPosition [] initDepth initHPos initAim = (initDepth, initHPos)
calculateRefinedPosition (cmd : cmds) initDepth initHPos initAim =
  let [direction, amountStr] = words cmd
      updateAim = calculateRefinedPosition cmds initDepth initHPos
      amount = readInt amountStr
   in case direction of
        "up" -> updateAim (initAim - amount)
        "down" -> updateAim (initAim + amount)
        "forward" -> calculateRefinedPosition cmds (initDepth + (initAim * amount)) (initHPos + amount) initAim
        _ -> error $ "Direction not supported: " ++ show direction

main = do
  contents <- readFile "input.txt"
  let commands = lines contents
  let (depth, hpos) = calculatePosition commands 0 0
  let (refinedDepth, refinedHPos) = calculateRefinedPosition commands 0 0 0

  putStrLn $
    "Depth: "
      ++ show depth
      ++ " - Horizontal position: "
      ++ show hpos
      ++ " - Combined value: "
      ++ show (depth * hpos)

  putStrLn $
    "Refined depth: "
      ++ show refinedDepth
      ++ " - Refined horizontal position: "
      ++ show refinedHPos
      ++ " - Refined combined value: "
      ++ show (refinedDepth * refinedHPos)