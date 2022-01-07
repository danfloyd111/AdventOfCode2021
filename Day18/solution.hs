import System.IO

data Tree = Pair Tree Tree | Leaf Int deriving (Eq)

instance Show Tree where
  show (Leaf leaf) = show leaf
  show (Pair left right) = "[" ++ show left ++ "," ++ show right ++ "]"

main = do
  putStrLn "Hello snailfish!"