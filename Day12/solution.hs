import Data.List (intercalate)
import Data.Map
import GHC.Unicode (isLower)
import System.IO

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
  "" -> []
  s' -> w : wordsWhen p s''
    where
      (w, s'') = break p s'

swapTuplesList [] acc = acc
swapTuplesList ((a, b) : xs) acc = swapTuplesList xs ((b, a) : acc)

fromJust :: Maybe a -> a
fromJust Nothing = error "fromJust - got Nothing"
fromJust (Just x) = x

getLastNodeFromPath p = last $ wordsWhen (== '-') p

getFirstNodeFromPath p = head $ wordsWhen (== '-') p

removeStartNodeFromPath p =
  let (_ : xs) = wordsWhen (== '-') p
   in intercalate "-" xs

removeEndNodeFromPath p =
  let xs = init $ wordsWhen (== '-') p
   in intercalate "-" xs

buildCache = Prelude.map removeStartNodeFromPath

isTotalPath p =
  let fst = getFirstNodeFromPath p
      lst = getLastNodeFromPath p
   in fst == "start" && lst == "end"

splitPathsInPartialsAndTotals [] parts tots = (parts, tots)
splitPathsInPartialsAndTotals (p : ps) parts tots =
  if isTotalPath p
    then splitPathsInPartialsAndTotals ps parts (p : tots)
    else splitPathsInPartialsAndTotals ps (p : parts) tots

isSmallNode :: String -> Bool
isSmallNode n = all isLower n && n /= "end" && n /= "start" -- we're just interestd in intermediate nodes

isNodeOfPath :: String -> String -> Bool
isNodeOfPath n p =
  let ps = wordsWhen (== '-') p
   in n `elem` ps

isValidNodeForPath n p = not (isSmallNode n && isNodeOfPath n p)

-- TODO: not using cache for now... use it and optimize!!!!
findTotalPaths nodesMap _ [] totals = totals
findTotalPaths nodesMap cache partials totals =
  let buildNextPartialsList [] acc = acc
      buildNextPartialsList (p : ps) acc =
        let lastNode = getLastNodeFromPath p
            leafs = fromJust (Data.Map.lookup lastNode nodesMap)
            updatedPaths = Prelude.map (\l -> p ++ "-" ++ l) leafs
            validPaths = Prelude.filter (\p -> isValidNodeForPath (getLastNodeFromPath p) (removeEndNodeFromPath p)) updatedPaths
         in buildNextPartialsList ps (validPaths ++ acc)
      paths = buildNextPartialsList partials []
      (nextPartials, nextTotals) = splitPathsInPartialsAndTotals paths [] []
      newCache = buildCache nextTotals
   in findTotalPaths nodesMap (cache ++ newCache) nextPartials (totals ++ nextTotals)

main = do
  content <- readFile "input.txt"
  let lns = lines content
  let splitLine = wordsWhen (== '-')
  let fromToLists = Prelude.map splitLine lns
  let fromToTuples = Prelude.map (\xs -> (head xs, last xs)) fromToLists
  let toFromTuples = swapTuplesList fromToTuples []
  let branches = Prelude.filter (\(from, to) -> from /= "end" && to /= "start") (fromToTuples ++ toFromTuples)
  -- we don't want to "escape" from end node and go again on the start node
  let nodesMap = Data.Map.map (wordsWhen (== ',')) (fromListWith (\a b -> a ++ "," ++ b) branches)

  let firstLeafs = fromJust (Data.Map.lookup "start" nodesMap)
  let firstBranches = Prelude.map ("start-" ++) firstLeafs

  let test = findTotalPaths nodesMap [] firstBranches []
  let test2 = isValidNodeForPath (getLastNodeFromPath "start-b-d") (removeEndNodeFromPath "start-b-d")

  -- TODO: ok sembra funzionare, costruisci la cache e prova

  putStrLn $ "Result: " ++ show (length test)