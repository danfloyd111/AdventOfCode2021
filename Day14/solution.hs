{-# LANGUAGE TupleSections #-}

import Data.List (sort)
import Data.Map (elems, empty, fromList, insert, insertWith, lookup, toList)
import System.IO

polymerToPairs :: String -> [String] -> [String]
polymerToPairs (a : b : "") acc = [a, b] : acc
polymerToPairs (a : b : str) acc = polymerToPairs (b : str) ([a, b] : acc)
polymerToPairs _ _ = error "polymerToPairs - the polymer needs to have at least lenght 2"

parseReaction :: String -> (String, String) -- starting pair, new element
parseReaction str =
  let [pair, _, element] = words str
   in (pair, element)

react :: [(String, String)] -> String -> (Char, [String])
react [] pair = error "react - something wrong with your pair!"
react ((p, elem) : rs) pair = if pair == p then (head elem, [head p : elem, elem ++ [last p]]) else react rs pair

replicatePairs count [] acc = acc
replicatePairs count (p : ps) acc =
  let newPairs = replicate count p
   in replicatePairs count ps (newPairs ++ acc)

reactList :: [(String, String)] -> [(String, Int)] -> [(Char, Int)] -> [(String, Int)] -> ([(Char, Int)], [(String, Int)])
reactList rs [] elements resultingPairs = (elements, resultingPairs)
reactList rs ((p, count) : ps) elements resultingPairs =
  let (element, pairs) = react rs p
      elemTuple = (element, count)
      pairTuples = map (,count) pairs
   in reactList rs ps (elemTuple : elements) (pairTuples ++ resultingPairs)

updateElementsMap [] m = m
updateElementsMap ((element, count) : elems) m =
  let m' = insertWith (+) element count m
   in updateElementsMap elems m'

updatePairsMap [] m = m
updatePairsMap ((pair, count) : pairs) m =
  let m' = insertWith (+) pair count m
   in updatePairsMap pairs m'

fromJust :: Maybe a -> a
fromJust Nothing = error "fromJust - got Nothing"
fromJust (Just x) = x

buildElementsMap bs m = foldl (\m b -> insertWith (+) b 1 m) m bs

buildPairsMap ps = fromList (map (,1) ps)

polymerizationStep reactionTuples elementsMap pairsMap =
  let activePairs = Prelude.filter (\(p, c) -> c > 0) (toList pairsMap)
      (elements, pairs) = reactList reactionTuples activePairs [] [] --TODO: attention here!
      newElementsMap = updateElementsMap elements elementsMap
      newPairsMap = updatePairsMap pairs empty
   in (newElementsMap, newPairsMap)

polymerize reactionTuples elementsMap pairsMap 0 = (elementsMap, pairsMap)
polymerize reactionTuples elementsMap pairsMap n =
  let (em', pm') = polymerizationStep reactionTuples elementsMap pairsMap
   in polymerize reactionTuples em' pm' (n -1)

main = do
  contents <- readFile "input.txt"
  let base : _ : reactions = lines contents
  let basePairs = polymerToPairs base []
  let reactionTuples = map parseReaction reactions
  let baseElementsMap = buildElementsMap base empty
  let basePairsMap = buildPairsMap basePairs

  let (elemMapAfter10Steps, _) = polymerize reactionTuples baseElementsMap basePairsMap 10
  let countersAfter10Steps = sort $ elems elemMapAfter10Steps
  let part1Result = last countersAfter10Steps - head countersAfter10Steps

  putStrLn $ "Largest difference (after 10 steps): " ++ show part1Result

  let (elemMapAfter40Steps, _) = polymerize reactionTuples baseElementsMap basePairsMap 40
  let countersAfter40Steps = sort $ elems elemMapAfter40Steps
  let part2Result = last countersAfter40Steps - head countersAfter40Steps

  putStrLn $ "Largest difference (after 40 steps): " ++ show part2Result