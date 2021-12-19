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

reactList :: [(String, String)] -> [(String, Int)] -> String -> [String] -> (String, [String])
reactList rs [] elements resultingPairs = (elements, resultingPairs)
reactList rs ((p, count) : ps) elements resultingPairs =
  let (element, pairs) = react rs p
      newElements = replicate count element --TODO: NO!!!!  just element and count for part2
      newPairs = replicatePairs count pairs [] --TODO: NO!!! just pair and count for part2
   in reactList rs ps (newElements ++ elements) (newPairs ++ resultingPairs)

increaseElementsMap = foldl (\m e -> insertWith (+) e 1 m)

fromJust :: Maybe a -> a
fromJust Nothing = error "fromJust - got Nothing"
fromJust (Just x) = x

buildElementsMap bs m = foldl (\m b -> insertWith (+) b 1 m) m bs

buildPairsMap ps = fromList (map (,1) ps)

buildPairsMapFromPairsList = foldl (\m p -> insertWith (+) p 1 m)

polymerizationStep reactionTuples elementsMap pairsMap =
  let activePairs = Prelude.filter (\(p, c) -> c > 0) (toList pairsMap)
      (elements, pairs) = reactList reactionTuples activePairs [] []
      newElementsMap = increaseElementsMap elementsMap elements
      newPairsMap = buildPairsMapFromPairsList empty pairs
   in (newElementsMap, newPairsMap)

polymerize reactionTuples elementsMap pairsMap 0 = (elementsMap, pairsMap)
polymerize reactionTuples elementsMap pairsMap n =
  let (em', pm') = polymerizationStep reactionTuples elementsMap pairsMap
   in polymerize reactionTuples em' pm' (n -1)

main = do
  contents <- readFile "input-sample.txt"
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