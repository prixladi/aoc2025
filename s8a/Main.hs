module Main (main) where

import Data.List (groupBy, sortBy)

type Node = [Float]

type Edge = (Node, Node, Float)

k :: Int
k = 1000

main :: IO ()
main = do
  cont <- getContents

  let nodes = fmap read . splitOn ',' <$> lines cont

  print $ solve nodes

solve :: [Node] -> Int
solve nodes = product (take 3 (sortBy (flip compare) (map length nodeGroups)))
  where
    edges = buildEdges nodes
    solvedNodes = sortBy (\(_, a) (_, b) -> compare a b) (solve' (take k edges) (numbered nodes))
    nodeGroups = groupBy (\(_, a) (_, b) -> a == b) solvedNodes

solve' :: [Edge] -> [(Node, Int)] -> [(Node, Int)]
solve' [] nodeGroups = nodeGroups
solve' ((from, to, _) : xs) nodeGroups = solve' xs newNodeGroups
  where
    (_, g1) = head $ filter (\x -> fst x == from) nodeGroups
    (_, g2) = head $ filter (\x -> fst x == to) nodeGroups

    newNodeGroups = if g1 == g2 then nodeGroups else mergeGroups nodeGroups g1 g2

mergeGroups :: [(Node, Int)] -> Int -> Int -> [(Node, Int)]
mergeGroups [] _ _ = []
mergeGroups (x : xs) g1 g2
  | snd x == g1 = (fst x, g2) : mergeGroups xs g1 g2
  | otherwise = x : mergeGroups xs g1 g2

buildEdges :: [Node] -> [Edge]
buildEdges nodes =
  sortBy
    (\(_, _, d1) (_, _, d2) -> compare d1 d2)
    ((\(node, pos) -> buildEdges' (drop pos nodes) node) =<< numbered nodes)

buildEdges' :: [Node] -> Node -> [Edge]
buildEdges' [] _ = []
buildEdges' (to : xs) from = (from, to, distance from to) : buildEdges' xs from

distance :: Node -> Node -> Float
distance [] [] = 0
distance (a : as) (b : bs) = (a - b) ** 2 + distance as bs
distance _ _ = error "Node dimension mismatch"

splitOn :: (Foldable t, Eq a) => a -> t a -> [[a]]
splitOn delimiter = foldr f [[]]
  where
    f c l@(x : xs)
      | c == delimiter = [] : l
      | otherwise = (c : x) : xs
    f _ [] = []

numbered :: [a] -> [(a, Int)]
numbered = f 1
  where
    f _ [] = []
    f pos (x : xs) = (x, pos) : f (pos + 1) xs