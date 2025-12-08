module Main (main) where

import Data.List (sortBy)

type Node = [Float]

type Edge = (Node, Node, Float)

main :: IO ()
main = do
  cont <- getContents

  let nodes = fmap read . splitOn ',' <$> lines cont

  print (round (run nodes) :: Int)

run :: [Node] -> Float
run nodes = kruksal' (buildEdges nodes) (numbered nodes) 0

kruksal' :: [Edge] -> [(Node, Int)] -> Int -> Float
kruksal' [] _ _ = error "Unable to create connected graph"
kruksal' ((from, to, _) : xs) nodeGroups cnt
  | (newCnt + 1) >= length nodeGroups = head from * head to
  | otherwise = kruksal' xs newNodeGroups newCnt
  where
    (_, g1) = head $ filter (\x -> fst x == from) nodeGroups
    (_, g2) = head $ filter (\x -> fst x == to) nodeGroups

    newCnt = cnt + (if g1 == g2 then 0 else 1)
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