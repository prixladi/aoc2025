{-# LANGUAGE InstanceSigs #-}

module Main (main) where

import Data.List (sort)

newtype Range = Pair (Int, Int)
  deriving (Eq, Show)

instance Ord Range where
  compare :: Range -> Range -> Ordering
  compare (Pair (a1, _)) (Pair (a2, _)) =
    compare a1 a2

main :: IO ()
main = do
  cont <- getContents
  let ranges = simplifyRanges $ sort $ parseRanges (lines cont)
  print $ foldr ((+) . (\(Pair (start, end)) -> end - start + 1)) 0 ranges

simplifyRanges :: [Range] -> [Range]
simplifyRanges ((Pair (s1, e1)) : (Pair (s2, e2)) : xs) | e1 >= s2 = simplifyRanges $ Pair (s1, max e1 e2) : xs
simplifyRanges (x : xs) = x : simplifyRanges xs
simplifyRanges [] = []

parseRanges :: [String] -> [Range]
parseRanges [] = []
parseRanges (x : xs) = Pair ((read . head) rangeTuple, read (rangeTuple !! 1)) : parseRanges xs
  where
    rangeTuple = splitOn '-' x

splitOn :: (Foldable t, Eq a) => a -> t a -> [[a]]
splitOn delimiter = foldr f [[]]
  where
    f c l@(x : xs)
      | c == delimiter = [] : l
      | otherwise = (c : x) : xs
    f _ [] = []