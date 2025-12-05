{-# LANGUAGE InstanceSigs #-}

module Main (main) where

import Data.List (isInfixOf, sort)

newtype Range = Pair (Int, Int)
  deriving (Eq, Show)

instance Ord Range where
  compare :: Range -> Range -> Ordering
  compare (Pair (a1, _)) (Pair (a2, _)) =
    compare a1 a2

main :: IO ()
main = do
  cont <- getContents
  let (ranges, values) = prepareData $ lines cont
  print $ foldr (incIfTrue . isInRanges ranges) 0 values

isInRanges :: [Range] -> Int -> Bool
isInRanges [] _ = False
isInRanges [midValue] value = isInRange midValue value
isInRanges ranges value
  | isInRange midValue value = True
  | value < start = isInRanges (take mid ranges) value
  | otherwise = isInRanges (drop mid ranges) value
  where
    mid = length ranges `div` 2
    midValue@(Pair (start, _)) = ranges !! mid

isInRange :: Range -> Int -> Bool
isInRange (Pair (start, end)) value = value >= start && value <= end

prepareData :: [String] -> ([Range], [Int])
prepareData content =
  ( simplifyRanges $ sort $ parseRanges $ filter (isInfixOf "-") content,
    parseValues $ filter (\x -> not ("-" `isInfixOf` x) && x /= "") content
  )

simplifyRanges :: [Range] -> [Range]
simplifyRanges ((Pair (s1, e1)) : (Pair (s2, e2)) : xs) | e1 >= s2 = simplifyRanges $ Pair (s1, max e1 e2) : xs
simplifyRanges (x : xs) = x : simplifyRanges xs
simplifyRanges [] = []

parseRanges :: [String] -> [Range]
parseRanges [] = []
parseRanges (x : xs) = Pair ((read . head) rangeTuple, read (rangeTuple !! 1)) : parseRanges xs
  where
    rangeTuple = splitOn '-' x

parseValues :: [String] -> [Int]
parseValues = map read

splitOn :: (Foldable t, Eq a) => a -> t a -> [[a]]
splitOn delimiter = foldr f [[]]
  where
    f c l@(x : xs)
      | c == delimiter = [] : l
      | otherwise = (c : x) : xs
    f _ [] = []

incIfTrue :: Bool -> Int -> Int
incIfTrue True acc = acc + 1
incIfTrue False acc = acc