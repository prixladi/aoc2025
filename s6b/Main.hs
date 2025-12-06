module Main (main) where

import Data.Char (isSpace)
import Data.List (transpose)

main :: IO ()
main = do
  cont <- getContents

  let allLines = lines cont

  let signs = filter (not . all isSpace) . splitOn ' ' $ (allLines !! (length allLines - 1))
  let matrix = transpose $ take (length allLines - 1) allLines
  let numbers = getNumbersFromMatrix matrix [[]]

  print $ solve signs numbers

solve :: [String] -> [[Int]] -> Int
solve (o : os) (n : ns) =
  foldr
    (getOperationFunction o)
    (getOperationIdentity o)
    n
    + solve os ns
solve [] [] = 0
solve _ _ = error "Mismatch in length of numbers and operators"

getNumbersFromMatrix :: [String] -> [[Int]] -> [[Int]]
getNumbersFromMatrix [] acc = reverse acc
getNumbersFromMatrix _ [] = error "Invalid matrix layout"
getNumbersFromMatrix (x : xs) acc | all isSpace x = getNumbersFromMatrix xs ([] : acc)
getNumbersFromMatrix (x : xs) (acc : rest) = getNumbersFromMatrix xs ((read (filter (not . isSpace) x) : acc) : rest)

getOperationFunction :: String -> (Int -> Int -> Int)
getOperationFunction "+" = (+)
getOperationFunction "*" = (*)
getOperationFunction op = error $ "invalid operation: " ++ op

getOperationIdentity :: String -> Int
getOperationIdentity "+" = 0
getOperationIdentity "*" = 1
getOperationIdentity op = error $ "invalid operation: " ++ op

splitOn :: (Foldable t, Eq a) => a -> t a -> [[a]]
splitOn delimiter = foldr f [[]]
  where
    f c l@(x : xs)
      | c == delimiter = [] : l
      | otherwise = (c : x) : xs
    f _ [] = []