module Main (main) where

import Data.Char (isSpace)
import Data.List (transpose)

main :: IO ()
main = do
  cont <- getContents

  let matrix = filter (not . all isSpace) . splitOn ' ' <$> lines cont
  let numbers = (read <$>) <$> transpose (take (length matrix - 1) matrix)
  let signs = matrix !! (length matrix - 1)

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