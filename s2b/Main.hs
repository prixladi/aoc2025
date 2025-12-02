module Main (main) where

main :: IO ()
main = do
  cont <- getContents
  print $ foldr ((+) . sumInvalidCodes) 0 (splitOn ',' cont)

sumInvalidCodes :: String -> Int
sumInvalidCodes range = foldr ((+) . sumInvalidCode) 0 [start .. end]
  where
    rangeTuple = splitOn '-' range
    start = read $ head rangeTuple
    end = read $ rangeTuple !! 1

sumInvalidCode :: Int -> Int
sumInvalidCode code | isInvalidCode (show code) = code
sumInvalidCode _ = 0

isInvalidCode :: String -> Bool
isInvalidCode code | length code < 2 = False
isInvalidCode code = any matcher [1 .. (div (length code) 2)]
  where
    matcher len = matchPrefix (take len code) (drop len code)

matchPrefix :: String -> String -> Bool
matchPrefix _ [] = True
matchPrefix prefix str | length str < length prefix = False
matchPrefix prefix str = eq && matchPrefix prefix rest
  where
    prefixLen = length prefix
    rest = drop prefixLen str
    eq = prefix == take prefixLen str

splitOn :: (Foldable t, Eq a) => a -> t a -> [[a]]
splitOn delimiter = foldr f [[]]
  where
    f c l@(x : xs)
      | c == delimiter = [] : l
      | otherwise = (c : x) : xs
    f _ [] = []