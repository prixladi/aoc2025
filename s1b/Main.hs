module Main (main) where

main :: IO ()
main = do
  cont <- getContents
  print (processInstructions (lines cont))

processInstructions :: [String] -> Int
processInstructions = fst . foldr (processInstruction . parseInstruction) (0, 50)

processInstruction :: Int -> (Int, Int) -> (Int, Int)
processInstruction 0 acc = acc
processInstruction ins (cnt, pos) = (cnt + rotations + overflow, newPos)
  where
    rotations = div (abs ins) 100
    total = pos + rem ins 100
    newPos = mod total 100
    overflow = if ((total >= 100) || (total <= 0)) && pos /= 0 then 1 else 0

parseInstruction :: String -> Int
parseInstruction ('L' : rest) = -(1 * read rest)
parseInstruction ('R' : rest) = read rest
parseInstruction ins = error ("Invalid instruction " ++ ins)
