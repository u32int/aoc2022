module Main where

import System.IO

-- I don't know how to split a string on an arbitrary delimiter yet so this is my
-- overcomplicated solution that returns the remaining part of the list instead.
sumUntilNewline :: [String] -> (Integer, [String])
sumUntilNewline list
  | length list == 0 = (0, [])
  | otherwise =
      case head list of
        "" -> (0, tail list)
        _ -> s where
          cs = ((read :: String -> Integer) $ head list)
          r = sumUntilNewline $ tail list
          s = (cs + (fst r), snd r)

totalsList :: [String] -> [Integer]
totalsList [] = []
totalsList list = ret
  where
    r = sumUntilNewline list
    ret = fst r : (totalsList $ snd r)

maxListItem :: [Integer] -> Integer
maxListItem list = foldr max 0 list

topThreeItems :: ([Integer], Int) -> [Integer]
topThreeItems (list, bound)
  | length list <= bound = []
  | otherwise = ret
    where
      m = maxListItem list
      isNotMaxItem a = a /= m
      ret = m : topThreeItems(filter isNotMaxItem list, bound)

main :: IO ()
main = do
  input <- readFile "inputs/day1.txt"
  let totals = totalsList $ lines input

  let maxListItem list = foldr max 0 list
  putStrLn $ "[Part 1] " ++ (show $ maxListItem $ totals)
  putStrLn $ "[Part 2] " ++ (show $ sum $ topThreeItems $ (totals, length(totals) - 3))
                             
