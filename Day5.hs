module Main where

import System.IO

startsWith :: String -> String -> Bool
startsWith [] _ = True
startsWith _ [] = False
startsWith (ax:axs) (bx:bxs) =
  if ax /= bx then
    False
  else startsWith axs bxs

splitOnce :: String -> String -> (String, String)
splitOnce _ [] = ([], [])
splitOnce delim str 
  | startsWith delim str = ([], tail str)
  | otherwise  = ret
    where
      (f,b) = splitOnce delim (tail str)
      ret = (head str : f, b)

splitString :: String -> String -> [String]
splitString delim str
  | null next = [prev]
  | otherwise = prev : splitString delim next
    where
      (prev, next) = splitOnce delim str

elemIndex :: Int -> Int
elemIndex n = 1 + n * 4

elemsUpToBound :: Int -> Int -> [Int]
elemsUpToBound id bound 
  | nth >= bound = []
  | otherwise  = nth : (elemsUpToBound (id+1) bound)
    where
      nth = elemIndex id

-- Not very happy with this, probably can be done without the extra Int
intoStacks :: Int -> [String] -> [String]
intoStacks id list
  | id < (length $ last list) = filter (\x -> x /= ' ')
                                (map (\x -> if id < length x then x !! id else ' ') list)
                                : intoStacks (id+1) list
  | otherwise = []

parseLayout :: [String] -> [String]
parseLayout [] = []
parseLayout (x:xs) = ret
  where
    chars = map (\a -> x !! a) $ elemsUpToBound 0 $ length x
    ret = chars : parseLayout xs

replaceAt :: Int -> [String] -> String -> [String]
replaceAt n list newstr = ret
  where
    (ys,zs) = splitAt n list
    ret = if null ys then [newstr] ++ tail zs else ys ++ [newstr] ++ tail zs

move :: [String] -> (Int, Int, Int) -> Bool -> [String]
move stacks (n, from, to) p2 = ret
  where
    new_from = drop n (stacks !! from)
    moved    = take n (stacks !! from)
    new_to   = (if p2 then moved else reverse moved) ++ (stacks !! to)

    intr = replaceAt from stacks new_from
    ret  = replaceAt to intr new_to

solve :: [String] -> [String] -> Bool -> String
solve stacks [] _ = map (\x -> if null x then ' ' else x !! 0) stacks
solve stacks (curinstr:rest) p2 = solve (move stacks (n, from, to) p2) rest p2
  where
    split = splitString " " curinstr
    n = (read $ split !! 1)
    from = (read $ split !! 3) - 1
    to = (read $ split !! 5) - 1

main :: IO ()
main = do
  input <- readFile "inputs/day5.txt"
  let (layout, instructions) = splitOnce "\n\n" input
  let stacks = intoStacks 0 $ parseLayout . init $ lines layout

  putStrLn $ "[Part 1] " ++ solve stacks (tail $ lines instructions) False
  putStrLn $ "[Part 2] " ++ solve stacks (tail $ lines instructions) True
