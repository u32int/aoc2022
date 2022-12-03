module Main where

import System.IO
import Data.Char (ord)

calcPts :: (Int, Int) -> Int
calcPts (left, right) = outcome + right + 1
  where
    outcome
      | left == right = 3
      | (left + 1) `mod` 3 == right = 6
      | otherwise = 0

solve :: [String] -> Int
solve [] = 0
solve (curr:rest) = ret
  where
    left  = curr !! 0
    right = curr !! 2
    ret = calcPts(ord(left) - ord('A'), ord(right) - ord('X')) + solve rest


solveTwo :: [String] -> Int
solveTwo [] = 0
solveTwo (curr:rest) = ret
  where
    right    = curr !! 2
    ordleft  = ord(curr !! 0) - ord('A')
    pts = case right of
      'X' -> 0 + (ordleft + 2) `mod` 3 + 1
      'Y' -> 3 + ordleft + 1
      'Z' -> 6 + (ordleft + 1) `mod` 3 + 1

    ret = pts + (solveTwo $ rest)

main :: IO ()
main = do
  input <- readFile "inputs/day2.txt"

  putStrLn $ "[Part 1] " ++ (show $ solve $ lines input)
  putStrLn $ "[Part 2] " ++ (show $ solveTwo $ lines input)
