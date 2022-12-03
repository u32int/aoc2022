module Main where

import System.IO
import Data.Char (ord)
import Data.List (sort, nub)
import Data.Set  (toList, fromList)

inList :: Char -> [Char] -> Bool
inList _ [] = False
inList chr (x:xs)
  | x == chr = True
  | otherwise = inList chr xs

findListDuplicate :: [Char] -> [Char] -> Char
findListDuplicate [] _ = undefined
findListDuplicate (x:xs) list
  | inList x list = x
  | otherwise = findListDuplicate xs list


{- Not a huge fan of this solution because of its time complexity (O(n^3)?) but idk
   if it's possible to do better. Maybe sorting + bin search would be more efficient
   for large enough inputs.
-}

solve :: [String] -> Int
solve [] = 0
solve (curr:rest) = ret
  where (left, right) = splitAt (length curr `div` 2) curr
        dup = findListDuplicate left right
        priority
          | ord dup >= ord 'a' = ord dup - ord 'a' + 1
          | otherwise          = ord dup - ord 'A' + 27
        ret = priority + solve(rest)


{- Given a sorted list of Ints, traverse it with a 3 int wide window.
   If they match, return the Int.
-} 
findListTriple :: [Int] -> Int
findListTriple [] = undefined
findListTriple list 
  | all (\x -> x == list !! 0) $ take 3 list = list !! 0
  | otherwise = findListTriple $ drop 1 list

-- Not sure if this is much better than just the most naive approach
solveTwo :: [String] -> Int
solveTwo [] = 0
solveTwo list = ret
  where curr = (foldr (++) [] $ map (toList . fromList) $ take 3 list)
        sorted = sort $ map (
          \x -> if ord x >= ord 'a'
                then ord x - ord 'a' + 1
                else ord x - ord 'A' + 27) curr
        ret = findListTriple sorted + (solveTwo $ drop 3 list)
        
main :: IO ()
main = do
  input <- readFile "inputs/day3.txt"
  -- input <- readFile "foo.txt"

  putStrLn $ "[Part 1] " ++ (show . solve    $ lines input)
  putStrLn $ "[Part 2] " ++ (show . solveTwo $ lines input)
