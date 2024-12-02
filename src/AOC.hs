module AOC (getDay, Part(A, B)) where

import Data.List (transpose, sort)
import Control.Arrow

data Part = A | B
    deriving (Show, Read, Eq)

functions :: [(String -> Int, String -> Int)]
functions = [(day1a, day1b), (day2a, day2b)]

getDay :: Int -> Part -> (String -> Int)
getDay n A = fst $ (!!) functions $ n-1
getDay n B = snd $ (!!) functions $ n-1

parseDay1Lists :: String -> ([Int], [Int])
parseDay1Lists = (head &&& (head . tail)) . 
                 map sort . 
                 transpose .
                 parseNumLineLists

parseNumLineLists :: String -> [[Int]]
parseNumLineLists = map (map read . words) . lines


day1a :: String -> Int
day1a = sum . 
        map abs .
        uncurry (zipWith (-)) . 
        parseDay1Lists

day1b :: String -> Int
day1b = sum . 
        (\(xs, ys) -> map (\x -> (*) x $ length $ filter (==x) ys) xs) .
        parseDay1Lists

day2a :: String -> Int
day2a = length . filter day2aPred . parseNumLineLists

day2aPred :: [Int] -> Bool
day2aPred = uncurry (&&) . 
            first (uncurry (||)) . 
            ((all (>0) &&& all (<0)) &&& all ((<=3) . abs)) . 
            uncurry (zipWith (-)) . 
            (id &&& tail)

day2b :: String -> Int
day2b = length . filter day2bPred . parseNumLineLists

day2bPred :: [Int] -> Bool
day2bPred = let
        filterHelp ls = case ls of
            []      -> []
            (x:xs) -> (:) xs $ map (x:) $ filterHelp xs
    in
    any 
    (   uncurry (&&) . 
        first (uncurry (||)) . 
        ((all (>0) &&& all (<0)) &&& all ((<=3) . abs)) .
        uncurry (zipWith (-)) .
        (id &&& tail)
    ) . 
    uncurry (:) . 
    (id &&& filterHelp)
