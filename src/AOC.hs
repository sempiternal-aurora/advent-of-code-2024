module AOC (getDay, Part(A, B)) where

import Data.List
import Control.Arrow
import Text.ParserCombinators.ReadP
import Data.Char

data Part = A | B
    deriving (Show, Read, Eq)

functions :: [(String -> Int, String -> Int)]
functions = [(day1a, day1b), (day2a, day2b), (day3a, day3b), (day4a, day4b)]

getDay :: Int -> Part -> String -> Int
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


day3a :: String -> Int
day3a = foldr (curry $ uncurry (+) . first helper) 0 . fst . head .  filter ((=="") . snd) . readP_to_S day3aParser
    where 
        helper asm = case asm of
            Mul x y -> x * y
            _       -> 0

data Day3ASM = Mul Int Int | Do | Don't | Noop
    deriving (Eq, Show)

day3aParser :: ReadP [Day3ASM]
day3aParser = many (mulParser <++ noopParser)

mulParser :: ReadP Day3ASM
mulParser = let
        digit = foldr1 (<++) (map (flip count $ satisfy isDigit) [3,2,1])
        digits = uncurry Mul . (read . head &&& read . last) <$> sequence [digit, string ",", digit]
    in between (string "mul(") (char ')') digits

day3b :: String -> Int
day3b = helper True . fst . head . filter ((=="") . snd) . readP_to_S day3bParser
    where
        helper b asms = case asms of
            (Mul x y):asms' -> if b then x * y + helper b asms' else helper b asms'
            Do:asms'        -> helper True asms'
            Don't:asms'     -> helper False asms'
            Noop:asms'      -> helper b asms'
            []              -> 0

day3bParser :: ReadP [Day3ASM]
day3bParser = many $ foldr1 (<++) [mulParser, doParser, don'tParser, noopParser]

noopParser :: ReadP Day3ASM
noopParser = Noop <$ satisfy (const True)

don'tParser :: ReadP Day3ASM
don'tParser = Don't <$ string "don't()"

doParser :: ReadP Day3ASM
doParser = Do <$ string "do()"

day4a :: String -> Int
day4a = sum . map (sum . map test) . (trans <*>) . pure . lines
    where
        trans :: [[String] -> [String]]
        trans = [ id, transpose, shear, shear . map reverse]
        
        test :: String -> Int
        test xs@('X':'M':'A':'S':_) = 1 + test (tail xs)
        test xs@('S':'A':'M':'X':_) = 1 + test (tail xs)
        test (_:xs)                 = test xs
        test []                     = 0
    
shear :: [[a]] -> [[a]]
shear = foldr zipConsSkew []

zipConsSkew :: [a] -> [[a]] -> [[a]]
zipConsSkew xt yss =
    uncurry (:) $
    case xt of
        x:xs    -> ([x], zipCons xs yss)
        []      -> ([], yss)

zipCons :: [a] -> [[a]] -> [[a]]
zipCons (x:xs) (y:ys)   = (x:y):zipCons xs ys
zipCons xs      []      = map (:[]) xs
zipCons []      ys      = ys

day4b :: String -> Int
day4b = sum . map (length . filter crossMasTest . (trans <*>)) . get3by3 . lines
    where
        trans :: [[String] -> [String]]
        trans = [ id, transpose, reverse, reverse . transpose ]


crossMasTest :: [String] -> Bool
crossMasTest [x,y,z] = row1 && row2 && row3 
    where
        row1 = case x of
            ['M',_,'M'] -> True
            _           -> False
        row2 = case y of
            [_,'A',_]   -> True
            _           -> False
        row3 = case z of
            ['S',_,'S'] -> True
            _           -> False
crossMasTest _          = False

get3by3 :: [[Char]] -> [[[[Char]]]]
get3by3 = foldr helper [[]]
    where
        helper :: [a] -> [[[[a]]]] -> [[[[a]]]]
        helper xt yss = map (:[]) sliced : map (zipCons sliced) hd ++ tl
            where
                sliced  = slice 3 xt
                (hd,tl) = splitAt 2 yss

        slice :: Int -> [a] -> [[a]]
        slice n ls@(_:xs) = take n ls : slice n xs
        slice _ []        = []
        
