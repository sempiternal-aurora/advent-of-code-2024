module AOC (getDay, Part(A, B)) where

import Data.List
import Control.Arrow
import Text.ParserCombinators.ReadP
import Data.Char
import Data.Tuple
import Data.Maybe
import Debug.Trace

data Part = A | B
    deriving (Show, Read, Eq)

functions :: [(String -> Int, String -> Int)]
functions = [(day1a, day1b), (day2a, day2b), (day3a, day3b), (day4a, day4b), (day5a, day5b), (day6a, day6b)]

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
        

day5a :: String -> Int
day5a = sum . map middle . (\(rules, updates) -> filter (day5Pred rules) updates) . day5Splitter . break null . lines

day5Pred :: [(Int,Int)] -> [Int] -> Bool
day5Pred rules = all (uncurry helper) . (predRules rules <*>) . pure
    where
        helper :: Int -> (Int, Int) -> Bool
        helper l (x,y) = l == x || l == y || x <= y

getProblem :: [(Int,Int)] -> [Int] -> (Int, Int)
getProblem rules = head . mapMaybe (uncurry helper) . (predRules rules <*>) . pure
    where
        helper :: Int -> (Int, Int) -> Maybe (Int, Int)
        helper l (x,y) = if l == x || l == y || x <= y then Nothing else Just (x,y)
        
predRules :: [(Int, Int)] -> [[Int] -> (Int, (Int, Int))]
predRules = map ((length &&&) . predicate')

predicate' :: (Int, Int) -> [Int] -> (Int, Int) 
predicate' = uncurry (&&&) . both (length .: takeWhile . (/=))

day5Splitter :: ([String], [String]) -> ([(Int, Int)], [[Int]])
day5Splitter = map ((read *** read . tail) . break (=='|')) *** tail . map (map read . splitWhen (==','))

(.:) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
f .: g = \x y -> f $ g x y
infixr 8 .:

both :: (a -> b) -> (a,a) -> (b,b)
both f = f *** f

middle :: [a] -> a
middle [] = error "No middle of empty list"
middle [a] = a
middle [_,_] = error "Even number of elements has no middle"
middle xs = middle $ tail $ init xs

splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen _ [] = []
splitWhen f xs = let
        (hd, tl) = break f xs
    in case tl of 
        [] -> [hd]
        _:xs' -> hd : splitWhen f xs'

day5b :: String -> Int
day5b = 
    sum . 
    uncurry helper .
    day5Splitter . 
    break null . 
    lines
    where
        helper :: [(Int, Int)] -> [[Int]] -> [Int]
        helper rules = map (middle . helper' rules) . filter (not . day5Pred rules)
        
        helper' :: [(Int, Int)] -> [Int] -> [Int]
        helper' rules ls = if day5Pred rules ls then ls else let 
                (x,y) = getProblem rules ls 
            in helper' rules $ swapList x y ls

swapList :: Show a => Int -> Int -> [a] -> [a]
swapList x y
    | x < y = uncurry (++) . 
              second (
                uncurry (++) . 
                uncurry helper .
                ((swap . both head) &&& both tail) . 
                splitAt (y-x)
              ) . splitAt x
    | x == y = id
    | otherwise = swapList y x
    where
        helper :: (a,a) -> ([a], [a]) -> ([a], [a])
        helper (x',y') (xs,ys) = (x':xs,y':ys)

data Dir = N | S | E | W

nextDir :: Dir -> Dir
nextDir N = E
nextDir E = S
nextDir S = W
nextDir W = N

replace :: Int -> Int -> a -> [[a]] -> [[a]]
replace x y a = uncurry (++) . 
    second (
        uncurry (:) . first (
            uncurry (++) .
            second (
                uncurry (:) . first (const a) . fromJust . uncons
            ) .
            splitAt x
        ) . fromJust . uncons
    ) . 
    splitAt y

day6a :: String -> Int
day6a s = let
        grid = lines s
        rows = length grid
        cols = length $ head grid
        guardPos = length $ takeWhile (/='^') $ concat grid
        guardXY = (guardPos `mod` cols, guardPos `div` rows)
        next N (x,y) = (x,y-1)
        next S (x,y) = (x,y+1)
        next E (x,y) = (x+1,y)
        next W (x,y) = (x-1,y)
        inside x y = x >= 0 && x < cols && y >= 0 && y < rows
        helper :: Dir -> (Int, Int) -> [[Char]] -> Int
        helper d (x,y) gr = 
            let
                (x',y') = next d (x,y)
                rotate = (gr' !! y' !! x') == '#'
                d' = if rotate then nextDir d else d
                (x'', y'') = if rotate then (x,y) else (x',y')
                gr' = replace x y 'X' gr
            in 
                if inside x' y'
                then helper d' (x'',y'') gr'
                else (sum . map (length . filter (=='X'))) gr'
    in helper N guardXY grid
        
day6b :: String -> Int
day6b = undefined
