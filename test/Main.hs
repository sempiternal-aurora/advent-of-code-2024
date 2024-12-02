module Main (main) where

import Test.HUnit
import AOC (getDay, Part(..))

main :: IO Counts
main = runTestTT tests

tests :: Test
tests = TestList [day1aTest, day1bTest, day2aTest, day2bTest]

day1TestInput :: String
day1TestInput = "3   4\n4   3\n2   5\n1   3\n3   9\n3   3"

day2TestInput :: String
day2TestInput = "7 6 4 2 1\n1 2 7 8 9\n9 7 6 2 1\n1 3 2 4 5\n8 6 4 4 1\n1 3 6 7 9"

day1aTest :: Test
day1aTest = TestCase $ assertEqual "Simple test for day 1 part a as given" 11 (getDay 1 A day1TestInput)

day1bTest :: Test
day1bTest = TestCase $ assertEqual "Simple test for day 1 part b as given" 31 (getDay 1 B day1TestInput)

day2aTest :: Test
day2aTest = TestCase $ assertEqual "Simple test for day 2 part a as given" 2 (getDay 2 A day2TestInput)

day2bTest :: Test
day2bTest = TestCase $ assertEqual "Simple test for day 2 part b as given" 4 (getDay 2 B day2TestInput)



