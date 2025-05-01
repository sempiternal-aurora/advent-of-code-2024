module Main (main) where

import System.Exit
import Test.HUnit
import AOC (getDay, Part(..))

main :: IO ()
main = do
  results <- runTestTT tests
  if errors results + failures results == 0
    then
      exitSuccess
    else
      exitWith (ExitFailure 1)


tests :: Test
tests = TestList [
    day1aTest, day1bTest, 
    day2aTest, day2bTest, 
    day3aTest, day3bTest, 
    day4aTest, day4bTest, 
    day5aTest, day5bTest,
    day6aTest, day6bTest]

day1TestInput :: String
day1TestInput = "3   4\n4   3\n2   5\n1   3\n3   9\n3   3"

day2TestInput :: String
day2TestInput = "7 6 4 2 1\n1 2 7 8 9\n9 7 6 2 1\n1 3 2 4 5\n8 6 4 4 1\n1 3 6 7 9"

day3aTestInput :: String
day3aTestInput = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

day3bTestInput :: String
day3bTestInput = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

day4TestInput :: String
day4TestInput = "MMMSXXMASM\nMSAMXMSMSA\nAMXSXMAAMM\nMSAMASMSMX\nXMASAMXAMM\nXXAMMXXAMA\nSMSMSASXSS\nSAXAMASAAA\nMAMMMXMMMM\nMXMXAXMASX"

day5TestInput :: String
day5TestInput = "47|53\n97|13\n97|61\n97|47\n75|29\n61|13\n75|53\n29|13\n97|29\n53|29\n61|53\n97|53\n61|29\n47|13\n75|47\n97|75\n47|61\n75|61\n47|29\n75|13\n53|13\n\n75,47,61,53,29\n97,61,53,29,13\n75,29,13\n75,97,47,61,53\n61,13,29\n97,13,75,29,47"

day6TestInput :: String
day6TestInput = "....#.....\n.........#\n..........\n..#.......\n.......#..\n..........\n.#..^.....\n........#.\n#.........\n......#..."

day1aTest :: Test
day1aTest = TestCase $ assertEqual "Simple test for day 1 part a as given" 11 (getDay 1 A day1TestInput)

day1bTest :: Test
day1bTest = TestCase $ assertEqual "Simple test for day 1 part b as given" 31 (getDay 1 B day1TestInput)

day2aTest :: Test
day2aTest = TestCase $ assertEqual "Simple test for day 2 part a as given" 2 (getDay 2 A day2TestInput)

day2bTest :: Test
day2bTest = TestCase $ assertEqual "Simple test for day 2 part b as given" 4 (getDay 2 B day2TestInput)

day3aTest :: Test
day3aTest = TestCase $ assertEqual "Simple test for day 3 part a as given" 161 (getDay 3 A day3aTestInput)

day3bTest :: Test
day3bTest = TestCase $ assertEqual "Simple test for day 3 part b as given" 48 (getDay 3 B day3bTestInput)

day4aTest :: Test
day4aTest = TestCase $ assertEqual "Simple test for day 4 part a as given" 18 (getDay 4 A day4TestInput)

day4bTest :: Test
day4bTest = TestCase $ assertEqual "Simple test for day 4 part b as given" 9 (getDay 4 B day4TestInput)

day5aTest :: Test
day5aTest = TestCase $ assertEqual "Simple test for day 5 part a as given" 143 (getDay 5 A day5TestInput)

day5bTest :: Test
day5bTest = TestCase $ assertEqual "Simple test for day 5 part b as given" 123 (getDay 5 B day5TestInput)

day6aTest :: Test
day6aTest = TestCase $ assertEqual "Simple test for day 6 part a as given" 41 (getDay 6 A day6TestInput)

day6bTest :: Test
day6bTest = TestCase $ assertEqual "Simple test for day 6 part b as given" 6 (getDay 6 A day6TestInput)
