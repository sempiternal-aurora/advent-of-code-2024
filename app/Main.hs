module Main where

import qualified AOC (Part, getDay)
import System.Environment (getArgs)

main :: IO ()
main = do
    args    <- getArgs
    inFile  <- readFile $ head $ tail $ tail args
    let day  = (read $ head args :: Int)
    let part = (read $ head $ tail args :: AOC.Part)
    let func = AOC.getDay day part
    let res  = func inFile
    putStrLn $ "The result is: " ++ show res


    
