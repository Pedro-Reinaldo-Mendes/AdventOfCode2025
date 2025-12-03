-- Author: Pedro Reinaldo Mendes (https://pedrorm.com)
-- Version: v1.0

-- For this module to work fine, we're at all times thinking on well-formatted input as the explained in the instructions
module Puzzle1 where

import System.IO
import Data.List (nub)

-- Given a certain delimiter, split a string into a list of strings
splitStringBy :: Char -> String -> [String]
splitStringBy _ "" = [] -- If no string was assigned, we return an empty list
splitStringBy delimiter str =
    let (start, rest) = break (== delimiter) str
    in case rest of
        -- In case there're no more delimiters, we'll remove the newline char
        [] -> 
            let finalFragment = if last start == '\n'
                                then init start
                                else start
            in [finalFragment]

        -- In case there's still delimiters, we'll use recursion
        (_:remaining) -> start : splitStringBy delimiter remaining

-- Helper function to generate all possible numbers with a certain length which are formed by repeating a sequence
generatePeriodicNumbers :: Int -> [Int]
generatePeriodicNumbers len =
    let 
        -- Find all divisors of len strictly less than len
        divisors = [d | d <- [1 .. len `div` 2], len `mod` d == 0]
        
        -- For each divisor, generate the periodic numbers
        generateForDivisor d = 
            let 
                start = 10 ^ (d - 1)
                end = (10 ^ d) - 1
                -- Generate base numbers of length d
                baseNums = [start .. end]
                -- Repeat the string representation len/d times
                repeats = len `div` d
            in map (\i -> read (concat (replicate repeats (show i))) :: Int) baseNums
            
    in nub (concatMap generateForDivisor divisors)

-- Main function to solve the puzzle
main :: IO ()
main = do
    input <- readFile "input.txt"

    -- We'll obtain our intervals as strings
    let intervalsString :: [[String]]
        intervalsString = map (splitStringBy '-') (splitStringBy ',' input)

    -- print intervalsString -- FOR DEBUGGING PURPOSES

    -- Now, we'll just need to map over the strings and get the integer values
    let intervals :: [[Int]]
        intervals = map (map (\s -> read s :: Int)) intervalsString

    -- print "..........." -- FOR DEBUGGING PURPOSES
    -- print intervals -- FOR DEBUGGING PURPOSES

    -- We'll create a list of every number of digits the interval's values could be composed of
    let possibleLengths :: [[Int]]
        possibleLengths = map (\[x, y] -> [length x .. length y]) intervalsString

    -- print "..........." -- FOR DEBUGGING PURPOSES
    -- print possibleLengths -- FOR DEBUGGING PURPOSES

    -- We'll now zip the intervals with their possible value lengths
    let invalidIDsPerInterval = zipWith (\interval candidatesLen ->
            let
                -- We'll generate all possible periodic numbers for all possible lengths of the correspondent interval
                candidates = concatMap generatePeriodicNumbers candidatesLen

                -- NOTE: I'm aware that we'll be working with periodic numbers that might be certainly outside our interval
                -- because they started to be greater than the max-value of the interval. This may be fixed in the future :)

                -- We'll keep only the periodic numbers that are part of our interval
                inInterval = filter (\x -> x >= head interval && x <= last interval) candidates
            in
                inInterval
            ) intervals possibleLengths

    -- print "..........." -- FOR DEBUGGING PURPOSES
    -- print invalidIDsPerInterval -- FOR DEBUGGING PURPOSES

    print ".....FINAL SUM......"
    print (sum (concat invalidIDsPerInterval)) -- SOLUTION