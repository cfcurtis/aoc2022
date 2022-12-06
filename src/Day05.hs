module Day05 (solve) where
import Data.List.Split (splitOn)
import Data.Char (isDigit, isSpace, digitToInt, isAlpha)
import Data.List (transpose)
import Debug.Trace (trace)

-- Parses the crate stack from the input
readCrates :: String -> [String]
readCrates contents =
    map (init . dropWhile isSpace) -- get rid of the leading space and crate numbering
    $ filter (isDigit . last) $ transpose -- transpose to get the crates in columns
    $ lines . head $ splitOn "\n\n" contents -- split the crate definition from instructions

-- Parses the instructions from the input
readInstructions :: String -> [[Int]]
readInstructions contents =
    [map read . words $ filter (not . isAlpha) line | -- drop the text from the instructions
    line <- lines $ last $ splitOn "\n\n" contents] -- split the crate definition from instructions

-- manipulate the crates
modStack :: [Char] -> Int -> Int -> (Int, String) -> String
modStack crate from to (index, stack)
    | null crate = stack -- Don't drop an empty crate
    | index == from = drop 1 stack -- drop the crate from the stack if the current index is the from index
    | index == to = crate ++ stack -- push the crate onto the stack if the current index is the to index
    | otherwise = stack -- otherwise, leave the stack unchanged

-- move crates according to the instructions
moveCrate :: [String] -> [Int] -> [String]
moveCrate crates (num:from:to:rest)
    | num == 0 = crates
    | num == 1 = zipWith (curry (modStack crate from to)) [1..(length crates)] crates
    | from == to = crates
    | otherwise = moveCrate (moveCrate crates [1, from, to]) [num - 1, from, to]
        where crate = trace (show crates ++ show [num, from, to]) take 1 $ crates !! (from - 1)
moveCrate _ _ = []

-- Which crates are on top of the stacks?
part1 :: String -> String
part1 contents = map head $ foldl moveCrate (readCrates contents) (readInstructions contents)

-- Placeholder for part 2
part2 :: String -> Int
part2 contents = 0

solve :: String -> IO ()
solve contents = putStrLn "--- Day 05 ---" >>
    putStrLn (part1 contents) >> print (part2 contents)