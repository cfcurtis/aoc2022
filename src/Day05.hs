module Day05 (solve) where
import Data.List.Split (splitOn)
import Data.Char (isDigit, isSpace, digitToInt)
import Data.List (transpose)

-- Parses the crate stack from the input
readCrates :: String -> [String]
readCrates contents =
    map (init . dropWhile isSpace) -- get rid of the leading space and crate numbering
    $ filter (isDigit . last) $ transpose -- transpose to get the crates in columns
    $ lines . head $ splitOn "\n\n" contents -- split the crate definition from instructions

-- Parses the instructions from the input
readInstructions :: String -> [[Int]]
readInstructions contents =
    [map digitToInt $ filter isDigit line | -- drop the text from the instructions
    line <- lines $ last $ splitOn "\n\n" contents] -- split the crate definition from instructions

pop :: String -> String
pop [] = []
pop (x:xs) = xs

push :: String -> Char -> String
push xs x = x:xs

moveCrate :: [String] -> [Int] -> [String]
moveCrate crates (num:from:to:rest)
    | num == 0 = crates
    | num == 1 =
        take (from - 1) crates
        ++ [tail $ crates !! (from - 1)]
        ++ take (to - from - 1) (drop from crates)
        ++ [head (crates !! (from - 1)) : (crates !! (to - 1))]
        ++ take (length crates - to) (drop to crates)
    | num == 1 && from > to =
        take (to - 1) crates
        ++ [head (crates !! (from - 1)) : (crates !! (to - 1))]
        ++ take (from - to - 1) (drop to crates)
        ++ [tail $ crates !! (from - 1)]
        ++ take (length crates - from) (drop from crates)
    | from == to = crates
    | otherwise = moveCrate crates [num - 1, from, to]
moveCrate _ _ = []

-- Which crates are on top of the stacks?
part1 :: String -> String
part1 contents = head $ readCrates contents

-- Placeholder for part 2
part2 :: String -> Int
part2 contents = 0

solve :: String -> IO ()
solve contents = putStrLn "--- Day 05 ---" >>
    putStrLn (part1 contents) >> print (part2 contents)