module Day07 (solve, part1, part2) where
import Data.Char (isDigit)
import Data.List.Split (splitOn)
import Prelude hiding (lookup)
import qualified Data.Map as Map

data Directory = Directory {
    size :: Int, 
    parent :: String,
    subDirs :: [String]
    } deriving (Show, Eq)

type FileSystem = Map.Map String Directory

-- reads the size of a file after an ls command
fileSize :: String -> Int
fileSize command
    | all isDigit $ head $ words command = read $ head $ words command
    | otherwise = 0

-- Creates a new directory in the filesystem
mkdir :: FileSystem -> String -> String -> FileSystem
mkdir fs parent dirName = Map.insert dirName Directory {
    size = 0, 
    parent = parent, 
    subDirs = []
    } $ Map.adjust (\d -> d {subDirs = dirName : subDirs d}) parent fs
    -- copilot wrote the last line

-- Get the subdirectories, accounting for the Maybeness of Map.lookup
getSubDirs :: Maybe Directory -> [String]
getSubDirs Nothing = []
getSubDirs (Just d) = subDirs d

-- Changes from the current directory to the given one, 
-- or creates it if not already in the filesystem
changeDir :: FileSystem -> String -> String -> (String, FileSystem)
changeDir fs currentDir toDir
    | elem toDir $ getSubDirs (Map.lookup currentDir fs) = (toDir, fs)
    | not . Map.member toDir $ fs = (toDir, mkdir fs currentDir toDir)
    | otherwise = (currentDir, fs) -- bad command

-- -- Parse the commands into a tree of directories
-- parseCommands :: Directory -> String -> Directory
-- parseCommands command
--     | command == "$ cd .." = 
--     | (take 4 command) == "$ cd" = 

-- Placeholder for part 1
part1 :: String -> Int
part1 contents = 0

-- Placeholder for part 2
part2 :: String -> Int
part2 contents = 0

solve :: String -> IO ()
solve contents = putStrLn "--- Day 07 ---" >>
    print (part1 contents) >> print (part2 contents)