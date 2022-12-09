module Day07 (solve, part1, part2) where
import Data.Char (isDigit)
import Data.List.Split (splitOn)
import Prelude hiding (lookup)
import qualified Data.Map as Map
import Data.Maybe ( fromMaybe )
import Data.IntMap (findWithDefault, singleton)

data Directory = Directory {
    size :: Int, 
    parent :: String,
    subDirs :: [String]
    } deriving (Show, Eq)

type FileSystem = Map.Map String Directory

-- Creates a new directory in the filesystem
mkdir :: FileSystem -> String -> String -> FileSystem
mkdir fs parent dirName = Map.insert dirName Directory {
    size = 0, 
    parent = parent, 
    subDirs = []
    } $ Map.adjust (\d -> d {subDirs = dirName : subDirs d}) parent fs
    -- copilot wrote the last line
    -- lambda function: \ is the lambda operator, d is the parent directory, 
    -- so we're concatenating the subDirs list of the parent with the new directory name

-- The root directory
root:: Directory
root = Directory {size=0, parent="/", subDirs=[]}

-- Changes from the current directory to the given one, 
-- or creates it if not already in the filesystem
changeDir :: FileSystem -> String -> String -> (String, FileSystem)
changeDir fs currentDir toDir
    | toDir == ".." = (parent $ Map.findWithDefault root currentDir fs, fs)
    | elem toDir $ subDirs (Map.findWithDefault root currentDir fs) = (toDir, fs)
    | not . Map.member toDir $ fs = (toDir, mkdir fs currentDir toDir)
    | otherwise = (currentDir, fs) -- Can't change to this directory from here

-- parse the ls output
parseLs :: String -> (String, FileSystem) -> (String, FileSystem)
parseLs lsOutput (currentDir, fs) 
    | all isDigit $ head $ words lsOutput = (currentDir, Map.adjust (\d -> d {size = size + read $ head $ words lsOutput} currentDir fs)

-- -- Parse the commands into a tree of directories
applyCommands :: (String, String) -> (String, FileSystem) -> (String, FileSystem)
applyCommands (cmd, operand) (currentDir, fs)
    | "cd" = changeDir fs currentDir operand
    | "ls" = parseLs operand (currentDir, fs)

-- Placeholder for part 1
part1 :: String -> Int
part1 contents = 0

-- Placeholder for part 2
part2 :: String -> Int
part2 contents = 0

solve :: String -> IO ()
solve contents = putStrLn "--- Day 07 ---" >>
    print (part1 contents) >> print (part2 contents)