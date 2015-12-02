module Day02 where

import Data.List.Split


day02 :: IO ()
day02 = do
    file <- readFile "input/day02"
    putStrLn $ splitAndAggregate presentFolding file
    putStrLn $ splitAndAggregate ribbonKnotting file

splitAndAggregate :: ((Int, Int, Int) -> Int) -> String -> String
splitAndAggregate func = show . sum . map (func . dimensionToTuple) . lines

dimensionToTuple :: String -> (Int, Int, Int)
dimensionToTuple str = (x,y,z)
    where [x,y,z] = map read $ splitOn "x" str

presentFolding :: (Int, Int, Int) -> Int
presentFolding (x, y, z) = 2 * sum sides + minimum sides
    where sides = [x*y, x*z, y*z]

ribbonKnotting :: (Int, Int, Int) -> Int
ribbonKnotting (x, y, z) = x * y * z + 2 * sum dimension - 2 * maximum dimension
    where dimension = [x,y,z]