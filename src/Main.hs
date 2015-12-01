module Main where

import qualified  Data.Map as M
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import Day01

solutions = M.fromList [
                        (1, day01)
                        ]

solution :: Integer -> Maybe (IO ())
solution number = M.lookup number solutions

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["--help"] -> usage >> exitSuccess
        ["-h"] -> usage >> exitSuccess
        [number] -> case solution (read number :: Integer) of
            Just result -> result
            Nothing -> putStrLn "There is no solution yet for this problem"
        _ -> usage >> exitSuccess
    where
        usage = putStrLn "Usage: cabal run [number]"