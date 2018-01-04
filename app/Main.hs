module Main where

import ProjectReader

main :: IO ()
main = do
    pj <- readProject "./domain"
    putStrLn "========================"
    putStrLn $ show pj
    putStrLn "========================"
