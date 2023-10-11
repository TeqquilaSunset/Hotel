module Main (main) where

import ConsoleMenu

main :: IO ()
main = do
    putStrLn "Welcome to the Hotel!"
    menuLoop
