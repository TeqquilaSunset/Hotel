module Main (main) where

import Lib 
import ConsoleMenu

main :: IO ()
main = do
    putStrLn "Welcome to the Hotel!"
    menuLoop
