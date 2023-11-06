module Main (main) where

import ConsoleMenu

main :: IO ()
main = do
    putStrLn "Добро пожаловать в систему отель!"
    menuLoop
