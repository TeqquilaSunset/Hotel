{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}
module ConsoleMenu
    ( menuLoop
    ) where

import System.IO (hFlush, stdout)
import RoomType
import Booking
import Client


menuLoop :: IO ()
menuLoop = do
    putStrLn "\n1. Посмотреть типы комнат"
    putStrLn "2. Забронировать номер"
    putStrLn "3. Выйти"
    putStr "Введите свой выбор: "
    hFlush stdout  -- чтобы пользователь видел вопрос перед тем, как вводить ответ
    choice <- getLine
    case choice of
        "1" -> do
            getType
            menuLoop
        "2" -> do
            bookRoom
            menuLoop
        "3" -> putStrLn "Досвидания!"
        "4" -> do
            addClientsAndPassports
            menuLoop
        _   -> do
            putStrLn "Некорректный выбор. Пожалуйста, попробуйсте еще раз."
            menuLoop
