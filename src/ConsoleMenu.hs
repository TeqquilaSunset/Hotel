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
    putStrLn "\nВы находитесь в главном меню системы отель."
    putStrLn "Вы можете открыть следующие пункты:"
    putStrLn "1. Посмотреть типы комнат"
    putStrLn "2. Забронировать номер"
    putStrLn "3. Выйти"
    putStr "Введите свой выбор: "
    hFlush stdout  -- чтобы пользователь видел вопрос перед тем, как вводить ответ, строка не сносится
    choice <- getLine
    case choice of
        "1" -> do
            getType 0
            menuLoop
        "2" -> do
            bookingRoom
            putStrLn "\nПроцесс бронирования закончен.\n"
            menuLoop
        "3" -> putStrLn "Досвидания!"
        _   -> do
            putStrLn "Некорректный выбор. Пожалуйста, попробуйсте еще раз."
            menuLoop
