module ConsoleMenu
    ( menuLoop
    ) where

import System.IO (hFlush, stdout)

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
            putStrLn "Вы просматриваете типы номеров."
            menuLoop
        "2" -> do
            putStrLn "Вы забронировали номер."
            menuLoop
        "3" -> putStrLn "Досвидания!"
        _   -> do
            putStrLn "Некорректный выбор. Пожалуйста, попробуйсте еще раз."
            menuLoop
