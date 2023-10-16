{-# LANGUAGE OverloadedStrings #-}

module Booking
    ( bookRoom
    ) where

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Control.Monad (void)
import Text.Printf (printf)

data Room = Room {
    roomNumber    :: Int,
    roomTypeId    :: Int,
    status        :: String
} deriving (Show)

instance FromRow Room where
  fromRow = Room <$> field <*> field <*> field

-- Получить список доступных номеров по датам
getAvailableRooms :: Connection -> String -> String -> IO [Room]
getAvailableRooms conn checkInDate checkOutDate = 
    query conn 
        "SELECT * FROM Rooms WHERE Room_Number NOT IN \
        \(SELECT Room_Number FROM Bookings \
        \WHERE (Check_In_Date BETWEEN ? AND ?) \
        \OR (Check_Out_Date BETWEEN ? AND ?) \
        \OR (? BETWEEN Check_In_Date AND Check_Out_Date) \
        \OR (? BETWEEN Check_In_Date AND Check_Out_Date));" 
        (checkInDate, checkOutDate, checkInDate, checkOutDate, checkInDate, checkOutDate)

-- Функция для вывода одного Room в виде строки таблицы
printRoom :: Room -> IO ()
printRoom r = 
    printf "%-13d %-13d %-25s\n" 
           (roomNumber r) 
           (roomTypeId r) 
           (status r)

-- Бронирование номера
bookRoomProcedure :: Connection -> Int -> String -> String -> Int -> String -> IO ()
bookRoomProcedure conn roomNumber checkInDate checkOutDate guestCount paymentMethod = do
    execute conn 
        "INSERT INTO Bookings (Room_Number, Check_In_Date, Check_Out_Date, Guest_Count, Payment_Method, Payment_Status) \
        \VALUES (?, ?, ?, ?, ?, ?)" 
        (roomNumber, checkInDate, checkOutDate, guestCount, paymentMethod, False :: Bool)  -- Предполагаем, что статус оплаты False

bookRoom :: IO ()
bookRoom = do
    conn <- open "hotel.db"

    -- Получить ввод пользователя
    putStrLn "Введите дату заезда (YYYY-MM-DD):"
    checkInDate <- getLine
    putStrLn "Введите дату выезда (YYYY-MM-DD):"
    checkOutDate <- getLine

    -- Получить список доступных номеров и вывести их
    availableRooms <- getAvailableRooms conn checkInDate checkOutDate
    printf "%-13s %-13s %-25s\n" ("Номер комнаты" :: String) ("ID типа комнаты" :: String) ("Статус" :: String)
    putStrLn $ replicate 53 '-'
    mapM_ printRoom availableRooms
    
    -- Получить выбор номера и информацию для бронирования от пользователя
    putStrLn "Выберите номер комнаты для бронирования:"
    roomNumberStr <- getLine
    let roomNumber = read roomNumberStr :: Int
    
    putStrLn "Введите количество гостей:"
    guestCountStr <- getLine
    let guestCount = read guestCountStr :: Int

    putStrLn "Выберите метод оплаты:"
    paymentMethod <- getLine

    -- Процедура бронирования номера
    bookRoomProcedure conn roomNumber checkInDate checkOutDate guestCount paymentMethod
    
    putStrLn "Бронирование успешно завершено!"
    close conn
