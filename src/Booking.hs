{-# LANGUAGE OverloadedStrings #-}

module Booking
    ( bookingRoom
    ) where

import Database.SQLite.Simple
import Data.Time
import Text.Read (readMaybe)
import Control.Monad
import Client

-- Функция для получения даты от пользователя с проверкой ввода
getDate :: String -> IO Day
getDate prompt = do
    putStrLn prompt
    dateString <- getLine
    case parseTimeM True defaultTimeLocale "%Y-%m-%d" dateString :: Maybe Day of
        Just date -> return date
        Nothing -> do
            putStrLn "Некорректный ввод. Пожалуйста, попробуйте еще раз."
            getDate prompt

-- Функция для получения количества гостей от пользователя с проверкой ввода
getGuestCount :: IO Int
getGuestCount = do
    putStrLn "Введите количество гостей:"
    guestCountStr <- getLine
    case readMaybe guestCountStr of
        Just count -> return count
        Nothing -> do
            putStrLn "Некорректный ввод. Пожалуйста, попробуйте еще раз."
            getGuestCount

getRoomPreference :: IO Bool
getRoomPreference = do
    putStrLn "Есть ли у вас конкретные предпочтения по типу номера? (1 для Да, 0 для Нет):"
    preferenceStr <- getLine
    return $ preferenceStr == "1"


-- Определение типа для номера комнаты
data Room = Room
    { roomNum :: Int
    , roomType :: String
    , pricePerNight :: Float
    , roomCapacity :: Int
    } deriving (Show)

-- Определение типа для информации о бронировании
data BookingInfo = BookingInfo {
    roomNumber :: Int,
    checkInDate :: Day,
    checkOutDate :: Day,
    guestCount :: Int,
    paymentMethod :: String,
    paymentStatus :: Bool
} deriving (Show)

getAvailableRoomTypes :: Connection -> Day -> Day -> Int -> IO [(String, Float)]
getAvailableRoomTypes conn checkInDate checkOutDate minGuestCount = do
    rows <- query conn
        "WITH AvailableRoomNumbers AS ( \
        \    SELECT Rooms.Room_Number FROM Rooms \
        \    LEFT JOIN Bookings ON Rooms.Room_Number = Bookings.Room_Number \
        \    WHERE (Bookings.Check_In_Date IS NULL OR Bookings.Check_Out_Date <= ? OR Bookings.Check_In_Date >= ?) \
        \    GROUP BY Rooms.Room_Number \
        \) \
        \SELECT DISTINCT Room_Types.Name_Type, Room_Types.Price_Per_Night \
        \FROM Rooms \
        \JOIN Room_Types ON Rooms.Room_Type_ID = Room_Types.Room_Type_ID \
        \JOIN AvailableRoomNumbers ON Rooms.Room_Number = AvailableRoomNumbers.Room_Number \
        \WHERE Room_Types.Room_Capacity >= ?"
        (checkInDate, checkOutDate, minGuestCount)
    return rows

bookRoom :: Connection -> String -> Day -> Day -> Int -> [Int] -> IO ()
bookRoom conn roomType checkInDate checkOutDate guestCount clientIDs = do
    rows <- query conn
        "WITH AvailableRoom AS ( \
        \    SELECT Rooms.Room_Number FROM Rooms \
        \    JOIN Room_Types ON Rooms.Room_Type_ID = Room_Types.Room_Type_ID \
        \    LEFT JOIN Bookings ON Rooms.Room_Number = Bookings.Room_Number \
        \    WHERE Room_Types.Name_Type = ? \
        \    AND (Bookings.Check_In_Date IS NULL OR Bookings.Check_Out_Date <= ? OR Bookings.Check_In_Date >= ?) \
        \    LIMIT 1 \
        \) \
        \SELECT Rooms.Room_Number, Room_Types.Price_Per_Night \
        \FROM Rooms \
        \JOIN Room_Types ON Rooms.Room_Type_ID = Room_Types.Room_Type_ID \
        \JOIN AvailableRoom ON Rooms.Room_Number = AvailableRoom.Room_Number"
        (roomType, checkInDate, checkOutDate) :: IO [(Int, Float)]
    case rows of
        ((roomNumber, pricePerNight):_) -> do
            let bookingInfo = BookingInfo {
                roomNumber = roomNumber,
                checkInDate = checkInDate,
                checkOutDate = checkOutDate,
                guestCount = guestCount,
                paymentMethod = "Credit Card",
                paymentStatus = True
            }
            createBooking conn bookingInfo clientIDs
        [] -> putStrLn "Извините, нет доступных номеров выбранного типа."

createBooking :: Connection -> BookingInfo -> [Int] -> IO ()
createBooking conn bookingInfo clientIDs = do
    execute conn
        "INSERT INTO Bookings (Room_Number, Check_In_Date, Check_Out_Date, Guest_Count, Payment_Method, Payment_Status) \
        \VALUES (?, ?, ?, ?, ?, ?)"
        (roomNumber bookingInfo, checkInDate bookingInfo, checkOutDate bookingInfo, guestCount bookingInfo, paymentMethod bookingInfo, paymentStatus bookingInfo)
    bookingID <- lastInsertRowId conn
    forM_ clientIDs $ \clientID -> do
        execute conn
            "INSERT INTO ClientsBookings (Client_ID, Booking_ID) VALUES (?, ?)"
            (clientID, bookingID)

getRoomTypeIndex :: String -> IO Int
getRoomTypeIndex prompt = do
    putStrLn prompt
    input <- getLine
    case readMaybe input of
        Just index -> return index
        Nothing -> do
            putStrLn "Некорректный ввод. Пожалуйста, попробуйте еще раз."
            getRoomTypeIndex prompt

getClientIDs :: String -> IO [Int]
getClientIDs prompt = do
    putStrLn prompt
    input <- getLine
    case mapM readMaybe (words $ map (\c -> if c == ',' then ' ' else c) input) of
        Just ids -> return ids
        Nothing -> do
            putStrLn "Некорректный ввод. Пожалуйста, попробуйте еще раз."
            getClientIDs prompt


bookingRoom :: IO ()
bookingRoom = do
    conn <- open "hotel.db"
    
    checkInDate <- getDate "Введите дату заезда (ГГГГ-ММ-ДД):"
    checkOutDate <- getDate "Введите дату выезда (ГГГГ-ММ-ДД):"
    guestCount <- getGuestCount
    
    availableRoomTypes <- getAvailableRoomTypes conn checkInDate checkOutDate guestCount
    unless (null availableRoomTypes) $ do
        putStrLn "Выберите один из доступных типов номеров:"
        forM_ (zip [1..] availableRoomTypes) $ \(i, (roomType, pricePerNight)) ->
            putStrLn $ show i ++ ". Тип: " ++ roomType ++ ", Стоимость за ночь: " ++ show pricePerNight ++ " руб."
        
        roomTypeIndex <- getRoomTypeIndex "Введите номер выбранного типа номера:"
        let (selectedRoomType, _) = availableRoomTypes !! (roomTypeIndex - 1)
        

        putStrLn "Введите количество гостей: "
        input <- getLine
        let number = read input :: Int

        clientIDs <- addClientsAndPassports number
        
        bookRoom conn selectedRoomType checkInDate checkOutDate guestCount clientIDs
    
    close conn
