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
        Just date -> do
            currentDate <- utctDay <$> getCurrentTime
            if date < currentDate
                then do
                    putStrLn "\nВведенная дата раньше текущей. Пожалуйста, попробуйте еще раз."
                    getDate prompt
                else
                    return date
        Nothing -> do
            putStrLn "\nНекорректный ввод. Пожалуйста, попробуйте еще раз."
            getDate prompt

-- Функция для ввода даты выезда с проверкой и повторным запросом при ошибке
getCheckOutDate :: Day -> IO Day
getCheckOutDate checkInDate = do
    checkOutDate <- getDate "Введите дату выезда (ГГГГ-ММ-ДД):"
    if checkOutDate > checkInDate
        then return checkOutDate
        else do
            putStrLn "Дата выезда должна быть позже даты заезда и отличаться хотя бы на 1 день. Пожалуйста, попробуйте еще раз."
            getCheckOutDate checkInDate

-- Функция для получения количества гостей от пользователя с проверкой ввода
getGuestCount :: IO Int
getGuestCount = do
    putStrLn "Введите количество гостей:"
    guestCountStr <- getLine
    case readMaybe guestCountStr of
        Just count
            | count > 0 -> return count
            | otherwise -> do
                putStrLn "Количество гостей должно быть положительным числом. Пожалуйста, попробуйте еще раз."
                getGuestCount
        Nothing -> do
            putStrLn "Некорректный ввод. Пожалуйста, введите положительное целое число."
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
        \    WHERE Rooms.Room_Number NOT IN ( \
        \    SELECT Bookings.Room_Number \
        \    FROM Bookings \
        \    WHERE (Bookings.Check_In_Date BETWEEN ? AND ?) \
        \    OR (Bookings.Check_Out_Date BETWEEN ? AND ?) \
        \    OR (Bookings.Check_In_Date <= ? AND Bookings.Check_Out_Date >= ?) \
        \)) \
        \SELECT DISTINCT Room_Types.Name_Type, Room_Types.Price_Per_Night \
        \FROM Rooms \
        \JOIN Room_Types ON Rooms.Room_Type_ID = Room_Types.Room_Type_ID \
        \JOIN AvailableRoomNumbers ON Rooms.Room_Number = AvailableRoomNumbers.Room_Number \
        \WHERE Room_Types.Room_Capacity >= ?"
        (checkInDate, checkOutDate, checkInDate, checkOutDate, checkInDate, checkOutDate, minGuestCount)
    return rows


bookRoom :: Connection -> String -> Day -> Day -> Int -> [Int] -> String -> IO ()
bookRoom conn roomType checkInDate checkOutDate guestCount clientIDs paymentMethod = do
    rows <- query conn
        "WITH AvailableRoom AS ( \
        \    SELECT Rooms.Room_Number FROM Rooms \
        \    JOIN Room_Types ON Rooms.Room_Type_ID = Room_Types.Room_Type_ID \
        \    LEFT JOIN Bookings ON Rooms.Room_Number = Bookings.Room_Number \
        \    WHERE Room_Types.Name_Type = ? \
        \    AND NOT EXISTS ( \
        \        SELECT 1 FROM Bookings \
        \        WHERE Rooms.Room_Number = Bookings.Room_Number \
        \        AND (Bookings.Check_In_Date BETWEEN ? AND ? OR Bookings.Check_Out_Date BETWEEN ? AND ?) \
        \    ) \
        \    LIMIT 1 \
        \) \
        \SELECT Rooms.Room_Number, Room_Types.Price_Per_Night \
        \FROM Rooms \
        \JOIN Room_Types ON Rooms.Room_Type_ID = Room_Types.Room_Type_ID \
        \JOIN AvailableRoom ON Rooms.Room_Number = AvailableRoom.Room_Number"
        (roomType, checkInDate, checkOutDate, checkInDate, checkOutDate) :: IO [(Int, Float)]
    case rows of
        ((roomNumber, pricePerNight):_) -> do
            let bookingInfo = BookingInfo {
                roomNumber = roomNumber,
                checkInDate = checkInDate,
                checkOutDate = checkOutDate,
                guestCount = guestCount,
                paymentMethod = paymentMethod,
                paymentStatus = paymentMethod == "Онлайн"
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

getPaymentMethod :: IO String
getPaymentMethod = do
    putStrLn "Выберите метод оплаты:\n1. Онлайн\n2. Наличные"
    method <- getLine
    case method of
        "1" -> return "Онлайн"
        "2" -> return "Наличными"
        _ -> do
            putStrLn "Некорректный выбор."
            getPaymentMethod

getRoomTypeIndex :: String -> Int -> IO Int
getRoomTypeIndex prompt maxIndex = do
    putStrLn prompt
    input <- getLine
    case reads input :: [(Int, String)] of
        [(roomTypeIndex, "")] ->
            if roomTypeIndex >= 1 && roomTypeIndex <= maxIndex
                then return roomTypeIndex
                else do
                    putStrLn "Неверный индекс. Пожалуйста, выберите корректный индекс."
                    getRoomTypeIndex prompt maxIndex
        _ -> do
            putStrLn "Неверный ввод. Пожалуйста, введите целое число."
            getRoomTypeIndex prompt maxIndex


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
    checkOutDate <- getCheckOutDate checkInDate

    guestCount <- getGuestCount

    availableRoomTypes <- getAvailableRoomTypes conn checkInDate checkOutDate guestCount
    if null availableRoomTypes
        then putStrLn "Для указанных вами дат и количества гостей, доступных номеров нет.\nИзмените даты, а также проверьте доступные типы номеров."
        else do
            putStrLn "Выберите один из доступных типов номеров:"
            forM_ (zip [1..] availableRoomTypes) $ \(i, (roomType, pricePerNight)) ->
                putStrLn $ show i ++ ". Тип: " ++ roomType ++ ", Стоимость за ночь: " ++ show pricePerNight ++ " руб."

            roomTypeIndex <- getRoomTypeIndex "Введите номер выбранного типа номера:"  (length availableRoomTypes)
            let (selectedRoomType, _) = availableRoomTypes !! (roomTypeIndex - 1)
            putStrLn $ "Вы выбрали тип номера: " ++ selectedRoomType
            
            -- roomTypeIndex <- getRoomTypeIndex "Введите номер выбранного типа номера:"
            -- let (selectedRoomType, _) = availableRoomTypes !! (roomTypeIndex - 1)

            clientIDs <- addClientsAndPassports guestCount
            paymentMethod <- getPaymentMethod

            bookRoom conn selectedRoomType checkInDate checkOutDate guestCount clientIDs paymentMethod

    close conn
