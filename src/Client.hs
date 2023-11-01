{-# LANGUAGE OverloadedStrings #-}

module Client
    ( addClientsAndPassports
    ) where

import Text.Read (readMaybe)

import Database.SQLite.Simple
import Data.Time
import Data.Time.Format

data Client = Client {
    name :: String,
    middleName :: String,
    surname :: String,
    phoneNumber :: String,
    email :: String,
    dateOfBirth :: Day,
    passportId :: Maybe Int
}

data Passport = Passport {
    clientId :: Int,
    number :: Int,
    serial :: Int,
    dateOfIssue :: Day,
    issued :: String
}

parseDate :: String -> Day
parseDate = parseTimeOrError True defaultTimeLocale "%Y-%m-%d"

getDate :: String -> IO Day
getDate prompt = do
    putStrLn prompt
    dateString <- getLine
    case parseTimeM True defaultTimeLocale "%Y-%m-%d" dateString :: Maybe Day of
        Just date -> return date
        Nothing -> do
            putStrLn "\nНекорректный ввод. Пожалуйста, попробуйте еще раз."
            getDate prompt

getPersonInfo :: IO Client
getPersonInfo = do
    putStrLn "Введите имя: "
    name <- getLine
    putStrLn "Введите отчество: "
    middleName <- getLine
    putStrLn "Введите фамилию: "
    surname <- getLine
    putStrLn "Введите номер телефона: "
    phoneNumber <- getLine
    putStrLn "Введите электронную почту: "
    email <- getLine 
    -- putStrLn "Введите дату рождения (ГГГГ-ММ-ДД): "
    dateOfBirth <- getDate "Введите дату рождения (ГГГГ-ММ-ДД): "
    return (Client name middleName surname phoneNumber email dateOfBirth Nothing)

-- Функция для ввода и проверки номера паспорта
getPassportNumber :: IO Int
getPassportNumber = do
    putStrLn "Введите номер паспорта (6-значное целое число): "
    input <- getLine
    let maybeNumber = readMaybe input :: Maybe Int
    case maybeNumber of
        Just number | length input == 6 -> return number
        _ -> do
            putStrLn "Номер паспорта должен быть 6-значным целым числом."
            getPassportNumber

-- Функция для ввода и проверки серии паспорта
getPassportSerial :: IO Int
getPassportSerial = do
    putStrLn "Введите серию паспорта (4-значное целое число): "
    input <- getLine
    let maybeSerial = readMaybe input :: Maybe Int
    case maybeSerial of
        Just serial | length input == 4 -> return serial
        _ -> do
            putStrLn "Серия паспорта должна быть 4-значным целым числом."
            getPassportSerial

-- Основная функция для получения паспортной информации
getPassportInfo :: IO Passport
getPassportInfo = do
    number <- getPassportNumber
    serial <- getPassportSerial
    dateOfIssue <- getDate "Введите дату выдачи паспорта (ГГГГ-ММ-ДД): "
    putStrLn "Введите, кем выдан паспорт: "
    issued <- getLine
    return (Passport 0 number serial dateOfIssue issued)

addClient :: Connection -> Client -> IO Int
addClient conn client = do
    execute conn 
        "INSERT INTO Clients (Name, Middle_Name, Surname, Phone_number, Email, Date_of_birth, Passport_Id) VALUES (?,?,?,?,?,?,NULL)"
        (name client, middleName client, surname client, phoneNumber client, email client, dateOfBirth client)
    rowId <- lastInsertRowId conn
    return (fromIntegral rowId :: Int)

addPassport :: Connection -> Int -> Passport -> IO Int
addPassport conn clientId passport = do
    execute conn 
        "INSERT INTO Passports (Client_ID, Number, Serial, Date_of_issue, Issued) VALUES (?,?,?,?,?)"
        (clientId, number passport, serial passport, dateOfIssue passport, issued passport)
    rowId <- lastInsertRowId conn
    return (fromIntegral rowId :: Int)

updateClientPassportId :: Connection -> Int -> Int -> IO ()
updateClientPassportId conn clientId passportId =
    execute conn
        "UPDATE Clients SET Passport_Id = ? WHERE Client_ID = ?"
        (passportId, clientId)

addClientsAndPassports :: Int -> IO [Int]
addClientsAndPassports n = do
    conn <- open "hotel.db"
    clientIds <- mapM (\_ -> do
        client <- getPersonInfo
        clientId <- addClient conn client
        putStrLn "Введите информацию о паспорте:"
        passport <- getPassportInfo
        passportId <- addPassport conn clientId (passport { clientId = clientId })
        updateClientPassportId conn clientId passportId
        return clientId) [1..n]
    close conn
    return clientIds

