{-# LANGUAGE OverloadedStrings #-}

module Client
    ( addClientsAndPassports
    ) where

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
    putStrLn "Введите дату рождения (ГГГГ-ММ-ДД): "
    dobStr <- getLine
    let dateOfBirth = parseDate dobStr
    return (Client name middleName surname phoneNumber email dateOfBirth Nothing)

getPassportInfo :: IO Passport
getPassportInfo = do
    putStrLn "Введите номер паспорта: "
    numberStr <- getLine
    let number = read numberStr :: Int
    putStrLn "Введите серию паспорта: "
    serialStr <- getLine
    let serial = read serialStr :: Int
    putStrLn "Введите дату выдачи паспорта (ГГГГ-ММ-ДД): "
    doiStr <- getLine
    let dateOfIssue = parseDate doiStr
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

addClientsAndPassports :: IO ()
addClientsAndPassports = do
    conn <- open "hotel.db"
    client <- getPersonInfo
    clientId <- addClient conn client
    putStrLn "Введите информацию о паспорте:"
    passport <- getPassportInfo
    passportId <- addPassport conn clientId (passport { clientId = clientId })
    updateClientPassportId conn clientId passportId
    close conn

