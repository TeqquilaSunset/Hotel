{-# LANGUAGE OverloadedStrings #-}

module Client
    ( addClientsAndPassports
    ) where

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Control.Exception (catch, IOException)
import Control.Monad (replicateM_)
import Text.Read (readMaybe)

data Client = Client { clientId :: Maybe Int
                     , passportId :: Maybe Int
                     , name :: String
                     , middleName :: String
                     , surname :: String
                     , phoneNumber :: String
                     , email :: String
                     , dateOfBirth :: String
                     } deriving (Show)

data Passport = Passport { passportId' :: Maybe Int
                         , clientId' :: Maybe Int
                         , number :: Int
                         , serial :: Int
                         , dateOfIssue :: String
                         , issued :: String
                         } deriving (Show)

instance FromRow Client where
    fromRow = Client <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

instance FromRow Passport where
    fromRow = Passport <$> field <*> field <*> field <*> field <*> field <*> field

-- Safe number input
getNumber :: String -> IO Int
getNumber prompt = do
    putStrLn prompt
    input <- getLine
    case readMaybe input of
        Just n  -> return n
        Nothing -> do
            putStrLn "Некорректный ввод. Пожалуйста, введите число."
            getNumber prompt

-- Main functionality
addClientsAndPassports :: IO ()
addClientsAndPassports = catch (do
    conn <- open "hotel.db"
    execute_ conn "CREATE TABLE IF NOT EXISTS Clients (Client_ID INTEGER PRIMARY KEY AUTOINCREMENT, Passport_Id INTEGER, Name TEXT, Middle_Name TEXT, Surname TEXT, Phone_number TEXT, Email TEXT, Date_of_birth DATE, FOREIGN KEY (Passport_Id) REFERENCES Passports(Passport_Id))"
    execute_ conn "CREATE TABLE IF NOT EXISTS Passports (Passport_Id INTEGER PRIMARY KEY AUTOINCREMENT, Client_ID INTEGER, Number INTEGER, Serial INTEGER, Date_of_issue DATE, Issued TEXT, FOREIGN KEY (Client_ID) REFERENCES Clients(Client_ID))"
    n <- getNumber "Введите количество гостей:"
    replicateM_ n (insertClient conn)
    close conn) handler
    where
        handler :: IOException -> IO ()
        handler ex = putStrLn $ "Произошла ошибка: " ++ show ex

-- Insert Client
insertClient :: Connection -> IO ()
insertClient conn = do
    putStrLn "Введите имя:"
    name <- getLine
    putStrLn "Введите отчество:"
    middleName <- getLine
    putStrLn "Введите фамилию:"
    surname <- getLine
    number <- getNumber "Введите номер паспорта:"
    serial <- getNumber "Введите серию паспорта:"
    putStrLn "Введите дату выдачи паспорта (гггг-мм-дд):"
    dateOfIssue <- getLine
    putStrLn "Введите место выдачи паспорта:"
    issued <- getLine
    putStrLn "Введите номер телефона:"
    phoneNumber <- getLine
    putStrLn "Введите адрес электронной почты:"
    email <- getLine
    putStrLn "Введите дату рождения (гггг-мм-дд):"
    dateOfBirth <- getLine

    isUnique <- checkPassportUniqueness conn number serial
    if isUnique
        then do
            execute conn "INSERT INTO Passports (Number, Serial, Date_of_issue, Issued) VALUES (?, ?, ?, ?)"
                (number, serial, dateOfIssue, issued)
            passportId <- lastInsertRowId conn
            execute conn "INSERT INTO Clients (Passport_Id, Name, Middle_Name, Surname, Phone_number, Email, Date_of_birth) VALUES (?, ?, ?, ?, ?, ?, ?)"
                (passportId, name, middleName, surname, phoneNumber, email, dateOfBirth)
            putStrLn "Данные успешно добавлены в базу данных."
        else putStrLn "Паспорт с такой серией и номером уже существует."

-- Check Uniqueness
checkPassportUniqueness :: Connection -> Int -> Int -> IO Bool
checkPassportUniqueness conn number serial = do
    [Only count] <- query conn "SELECT COUNT(*) FROM Passports WHERE Number = ? AND Serial = ?" (number, serial)
    return (count == (0 :: Int))
