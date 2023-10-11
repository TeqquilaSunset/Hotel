{-# LANGUAGE OverloadedStrings #-}

module RoomType
    ( getType
    ) where

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Text.Printf (printf)

data RoomType = RoomType {
    roomTypeId        :: Int,
    nameType          :: String,
    description       :: String,
    pricePerNight     :: Double,
    roomCapacity      :: Int
} deriving (Show)

instance FromRow RoomType where
  fromRow = RoomType <$> field <*> field <*> field <*> field <*> field

getRoomTypes :: Connection -> IO [RoomType]
getRoomTypes conn = 
    query_ conn "SELECT * FROM Room_Types;"

-- Функция для вывода одного RoomType в виде строки таблицы
printRoomType :: RoomType -> IO ()
printRoomType rt = 
    printf "%-25s %-90s %-10.2f %-13d\n" 
           (nameType rt) 
           (description rt) 
           (pricePerNight rt) 
           (roomCapacity rt)

getType :: IO ()
getType = do
    conn <- open "hotel.db"
    roomTypes <- getRoomTypes conn
    -- Печать заголовков таблицы

    printf "%-25s %-90s %-10s %-13s\n" ("Название" :: String) ("Описание" :: String) ("Цена" :: String) ("Вместимость" :: String)
    putStrLn $ replicate 140 '-'
    -- Печать каждого RoomType в виде строки таблицы
    mapM_ printRoomType roomTypes
    close conn
