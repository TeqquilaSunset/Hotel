import sqlite3
import createEntrie

sql_script = """
-- Включение поддержки внешних ключей
PRAGMA foreign_keys = ON;

-- Удаление индексов (если они существуют)
DROP INDEX IF EXISTS Relationship_4_FK;
DROP INDEX IF EXISTS Relationship_2_FK;
DROP INDEX IF EXISTS Relationship_3_FK;

-- Удаление таблиц (если они существуют)
DROP TABLE IF EXISTS ClientsBookings;
DROP TABLE IF EXISTS Bookings;
DROP TABLE IF EXISTS Clients;
DROP TABLE IF EXISTS Passports;
DROP TABLE IF EXISTS Room_Types;
DROP TABLE IF EXISTS Rooms;

-- Создание таблиц
CREATE TABLE Bookings (
    Booking_ID INTEGER PRIMARY KEY AUTOINCREMENT,
    Room_Number INTEGER,
    Check_In_Date DATE,
    Check_Out_Date DATE,
    Guest_Count INTEGER,
    Payment_Method TEXT,
    Payment_Status BOOLEAN
);

CREATE TABLE Clients (
    Client_ID INTEGER PRIMARY KEY AUTOINCREMENT,
    Passport_Id INTEGER,
    Name TEXT,
    Middle_Name TEXT,
    Surname TEXT,
    Phone_number TEXT,
    Email TEXT,
    Date_of_birth DATE,
    FOREIGN KEY (Passport_Id) REFERENCES Passports(Passport_Id)
);

CREATE TABLE Passports (
    Passport_Id INTEGER PRIMARY KEY AUTOINCREMENT,
    Client_ID INTEGER,
    Number INTEGER,
    Serial INTEGER,
    Date_of_issue DATE,
    Issued TEXT,
    FOREIGN KEY (Client_ID) REFERENCES Clients(Client_ID)
);

CREATE TABLE Room_Types (
    Room_Type_ID INTEGER PRIMARY KEY AUTOINCREMENT,
    Name_Type TEXT,
    Description TEXT,
    Price_Per_Night REAL,
    Room_Capacity INTEGER
);

CREATE TABLE Rooms (
    Room_Number INTEGER PRIMARY KEY,
    Room_Type_ID INTEGER,
    Status TEXT,
    FOREIGN KEY (Room_Type_ID) REFERENCES Room_Types(Room_Type_ID)
);

CREATE TABLE ClientsBookings (
    Client_ID INTEGER,
    Booking_ID INTEGER,
    PRIMARY KEY (Client_ID, Booking_ID),
    FOREIGN KEY (Client_ID) REFERENCES Clients(Client_ID),
    FOREIGN KEY (Booking_ID) REFERENCES Bookings(Booking_ID)
);

-- Создание индексов
CREATE INDEX Relationship_4_FK ON Bookings (Room_Number);
CREATE INDEX Relationship_2_FK ON Clients (Passport_Id);
CREATE INDEX Relationship_3_FK ON Passports (Client_ID);
"""

# Создание соединения с базой данных (или ее создание, если она не существует)
conn = sqlite3.connect('hotel.db')

# Выполнение скрипта для создания структуры БД
try:
    cursor = conn.cursor()
    cursor.executescript(sql_script)
    conn.commit()
    createEntrie.main()
    print("База данных успешно создана!")
except sqlite3.Error as e:
    print("Ошибка при работе с базой данных:", e)
finally:
    if conn:
        conn.close()
