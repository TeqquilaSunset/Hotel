import sqlite3
from datetime import datetime, timedelta

# Подключаемся к базе данных
conn = sqlite3.connect('hotel.db')
c = conn.cursor()

# Вспомогательные функции
def insert_client(name, middle_name, surname, phone_number, email, date_of_birth):
    c.execute("INSERT INTO Clients (Name, Middle_Name, Surname, Phone_number, Email, Date_of_birth) VALUES (?, ?, ?, ?, ?, ?)",
              (name, middle_name, surname, phone_number, email, date_of_birth))
    return c.lastrowid  # Возвращаем ID нового клиента

def insert_passport(client_id, number, serial, date_of_issue, issued):
    c.execute("INSERT INTO Passports (Client_ID, Number, Serial, Date_of_issue, Issued) VALUES (?, ?, ?, ?, ?)",
              (client_id, number, serial, date_of_issue, issued))

def insert_room_type(name_type, description, price_per_night, room_capacity):
    c.execute("INSERT INTO Room_Types (Name_Type, Description, Price_Per_Night, Room_Capacity) VALUES (?, ?, ?, ?)",
              (name_type, description, price_per_night, room_capacity))
    return c.lastrowid

def insert_room(room_number, room_type_id, status):
    c.execute("INSERT INTO Rooms (Room_Number, Room_Type_ID, Status) VALUES (?, ?, ?)",
              (room_number, room_type_id, status))

def insert_booking(room_number, check_in_date, check_out_date, guest_count, payment_method, payment_status):
    c.execute("INSERT INTO Bookings (Room_Number, Check_In_Date, Check_Out_Date, Guest_Count, Payment_Method, Payment_Status) VALUES (?, ?, ?, ?, ?, ?)",
              (room_number, check_in_date, check_out_date, guest_count, payment_method, payment_status))
    return c.lastrowid

def insert_clientsbooking(client_id, booking_id):
    c.execute("INSERT INTO ClientsBookings (Client_ID, Booking_ID) VALUES (?, ?)",
              (client_id, booking_id))

# Добавление данных
client_id = insert_client("John", "M.", "Doe", "+123456789", "john@example.com", "1990-01-01")
insert_passport(client_id, 123456, 1234, "2010-01-01", "Department")

room_type_id = insert_room_type("Standard", "A standard room", 100, 2)
insert_room(101, room_type_id, "Available")

booking_id = insert_booking(101, str(datetime.now().date()), str((datetime.now() + timedelta(days=5)).date()), 1, "Credit Card", True)
insert_clientsbooking(client_id, booking_id)

# Сохраняем изменения и закрываем соединение
conn.commit()
conn.close()
