import sqlite3
from datetime import datetime, timedelta

# Подключаемся к базе данных
conn = sqlite3.connect('hotel.db')
c = conn.cursor()

# Классы
class Room_Type:
    def __init__(self, name_type, description, price_per_night, room_capacity):
        self.name_type = name_type
        self.description = description
        self.price_per_night = price_per_night
        self.room_capacity = room_capacity

class Client:
    def __init__(self, name, middle_name, surname, phone_number, email, date_of_birth):
        self.name = name
        self.middle_name = middle_name
        self.surname = surname
        self.phone_number = phone_number
        self.email = email
        self.date_of_birth = date_of_birth

class Passport:
    def __init__(self, client_id, number, serial, date_of_issue, issued):
        self.client_id = client_id
        self.number = number
        self.serial = serial
        self.date_of_issue = date_of_issue
        self.issued = issued

class RoomType:
    def __init__(self, name_type, description, price_per_night, room_capacity):
        self.name_type = name_type
        self.description = description
        self.price_per_night = price_per_night
        self.room_capacity = room_capacity

class Booking:
    def __init__(self, room_number, check_in_date, check_out_date, guest_count, payment_method, payment_status):
        self.room_number = room_number
        self.check_in_date = check_in_date
        self.check_out_date = check_out_date
        self.guest_count = guest_count
        self.payment_method = payment_method
        self.payment_status = payment_status

class ClientsBooking:
    def __init__(self, client_id, booking_id):
        self.client_id = client_id
        self.booking_id = booking_id


# Вспомогательные функции
def insert_client(object):
    c.execute("INSERT INTO Clients (Name, Middle_Name, Surname, Phone_number, Email, Date_of_birth) VALUES (?, ?, ?, ?, ?, ?)",
              (object.name, object.middle_name, object.surname, object.phone_number, object.email, object.date_of_birth))
    return c.lastrowid  # Возвращаем ID нового клиента

def insert_passport(object):
    c.execute("INSERT INTO Passports (Client_ID, Number, Serial, Date_of_issue, Issued) VALUES (?, ?, ?, ?, ?)",
              (object.client_id, object.number, object.serial, object.date_of_issue, object.issued))

def insert_room_type(object):
    c.execute("INSERT INTO Room_Types (Name_Type, Description, Price_Per_Night, Room_Capacity) VALUES (?, ?, ?, ?)",
              (object.name_type, object.description, object.price_per_night, object.room_capacity))
    return c.lastrowid

def insert_room_type(object):
    c.execute("INSERT INTO Room_Types (Name_Type, Description, Price_Per_Night, Room_Capacity) VALUES (?, ?, ?, ?)",
              (object.name_type, object.description, object.price_per_night, object.room_capacity))
    return c.lastrowid

def insert_booking(object):
    c.execute("INSERT INTO Bookings (Room_Number, Check_In_Date, Check_Out_Date, Guest_Count, Payment_Method, Payment_Status) VALUES (?, ?, ?, ?, ?, ?)",
              (object.room_number, object.check_in_date, object.check_out_date, object.guest_count, object.payment_method, object.payment_status))
    return c.lastrowid

def insert_clientsbooking(object):
    c.execute("INSERT INTO ClientsBookings (Client_ID, Booking_ID) VALUES (?, ?)",
              (object.client_id, object.booking_id))

# Добавление данных


# Создание экземпляров класса Room_Type
room_types = [
    Room_Type("Эконом одиночный", "Уютный одноместный номер с минимальным набором удобств.", 800, 1),
    Room_Type("Эконом обычный", "Доступный двухместный номер с основными удобствами.", 1200, 2),
    Room_Type("Комфорт", "Современный двухместный номер с дополнительными удобствами для комфортного проживания.", 1500, 2),
    Room_Type("Комфорт+", "Просторный двухместный номер с расширенным спектром услуг и удобств.", 2000, 2),
    Room_Type("Семейный", "Четырехместный номер, идеально подходящий для семейного отдыха с детьми.", 1900, 4),
    Room_Type("Семейный+", "Улучшенный семейный номер с дополнительными удобствами для родителей и детей.", 2100, 4),
    Room_Type("Номер для новобрачных", "Романтичный двухместный номер, специально оформленный для молодоженов.", 1700, 2),
    Room_Type("Люкс", "Роскошный двухместный номер с высококлассным обслуживанием и эксклюзивными удобствами.", 3000, 2),
    Room_Type("Королевский", "Просторный трехместный номер с дизайнерским оформлением и превосходным сервисом.", 4500, 3),
    Room_Type("Компанейский", "Большой номер для шестерых гостей, оптимальный выбор для группы друзей или командировочных.", 4000, 6),
]







#client_id = insert_client("John", "M.", "Doe", "+123456789", "john@example.com", "1990-01-01")
#insert_passport(client_id, 123456, 1234, "2010-01-01", "Department")

#room_type_id = insert_room_type("Standard", "A standard room", 100, 2)
# for room_type in room_types:
#     insert_room_type(room_type)


#insert_room(101, room_type_id, "Available")

#booking_id = insert_booking(101, str(datetime.now().date()), str((datetime.now() + timedelta(days=5)).date()), 1, "Credit Card", True)
#insert_clientsbooking(client_id, booking_id)

# Сохраняем изменения и закрываем соединение


def main():
    #Создание записей
    for room_type in room_types:
        insert_room_type(room_type)

    # Сохранение и закрытие соединения
    conn.commit()
    conn.close()
    print("Записи созданны.")
