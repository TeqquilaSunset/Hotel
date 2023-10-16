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
    def __init__(self, passport_id, name, middle_name, surname, phone_number, email, date_of_birth):
        self.passport_id = passport_id
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


class Room:
    def __init__(self, number, room_type, status):
        self.number = number
        self.room_type = room_type
        self.status = status


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
    c.execute("INSERT INTO Clients (Passport_Id, Name, Middle_Name, Surname, Phone_number, Email, Date_of_birth) VALUES (?, ?, ?, ?, ?, ?, ?)",
              (object.passport_id, object.name, object.middle_name, object.surname, object.phone_number, object.email, object.date_of_birth))
    return c.lastrowid  # Возвращаем ID нового клиента


def insert_passport(object):
    c.execute("INSERT INTO Passports (Client_ID, Number, Serial, Date_of_issue, Issued) VALUES (?, ?, ?, ?, ?)",
              (object.client_id, object.number, object.serial, object.date_of_issue, object.issued))


def insert_room_type(object):
    c.execute("INSERT INTO Room_Types (Name_Type, Description, Price_Per_Night, Room_Capacity) VALUES (?, ?, ?, ?)",
              (object.name_type, object.description, object.price_per_night, object.room_capacity))
    return c.lastrowid


def insert_room(object):
    c.execute("INSERT INTO Rooms (Room_Number, Room_Type_ID, Status) VALUES (?, ?, ?)",
              (object.number, object.room_type, object.status))
    # return c.lastrowid


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
    Room_Type("Эконом одиночный",
              "Уютный одноместный номер с минимальным набором удобств.", 800, 1),
    Room_Type("Эконом обычный",
              "Доступный двухместный номер с основными удобствами.", 1200, 2),
    Room_Type("Комфорт", "Современный двухместный номер с дополнительными удобствами для комфортного проживания.", 1500, 2),
    Room_Type(
        "Комфорт+", "Просторный двухместный номер с расширенным спектром услуг и удобств.", 2000, 2),
    Room_Type(
        "Семейный", "Четырехместный номер, идеально подходящий для семейного отдыха с детьми.", 2100, 4),
    Room_Type("Номер для новобрачных",
              "Романтичный двухместный номер, специально оформленный для молодоженов.", 1700, 2),
    Room_Type("Люкс", "Роскошный двухместный номер с высококлассным обслуживанием и эксклюзивными удобствами.", 3000, 2),
    Room_Type("Королевский",
              "Просторный трехместный номер с дизайнерским оформлением и превосходным сервисом.", 4500, 3),
    Room_Type("Компанейский",
              "Большой номер для шестерых гостей, оптимальный выбор для группы друзей.", 4000, 6),
]

rooms = [
    Room(101, 1, "Доступен"),
    Room(102, 1, "Доступен"),
    Room(103, 1, "Доступен"),
    Room(104, 1, "Доступен"),
    Room(105, 1, "Доступен"),
    Room(106, 1, "Доступен"),
    Room(107, 1, "Доступен"),
    Room(108, 1, "Доступен"),
    Room(109, 1, "Доступен"),
    Room(110, 1, "Доступен"),
    Room(111, 2, "Доступен"),
    Room(112, 2, "Доступен"),
    Room(113, 2, "Доступен"),
    Room(114, 2, "Доступен"),
    Room(115, 2, "Доступен"),
    Room(116, 2, "Доступен"),
    Room(117, 2, "Доступен"),
    Room(118, 2, "Доступен"),
    Room(119, 2, "Доступен"),
    Room(120, 2, "Доступен"),
    Room(121, 3, "Доступен"),
    Room(122, 3, "Доступен"),
    Room(123, 3, "Доступен"),
    Room(124, 3, "Доступен"),
    Room(125, 3, "Доступен"),
    Room(201, 4, "Доступен"),
    Room(202, 4, "Доступен"),
    Room(203, 4, "Доступен"),
    Room(204, 4, "Доступен"),
    Room(205, 4, "Доступен"),
    Room(206, 5, "Доступен"),
    Room(207, 5, "Доступен"),
    Room(208, 5, "Доступен"),
    Room(209, 6, "Доступен"),
    Room(210, 7, "Доступен"),
    Room(211, 7, "Доступен"),
    Room(212, 7, "Доступен"),
    Room(213, 7, "Доступен"),
    Room(214, 8, "Доступен"),
    Room(215, 8, "Доступен"),
    Room(216, 9, "Доступен"),
]

# Name, Middle_Name, Surname, Phone_number, Email, Date_of_birth
clients = [
    Client("1", "Иван", "Иванович", "Иванов",
           "+79133423698", "ivan@mail.ru", "2000-01-01"),
    Client("2", "Петр", "Петрович", "Петров",
           "+79133423699", "petr@mail.ru", "1990-05-01"),
    Client("3", "Анна", "Анатольевна", "Смирнова",
           "+79133423700", "anna@mail.ru", "1995-07-03"),
    Client("4", "Ольга", "Олеговна", "Сидорова",
           "+79133423701", "olga@mail.ru", "1985-08-10"),
    Client("5", "Алексей", "Алексеевич", "Жуков",
           "+79133423702", "alex@mail.ru", "1992-06-20"),
    Client("6", "Елена", "Егоровна", "Кузнецова",
           "+79133423703", "elena@mail.ru", "1984-12-12"),
    Client("7", "Максим", "Максимович", "Белов",
           "+79133423704", "maxim@mail.ru", "1996-11-27"),
    Client("8", "Наталья", "Александровна", "Павлова",
           "+79133423705", "natalia@mail.ru", "1988-09-19"),
    Client("9", "Сергей", "Сергеевич", "Васильев",
           "+79133423706", "sergey@mail.ru", "1977-05-05"),
    Client("10", "Вера", "Васильевна", "Федорова",
           "+79133423707", "vera@mail.ru", "1993-10-10"),
    Client("11", "Дмитрий", "Олегович", "Алексеев",
           "+79133423708", "dmitriy@mail.ru", "1999-04-04"),
    Client("12", "Людмила", "Игоревна", "Григорьева",
           "+79133423709", "lyudmila@mail.ru", "1978-03-03"),
    Client("13", "Михаил", "Михайлович", "Лобанов",
           "+79133423710", "mikhail@mail.ru", "1986-02-02"),
    Client("14", "Артем", "Александрович", "Артемьев",
           "+79133423711", "artem@mail.ru", "1997-06-06"),
    Client("15", "Мария", "Дмитриевна", "Козлова",
           "+79133423712", "maria@mail.ru", "1991-07-07"),
    Client("16", "Татьяна", "Романовна", "Рыбакова",
           "+79133423713", "tatiana@mail.ru", "1983-08-08"),
    Client("17", "Кирилл", "Владимирович", "Прокофьев",
           "+79133423714", "kirill@mail.ru", "2002-12-12"),
    Client("18", "Оксана", "Павловна", "Щукина",
           "+79133423715", "oksana@mail.ru", "1994-11-11"),
    Client("19", "Григорий", "Леонидович", "Макаров",
           "+79133423716", "grigoriy@mail.ru", "1981-01-01"),
    Client("20", "Виктория", "Андреевна", "Соловьева",
           "+79133423717", "viktoria@mail.ru", "1989-05-05"),
    Client("21", "Екатерина", "Сергеевна", "Шарапова",
           "+79133423718", "ekaterina@mail.ru", "2000-06-15"),
    Client("22", "Игорь", "Анатольевич", "Мельников",
           "+79133423719", "igor@mail.ru", "1975-11-23"),
    Client("23", "Олеся", "Викторовна", "Абрамова",
           "+79133423720", "olesya@mail.ru", "1982-08-11"),
    Client("24", "Анатолий", "Владимирович", "Сергеев",
           "+79133423721", "anatoliy@mail.ru", "1993-02-27"),
    Client("25", "Тимур", "Русланович", "Фаизуллин",
           "+79133423722", "timur@mail.ru", "1998-09-09")
]

passports = [
    Passport("1", "021036", "6922", "2020-12-13",
             "УМВД России по Новосибирской области"),
    Passport("2", "021037", "6923", "2019-11-14",
             "УМВД России по Московской области"),
    Passport("3", "021038", "6924", "2018-10-15",
             "УМВД России по Санкт-Петербургу"),
    Passport("4", "021039", "6925", "2021-01-16",
             "УМВД России по Челябинской области"),
    Passport("5", "021040", "6926", "2022-02-17",
             "УМВД России по Республике Татарстан"),
    Passport("6", "021041", "6927", "2023-03-18",
             "УМВД России по Свердловской области"),
    Passport("7", "021042", "6928", "2017-04-19",
             "УМВД России по Республике Башкортостан"),
    Passport("8", "021043", "6929", "2016-05-20",
             "УМВД России по Ростовской области"),
    Passport("9", "021044", "6930", "2015-06-21",
             "УМВД России по Красноярскому краю"),
    Passport("10", "021045", "6931", "2014-07-22",
             "УМВД России по Воронежской области"),
    Passport("11", "021046", "6932", "2013-08-23",
             "УМВД России по Самарской области"),
    Passport("12", "021047", "6933", "2012-09-24",
             "УМВД России по Краснодарскому краю"),
    Passport("13", "021048", "6934", "2011-10-25",
             "УМВД России по Пермскому краю"),
    Passport("14", "021049", "6935", "2010-12-26",
             "УМВД России по Оренбургской области"),
    Passport("15", "021050", "6936", "2009-11-27",
             "УМВД России по Волгоградской области"),
    Passport("16", "021051", "6937", "2008-10-28",
             "УМВД России по Иркутской области"),
    Passport("17", "021052", "6938", "2007-09-29",
             "УМВД России по Нижегородской области"),
    Passport("18", "021053", "6939", "2006-08-30",
             "УМВД России по Саратовской области"),
    Passport("19", "021054", "6940", "2005-07-31",
             "УМВД России по Хабаровскому краю"),
    Passport("20", "021055", "6941", "2004-06-01",
             "УМВД России по Омской области"),
    Passport("21", "021056", "6942", "2001-07-22",
             "УМВД России по Хабаровского края"),
    Passport("22", "021057", "6943", "2003-06-30",
             "УМВД России по Белгородской области"),
    Passport("23", "021058", "6944", "1999-11-14",
             "УМВД России по Тульской области"),
    Passport("24", "021059", "6945", "2000-03-12",
             "УМВД России по Псковской области"),
    Passport("25", "021060", "6946", "2002-02-06",
             "УМВД России по Московской области"),
]

bookings = [
    Booking("101", "2023-10-01", "2023-10-07", "1", "Наличными", True),
    Booking("101", "2023-09-19", "2023-10-26", "1", "Наличными", True),
    Booking("110", "2023-09-22", "2023-09-29", "1", "Наличными", True),
    Booking("112", "2023-10-01", "2023-10-05", "1", "Наличными", True),
    Booking("111", "2023-10-05", "2023-10-10", "2", "Наличными", True),
    Booking("209", "2023-09-14", "2023-09-22", "4", "Наличными", True),
    Booking("121", "2023-09-01", "2023-09-24", "2", "Наличными", True),
    Booking("122", "2023-10-13", "2023-10-19", "2", "Наличными", True),
    Booking("121", "2023-10-04", "2023-10-06", "2", "Наличными", True),
    Booking("216", "2023-09-29", "2023-10-01", "5", "Наличными", True),

    Booking("120", "2023-11-01", "2023-11-02", "1", "Наличными", True),
    Booking("216", "2023-11-03", "2023-11-12", "5", "Наличными", True),
    Booking("121", "2023-11-01", "2023-11-09", "2", "Наличными", True),
    Booking("121", "2023-11-09", "2023-11-11", "2", "Наличными", True),
    Booking("209", "2023-10-28", "2023-11-03", "2", "Наличными", True),
]

clientsbookings = [
    ClientsBooking("1", "1"),
    ClientsBooking("2", "2"),
    ClientsBooking("3", "3"),
    ClientsBooking("4", "4"),
    ClientsBooking("5", "5"),
    ClientsBooking("5", "6"),
    ClientsBooking("6", "7"),
    ClientsBooking("6", "8"),
    ClientsBooking("6", "9"),
    ClientsBooking("6", "10"),
    ClientsBooking("7", "11"),
    ClientsBooking("7", "12"),
    ClientsBooking("8", "13"),
    ClientsBooking("8", "14"),
    ClientsBooking("9", "15"),
    ClientsBooking("9", "16"),
    ClientsBooking("10", "17"),
    ClientsBooking("10", "18"),
    ClientsBooking("10", "19"),
    ClientsBooking("10", "20"),
    ClientsBooking("10", "21"),

    ClientsBooking("11", "22"),
    ClientsBooking("12", "23"),
    ClientsBooking("12", "24"),
    ClientsBooking("12", "1"),
    ClientsBooking("12", "2"),
    ClientsBooking("12", "5"),
    ClientsBooking("13", "25"),
    ClientsBooking("13", "26"),
    ClientsBooking("14", "7"),
    ClientsBooking("14", "8"),
    ClientsBooking("15", "12"),
    ClientsBooking("15", "13"),
]


def main():
    # Создание записей
    # Типы комнат
    for room_type in room_types:
        insert_room_type(room_type)

    # Комнаты
    for room in rooms:
        insert_room(room)

    for client in clients:
        insert_client(client)

    for passport in passports:
        insert_passport(passport)

    for booking in bookings:
        insert_booking(booking)

    for clientbooking in clientsbookings:
        insert_clientsbooking(clientbooking)
    # Сохранение и закрытие соединения
    conn.commit()
    conn.close()
    print("Записи созданны.")
