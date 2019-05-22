-- Выведите столицу Малайзии (Malaysia) (в выводе: только название города).
-- (0,5 баллов)
SELECT City.Name FROM City
JOIN Country ON Country.Code = City.CountryCode
JOIN Capital ON Capital.CountryCode = Country.Code
WHERE Country.Name = 'Malaysia' AND City.Id = Capital.CityId;
