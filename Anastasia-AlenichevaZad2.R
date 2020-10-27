#Аленичева Анастасия, ПАЭ 123, Вариант 1
#Задание 2. Cоздайте модель множественной линейной регрессии ночных потоков углекислого газа за  осенний период 2013 года по данным измерений методом турбулентной пульсации.

#Указание и проверка рабочей директории
setwd ("D:/R/222")
getwd()

#Работа с библиотеками и установкой пакетов
install.packages("tidyverse")
library(tidyverse)

install.packages("nycflights13")
library("nycflights13")

install.packages("tidyr")
library("tidyr")

install.packages("stringr")
library("stringr")

install.packages("dplyr")
library("dplyr")

install.packages("tibble")
library("tibble")

install.packages("readr")
library("readr")

install.packages("rnoaa")
library(rnoaa)

install.packages("lubridate")
library(lubridate)

install.packages("ggplot2")
library("ggplot2")

#Чтаем данные из файла, пропускаем первую строку, заменяем текстовые 'NA', пустые и сгенерированные пороговые значения на NA, игнорируем строки с "[" 
eddypro = read_csv("eddypro.csv", skip = 1, na=c("","NA","-9999","-9999.0"), comment=c("["))

#Удаление первой строки и ненужного пустого столбца "roll"
eddypro = eddypro[-1, ];eddypro = select(eddypro, -(roll))

#Преобразование переменных типа char в факторы
eddypro = eddypro %>% mutate_if(is.character, factor)

#Изменение специальных символов в названии стобцов на допустимые для переменных названия
names(eddypro) = names(eddypro) %>% 
  str_replace_all("[!]", "_emph_") %>% 
  str_replace_all("[?]", "_quest_") %>% 
  str_replace_all("[*]", "_star_") %>% 
  str_replace_all("[+]", "_plus_") %>%
  str_replace_all("[-]", "_minus_") %>%
  str_replace_all("[@]", "_at_") %>%
  str_replace_all("[$]", "_dollar_") %>%
  str_replace_all("[#]", "_hash_") %>%
  str_replace_all("[/]", "_div_") %>%
  str_replace_all("[%]", "_perc_") %>%
  str_replace_all("[&]", "_amp_") %>%
  str_replace_all("[\\^]", "_power_") %>%
  str_replace_all("[()]", "_")

#Проверим столбцы в виде векторов
glimpse(eddypro)

#Уберем na, так как данные значения будут только мешать работе
eddypro = drop_na(eddypro)

#Осенний период, ночное время
eddypro = filter(eddypro, DOY >= 243 & DOY < 334, daytime==FALSE)

#Получение таблицы из интересующими нас колонок
eddypro_numeric = eddypro[,sapply(eddypro,is.numeric) ]
eddypro_non_numeric = eddypro[,!sapply(eddypro,is.numeric) ]

#Создадание непересекающиеся выборки
row_numbers = 1:length(eddypro_numeric$co2_flux)

#Обучающая
teach = sample(row_numbers, floor(length(eddypro_numeric$co2_flux)*.7))

#Тестирующая
test = row_numbers[-teach]

#Обучающая 
teaching_tbl = eddypro_numeric[teach,]

#Тестирующая
testing_tbl = eddypro_numeric[test,]

#Модель 1 
mod1 = lm(co2_flux~ (.) , data = teaching_tbl)
#Коэффициенты
coef(mod1)

#остатки
resid(mod1)

#Доверительный интервал
confint(mod1)

#P-значения по модели
summary(mod1)

#Дисперсионный анализ
anova(mod1)

#Графическое представление модели:
plot(mod1)

#Модель 2 
mod2 = lm ( co2_flux~ DOY  + Tau + qc_Tau + rand_err_Tau + H + rand_err_H + LE + qc_LE + rand_err_LE + qc_co2_flux + rand_err_co2_flux
            + h2o_flux + rand_err_h2o_flux + H_strg + co2_v_minus_adv  + h2o_v_minus_adv + co2_molar_density + co2_mole_fraction + co2_mixing_ratio + h2o_molar_density
            + h2o_mole_fraction + h2o_mixing_ratio + air_temperature + air_pressure + air_density + air_heat_capacity + e 
            + specific_humidity  + VPD + Tdew + u_unrot + w_unrot + u_rot + v_rot  + w_rot + max_speed + yaw + pitch + u_star_ + TKE + `_z_minus_d__div_L` 
            + T_star_ + x_peak + x_offset +  x_50_perc_ + un_Tau  + Tau_scf + un_H + H_scf + LE_scf + un_co2_flux  + h2o_spikes + co2_1  + co2_signal_strength_7200, data = teaching_tbl)

#Коэффициенты
coef(mod2)

#Остатки
resid(mod2)

#Доверительный интервал
confint(mod2)

#P-значения по модели
summary(mod2)

#Дисперсионный анализ
anova(mod2)

#Сравниваем 1 и 2 модель
anova(mod2, mod1)

#Графическое представление модели:
plot(mod2) 

#Модель 3 
mod3 = lm ( co2_flux~ DOY + qc_Tau + rand_err_Tau + H + rand_err_H + qc_LE + rand_err_LE + qc_co2_flux + rand_err_co2_flux
            + h2o_flux + rand_err_h2o_flux + co2_mole_fraction  + h2o_mole_fraction + air_heat_capacity + e
            + u_unrot  + u_rot  + pitch  + `_z_minus_d__div_L` + T_star_ + un_H + H_scf  + un_co2_flux, data = teaching_tbl)

#Коэффициенты
coef(mod3)

#Остатки
resid(mod3)

#Доверительный интервал
confint(mod3)

#P-значения по модели
summary(mod3)

#Дисперсионный анализ
anova(mod3)
anova(mod3, mod2)

#Графическое представление модели:
plot(mod3)

#Избавление от всех строк, где есть хотя бы одно значение NA, при помощи функции drop_na, и преобразование в таблицу
cor_teaching_tbl = select(teaching_tbl ,co2_flux , DOY , qc_Tau , rand_err_Tau , H , rand_err_H , qc_LE , rand_err_LE , qc_co2_flux , rand_err_co2_flux, h2o_flux , rand_err_h2o_flux
                          , co2_mole_fraction  , h2o_mole_fraction , air_heat_capacity , e, u_unrot  , u_rot  , pitch  , `_z_minus_d__div_L` , T_star_ , un_H , H_scf  , un_co2_flux)
#Таблица коэф. корреляции 
cor_td = cor(cor_teaching_tbl) %>% as.data.frame

##Проверка моделей
#Изпользуя модель 3, наложим её предсказанные значения 

#Первый график
qplot(co2_flux , co2_flux, data = teaching_tbl) + geom_line(aes(y = predict(mod3, teaching_tbl)))

#Второй график
qplot(co2_flux , co2_flux, data = testing_tbl) + geom_line(aes(y = predict(mod3, testing_tbl)))

#Модель зависит от множества переменных, мы можем вывести много графиков зависимостей co2_flux от учитываемых в моделе параметров
#В идеале предсказанная линия должна пройти через все точки, или как можно ближе к ним на ТЕСТИРУЮЩЕЙ выборке

#Примеры
qplot(DOY, co2_flux, data = testing_tbl) + geom_line(aes(y = predict(mod3, testing_tbl)))
qplot(Tau, co2_flux, data = testing_tbl) + geom_line(aes(y = predict(mod3, testing_tbl)))
qplot(h2o_flux, co2_flux, data = testing_tbl) + geom_line(aes(y = predict(mod3, testing_tbl)))
