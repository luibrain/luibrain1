# загрузка пакетов ----
library('dplyr')
library('data.table')          # работаем с объектами "таблица данных"
library('moments')             # коэффициенты асимметрии и эксцесса 
library('lattice')
library('ggplot2')

# загружаем файл с данными по импорту масла в РФ (из прошлой практики) ----
fileURL <- 'https://raw.githubusercontent.com/aksyuk/R-data/master/COMTRADE/040510-Imp-RF-comtrade.csv'
# создаём директорию для данных, если она ещё не существует:
if (!file.exists('./data')) {
  dir.create('./data')
}
# создаём файл с логом загрузок, если он ещё не существует:
if (!file.exists('./data/download.log')) {
  file.create('./data/download.log')
}
# загружаем файл, если он ещё не существует,
#  и делаем запись о загрузке в лог:
if (!file.exists('./data/040510-Imp-RF-comtrade.csv')) {
  download.file(fileURL, './data/040510-Imp-RF-comtrade.csv')
  # сделать запись в лог
  write(paste('Файл "040510-Imp-RF-comtrade.csv" загружен', Sys.time()), 
        file = './data/download.log', append = T)
}
# читаем данные из загруженного .csv во фрейм, если он ещё не существует
if (!exists('DT')){
  DT.import <- data.table(read.csv('./data/040510-Imp-RF-comtrade.csv', 
                                   stringsAsFactors = F))
}
# предварительный просмотр ----
dim(DT.import)            # размерность таблицы
str(DT.import)            # структура (характеристики столбцов)
DT.import          # удобный просмотр объекта data.table


unique(DT.import$Reporter)
unique(DT.import$Period.Desc)
# фильтр ----
DT.import <- data.table(filter(DT.import, startsWith(Period.Desc, "January ") | startsWith(Period.Desc, "February ")
                                |startsWith(Period.Desc, "March ") |startsWith(Period.Desc, "April ")
                                |startsWith(Period.Desc, "May ") |startsWith(Period.Desc, "June ")
                                |startsWith(Period.Desc, "July ") |startsWith(Period.Desc, "August ")))


# сколько NA в каждом из оставшихся столбцов? ----
na.num <- apply(DT.import, 2, function(x) length(which(is.na(x))))
# выводим только положительные и по убыванию
sort(na.num[na.num > 0], decreasing = T)
# явное преобразование типа, чтобы избежать проблем 
#  при заполнении пропусков
DT.import[, Netweight.kg := as.double(Netweight.kg)]
# считаем медианы и округляем до целого, как исходные данные
DT.import[, round(median(.SD$Netweight.kg, na.rm = T), 0),
          by = Year]
# сначала копируем все значения
DT.import[, Netweight.kg.median := round(median(.SD$Netweight.kg,
                                                na.rm = T), 0),
          by = Year]
# затем заменяем пропуски на медианы
DT.import[!is.na(Netweight.kg), Netweight.kg.median := Netweight.kg]
# смотрим результат
DT.import[, Netweight.kg, Netweight.kg.median]
DT.import[is.na(Netweight.kg), Year, Netweight.kg.median]

# Формирование групп стран ----

#США
USA <- c("United States of America")
#ЕС
EU <- c("EU-27", "Germany", "Estonia", "Lithuania", "Latvia", "Slovenia")
#остальные страны
ROTW <- c("Armenia", "Finland", "Georgia", "Belarus", "Kyrgyzstan", "Kazakhstan", "Ukraine", "Azerbaijan", "Russian Federation", "Mongolia", "New Zealand", "United Arab Emirates", "Egypt")

'for (i in range(dim(DT.import)[1])){
  if (DT.import$Reporter[i] %in% USA) {DT.import$Reporter[i] <- "USA"}
}'

'unique(DT.import$Reporter)'

#ключевое поле ----
setkey(DT.import, Reporter)
#3 отдельных таблицы
DT.import.USA <- DT.import[USA]
DT.import.EU <- DT.import[EU]
DT.import.ROTW <- DT.import[ROTW]
#принадлежность к группам стран
DT.import1 <- mutate(DT.import.USA, country_factor = 'USA')
DT.import2 <- mutate(DT.import.EU, country_factor = 'EU')
DT.import3 <- mutate(DT.import.ROTW, country_factor = 'ROTW')
#объединение таблиц
DT.import <- data.table()
DT.import <- full_join(DT.import1, DT.import2)
DT.import <- full_join(DT.import, DT.import3)



#считаем суммарные постаки по годам и союзу
res <- select(DT.import, Netweight.kg.median, country_factor,Year) %>%
  group_by(country_factor, Year)
res1 <- na.omit(res)
res1 <- data.table(res1)

#фактор по 3 группам стран
years <- as.factor(unique(res1$country_factor))

# Пакет "base" ----

#нужна палитра из 3 цветов
cls <- palette(rainbow(3))

# КОРОБЧАТАЯ ДИАГРАММА
# ящики с усами по месяцам
png('Pic-01.png', width = 500, height = 500)
boxplot(res1$Netweight.kg.median ~ as.factor(res1$country_factor) * as.factor(res1$Year),
        xlab = 'Год и группа стран',
        ylab = 'Суммарные поставки',
        main = 'пакет base'
        )

dev.off()

# Пакет "lattice" ----
# КОРОБКИ ПО ГРУППАМ
png('Pic-02.png', width = 500, height = 500)
bwplot( ~ Netweight.kg.median | as.factor(Year) * as.factor(country_factor), data = res1,
       xlab = 'Суммарные поставки',
       main = 'пакет lattice'
       )
dev.off()

# Пакет "ggplot2" ----

# КОРОБКИ ПО ГРУППАМ (ЦВЕТ + ОСЬ)
# всё, что зависит от значений данных, заносим в аргумент aes
res1[, Группы_стран := factor(country_factor, levels = c('USA', 'EU', 'ROTW'),
                                labels = c('США', 'Страны ЕС', 'Остальные страны'))]
png('Pic-03.png', width = 500, height = 500)
gp <- ggplot(data = res1, aes(x = as.factor(res1$Year),
                                y = Netweight.kg.median,
                                color = Группы_стран)
             
             )
gp <- gp + geom_boxplot()
gp <- gp + xlab('Год')
gp <- gp + ylab('Суммарные поставки')
gp

dev.off()
