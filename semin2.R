# загрузка пакетов
library('dplyr')
library('data.table')          # работаем с объектами "таблица данных"
library('moments')             # коэффициенты асимметрии и эксцесса 
library('lattice')
library('ggplot2')

# загружаем файл с данными по импорту масла в РФ (из прошлой практики)
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
# предварительный просмотр
dim(DT.import)            # размерность таблицы
str(DT.import)            # структура (характеристики столбцов)
DT.import          # удобный просмотр объекта data.table


unique(DT.import$Reporter)
unique(DT.import$Period.Desc)
# фильтр
DT.import <- data.table(filter(DT.import, startsWith(Period.Desc, "January ") | startsWith(Period.Desc, "February ")
                                |startsWith(Period.Desc, "March ") |startsWith(Period.Desc, "April ")
                                |startsWith(Period.Desc, "May ") |startsWith(Period.Desc, "June ")
                                |startsWith(Period.Desc, "July ") |startsWith(Period.Desc, "August ")))


# сколько NA в каждом из оставшихся столбцов?
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

#США
USA <- c("United States of America")
#ЕС
EU <- c("EU-27", "Germany", "Estonia", "Lithuania", "Latvia", "Slovenia")
#остальные страны
ROTW <- c("Armenia", "Finland", "Georgia", "Belarus", "Kyrgyzstan", "Kazakhstan", "Ukraine", "Azerbaijan", "Russian Federation", "Mongolia", "New Zealand", "United Arab Emirates", "Egypt")
#нужна палитра из 3 цветов
cls <- palette(rainbow(3))
for (i in dim(DT.import)[1]){
  if (DT.import$Reporter[i]==USA) {DT.import$Reporter[i] <- "USA"}
}

unique(DT.import$Reporter)
