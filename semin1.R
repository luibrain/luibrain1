# рейтинг холодильников с Я.маркета

library('rvest') 
# URL страницы для скраппинга
url <- 'https://market.yandex.ru/catalog--kholodilniki/71639/list?hid=15450081&onstock=1&local-offers-first=0&page=2'

# читаем HTML страницы
webpage <- read_html(url) 

# функция перебора тегов внутри тегов более высокого уровня
get_tags <- function(node){
  # найти все теги с метарейтингом
  raw_data <- html_nodes(node, selector) %>% html_text 
  # значения нулевой длины (для фильма нет такого тега) меняем на пропуски
  data_NAs <- ifelse(length(raw_data) == 0, NA, raw_data) }

# Название холодильника ----
name_data <- html_nodes(webpage, '.n-snippet-card2__title') %>% html_text 
length(name_data)
head(name_data)

# Описание ----
# Описание1
description1_data <- html_nodes(webpage,'div.n-snippet-card2__content > ul> li:nth-child(1)') %>% html_text
length(description1_data)
head(description1_data)
# Описание2
description2_data <- html_nodes(webpage,'div.n-snippet-card2__content > ul> li:nth-child(2)') %>% html_text
length(description2_data)
head(description2_data)
# Описание3
description3_data <- html_nodes(webpage,'div.n-snippet-card2__content > ul> li:nth-child(3)') %>% html_text
length(description3_data)
head(description3_data)
# Описание4
description4_data <- html_nodes(webpage,'div.n-snippet-card2__content > ul> li:nth-child(4)') %>% html_text
length(description4_data)
head(description4_data)
# Описание5
description5_data <- html_nodes(webpage,'div.n-snippet-card2__content > ul> li:nth-child(5)') %>% html_text
length(description5_data)
head(description5_data)

# Цена на холодильник ----
price_data <- html_nodes(webpage,'div.n-snippet-card2__price') %>% html_text
price_data <- gsub( '\\W','', price_data)
length(price_data)
head(price_data)
price_data <- as.numeric(price_data)

# Оценка ----
selector <- 'div.rating__value'
doc <- html_nodes(webpage, 'div.n-snippet-card2__header-stickers')
stars_data <-sapply(doc, get_tags)
length(stars_data)
head(stars_data)
stars_data <- as.numeric(stars_data)

# Количество отзывов ----
selector <- 'span'
doc <- html_nodes(webpage, 'div.n-snippet-card2__header-stickers')
counto_data <- sapply(doc, get_tags)
length(counto_data)
head(counto_data)
counto_data <- gsub( '\\W','', counto_data)
counto_data <- as.numeric(counto_data)

# Количество предложений ----
countp_data <- html_nodes(webpage, 'div.n-snippet-card2__more-prices-link > a') %>%html_text
length(countp_data)
head(countp_data)
countp_data <- gsub( '\\W','', countp_data)
countp_data <- as.numeric(countp_data)



# Количество человек, купивших товар ----
selector <- 'span'
doc <- html_nodes(webpage, 'div.n-snippet-card2__content')
countb_data <- sapply(doc, get_tags)
length(countb_data)
head(countb_data)
countb_data <- gsub( '\\W','', countb_data)
countb_data <- as.numeric(countb_data)

# Последний отзыв ----
selector <- 'div.n-badge-review__text > a'
doc <- html_nodes(webpage, 'div.n-snippet-card2__bottom')
lasto_data <- sapply(doc, get_tags)
length(lasto_data)
head(lasto_data)

# совмещаем данные в один фрейм
DF_refridge <- data.frame(Name = name_data, Description1 = description1_data,
                        Description2 = description2_data, Description3 = description3_data,
                        Description4 = description4_data, Description5 = description5_data,
                        Price = price_data, Stars = stars_data, Counto = counto_data,
                        Countp = countp_data, Countb = countb_data,
                        Lasto = lasto_data,
                        stringsAsFactors = F)
# результат
dim(DF_refridge)
str(DF_refridge)

# записываем в .csv
write.csv(DF_refridge, file = '../Refridge_Ya.Market.csv', row.names = F)
