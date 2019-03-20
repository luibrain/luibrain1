library('shiny')       # загрузка пакетов
library('data.table')

file.URL <- 'https://raw.githubusercontent.com/luibrain/luibrain1/laba3/Films-imdb2016.csv'
download.file(file.URL, destfile = 'Films.csv')

df <- data.table(read.csv('Films.csv', 
                          stringsAsFactors = F))

# список уникальных значений столбца Жанр
gn.filter <- as.character(unique(df$Genre))
names(gn.filter) <- gn.filter
gn.filter <- as.list(gn.filter)

# размещение всех объектов на странице
shinyUI(
    # создать страницу с боковой панелью
    # и главной областью для отчётов
    pageWithSidebar(
        # название приложения:
        headerPanel('Кассовые сборы фильмов за 2016 год на портале IMDB'),
        # боковая панель:
        sidebarPanel(
            selectInput(               # выпадающее меню: возраст
                'gn.to.plot',          # связанная переменная
                        'Выберите жанр',  # подпись списка
                        gn.filter),            # сам список
            
            sliderInput('Rating.range', 'Оценка:',
                        min = min(df$Rating), max = max(df$Rating), value = c(min(df$Rating), max(df$Rating))),
            
            sliderInput('Runtime.range', 'Продолжительность:',
                        min = min(df$Runtime), max = max(df$Runtime), value = c(min(df$Runtime), max(df$Runtime))),
            sliderInput(               # слайдер: кол-во интервалов гистограммы
              'int.hist',                       # связанная переменная
              'Количество интервалов гистограммы:', # подпись
              min = 2, max = 10,                    # мин и макс
              value = floor(1 + log(50, base = 2)), # базовое значение
              step = 1)                             # шаг
        ),
        # главная область
        mainPanel(
            # текстовый объект для отображения
            textOutput('gn.text'),
            textOutput('gn.text1'),
            textOutput('gn.text2'),
            textOutput('gn.text3'),
            # гистограммы переменных
            plotOutput('gn.hist')
            )
        )
    )