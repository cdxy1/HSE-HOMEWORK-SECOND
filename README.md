---
ДУБЛИРУЮ MarkDown ЧТОБЫ ВАМ БЫЛО ХОРОШО ВИДНО))
---

**Здесь я делаю все импорты**

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(ggplot2)
library(dplyr)
library(plotly)
```

Данные можно загрузить так:

```{r}
data <- read.csv('https://raw.githubusercontent.com/fivethirtyeight/data/master/comic-characters/marvel-wikia-data.csv')
```

**Задание 1.** (2 балла)

1.1 Выберите любую категориальную переменную. С помощью ggplot cоздайте график, который показывал бы, сколько персонажей относящихся к разным категориям этой переменной есть в датасете. 1.2 Используйте разные цвета для каждой из категорий. 1.3 Добавьте к графику заголовок, название легенды и её категорий. Названия осей графика уберите или измените. 1.4.Какой вывод можно сделать по этому графику?

-   обратите внимание, что в этих данных есть пустые значения. Вы можете их убрать или переименовать - это сделает ваши графики лучше.

```{r}
data <- data %>%
  filter(!is.na(ALIGN)) %>%
  mutate(ALIGN = trimws(toupper(ALIGN)))

unique(data$ALIGN)

data <- data %>%
  filter(ALIGN != "")

count_data <- data %>%
  group_by(ALIGN) %>%
  summarise(count = n())

ggplot(data, aes(x = ALIGN, fill = ALIGN)) +
  geom_bar() +
  scale_fill_manual(values = c("GOOD CHARACTERS" = "#00FF00", "BAD CHARACTERS" = "#FF0000", "NEUTRAL CHARACTERS" = "#0000FF")) + 
  labs(
    title = "Распределение персонажей по категориям ALIGN",
    fill = "Категория персонажа"
  ) +
  theme_minimal() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())


max_category <- count_data[which.max(count_data$count), ]

cat("Категория с наибольшим количеством персонажей:", max_category$ALIGN, "Количество персонажей в этой категории:", max_category$count, "\n")

```

Вывод:

**Задание 2.** (1 балл) Повторите задания 1.1 и 1.2 в Plotly (использовать ggplotly нельзя!). (Если в рамках мы не проходили тот тип графика, который вы использовали в первом задании, в plotly, можете поменять тип графика.)

-   задание 1.3 можете тоже можно повторить.

```{r}
data <- data %>%
  filter(!is.na(ALIGN)) %>%
  mutate(ALIGN = trimws(toupper(ALIGN)))

data <- data %>%
  filter(ALIGN != "")

count_data <- data %>%
  group_by(ALIGN) %>%
  summarise(count = n())

plot <- plot_ly(data = count_data, x = ~ALIGN, y = ~count, type = 'bar', 
                 marker = list(color = c("GOOD CHARACTERS" = "#00FF00", 
                                          "BAD CHARACTERS" = "#FF0000", 
                                          "NEUTRAL CHARACTERS" = "#0000FF")[count_data$ALIGN])) %>%
  layout(title = "Распределение персонажей по категориям ALIGN",
         xaxis = list(title = ""),
         yaxis = list(title = "Количество персонажей"),
         showlegend = TRUE)

plot

```

**Задание 3.** (2 балла)

3.1 Рассчитайте количество первых появлений персонажей по годам. Подсказка: переменная FIRST APPEARANCE для этого не нужна. Используйте просто переменную, которая указывает год первого появления персонажа. 3.2 Создайте график, который показывал бы, сколько персонажей появлялось каждый год с 1960 по 2000.

```{r}
data <- data %>%
  filter(!is.na(Year) & Year >= 1960 & Year <= 2000)

count_years <- data %>%
  group_by(Year) %>%
  summarise(count = n())

barplot(count_years$count, names.arg = count_years$Year, 
        col = "blue", 
        main = "Количество персонажей по годам (1960-2000)", 
        xlab = "Год", 
        ylab = "Количество персонажей")
print(count_years)
```

**Задание 4.** (1 балл)

-   Воссоздайте график из задания 3 с помощью plotly. В какой год появилось больше всего новых персонажей комиксов? Сколько их было в тот год?

```{r}
data <- data %>%
  filter(!is.na(Year) & Year >= 1960 & Year <= 2000)

count_years <- data %>%
  group_by(Year) %>%
  summarise(count = n())

plot <- plot_ly(data = count_years, x = ~Year, y = ~count, type = 'bar', 
                 marker = list(color = 'blue')) %>%
  layout(title = "Количество персонажей по годам (1960-2000)",
         xaxis = list(title = "Год"),
         yaxis = list(title = "Количество персонажей"),
         showlegend = FALSE)

plot

max_year <- count_years[which.max(count_years$count), ]
cat("Год с наибольшим количеством новых персонажей:", max_year$Year, "\n")
cat("Количество новых персонажей в этом году:", max_year$count, "\n")

```

Ответ: в 1993 году появилось 456 персонажей

**Задание 5.** (4 балла)

-   Представьте, что на презентации магистерских работ вам нужно показать подслеповатым профессорам самый красивый и понятный график. Исправьте ваш "плохой график" из мини-дз так, чтобы профессора остались довольны.

(Если в мини-дз ваша фантазия зашла слишком далеко и созданный график исправлению не подлежит, можете сделать аналогичный график, например, с другими переменными).

```{r}
# ЭТО DC датасет!!!!!!
data <- read.csv('https://raw.githubusercontent.com/fivethirtyeight/data/master/comic-characters/dc-wikia-data.csv')

data_cleaned <- data %>%
  filter(!is.na(SEX) & SEX != "")

sex_count <- data_cleaned %>%
  group_by(SEX) %>%
  summarise(count = n())

ggplot(sex_count, aes(x = SEX, y = count, fill = SEX)) +
  geom_bar(stat = "identity") +
  labs(title = "Распределение персонажей по полу",
       x = "Пол",
       y = "Количество персонажей",
       fill = "Пол") +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label = count), vjust = -0.5)
```

Взял самые примитивные данные из соседнего дата сета.
