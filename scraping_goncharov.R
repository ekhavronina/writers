library(dplyr)
library(rvest)
library(stringr)
library(data.table)


setwd('C:/Users/79175/Documents/rwd/writers_project')
getwd()


# создаем функцию для сбора текстов


parse_volume_page <- function(url) {

  html <- read_html(url)
  heading <- html %>% html_element('.t1') %>% html_text2()
  volume_text <- html %>% html_elements('.fb2 p') %>% html_text2()
  
  tibble(heading, volume_text)
}




### Собираем тексты


volume_urls <- list()
for (i in seq(1,8)) {
  volume_urls[i] <- paste(
    'https://traumlibrary.ru/book/goncharov-ss8-0', i, '/goncharov-ss8-0', i, '.html',
    sep = '')
}

all_texts <- list()
for (url in volume_urls) {
  message('Parsing url: ', url)
  all_texts[[url]] <- parse_volume_page(url)
}

all_texts <- rbindlist(all_texts)

goncharov <- all_texts %>% 
  group_by(heading) %>% 
  summarise(text = paste(volume_text, collapse = " "))

write.csv(goncharov,"goncharov_all.csv", row.names = FALSE)
