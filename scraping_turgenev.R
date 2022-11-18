library(dplyr)
library(netstat)
library(RSelenium)
library(rvest)
library(stringr)
library(purrr)

setwd('C:/Users/79175/Documents/rwd/writers_project')
getwd()

# ссылка на все собрание сочинений
main_url <- 'https://russian-literature.org/author/Turgenev'

# ссылки на тома
volume_urls <- read_html(main_url) %>% 
  html_elements('.children-object-data a') %>% 
  html_attr('href')
volume_urls <-  str_remove(volume_urls, '\n') %>% 
  str_squish()
volume_urls <- paste0('https://russian-literature.org', volume_urls)


# подключаем selenium

# функция для определения подходящей версии браузера, для аргумента chromever
#binman::list_versions("chromedriver")

rs_driver_object <- rsDriver(
  browser = 'chrome',
  verbose = F,
  chromever = '107.0.5304.18',
  port = free_port()
)

remDr <- rs_driver_object$client

remDr$open()

# собираем тексты со всех страниц

volume_texts <- list()

for (v in volume_urls) {
  remDr$navigate(v)
  Sys.sleep(3)
  pages <- remDr$findElements(
    using='class name', 'image-viewer-image') # все страницы тома
  Sys.sleep(10)
  volume_texts[[v]] <- lapply(
    pages, function(x) {x$getElementAttribute('alt')})
}

turgenev <- lapply(X = volume_texts, FUN = unlist)
