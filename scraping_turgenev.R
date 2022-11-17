library(dplyr)
library(rvest)
library(stringr)
library(data.table)
library(netstat)
library(RSelenium)
library(purrr)



setwd('C:/Users/79175/Documents/rwd/writers_project')
getwd()

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
remDr$navigate('https://russian-literature.org/tom/417796')

#remDr$goBack()
#remDr$goForward()
#remDr$refresh()

# определяем элементы

open_text_button <-  remDr$findElement(    # открывает окно с текстом
  using = 'xpath', 
  '//*[@id="objectDD-252324"]/div[2]/div[1]/div[2]/div[1]/button[8]'
  )

close_text_button <- remDr$findElement(    # закрывает окно с текстом
  using = 'xpath', 
  '//*[@id="imageTreePageTextModal"]/div/div/div[3]/button'
  )

next_page_button <-  remDr$findElement(    # перелистывает страницу
  using = 'xpath', 
  '//*[@id="objectDD-252324"]/div[2]/div[1]/div[2]/div[1]/button[5]'
  )

page_text <- remDr$findElement(            # текст страницы
  using = 'class name', 
  'image-viewer-page-text'
)

page_count <- remDr$findElement(           # количество страниц в томе
  using = 'class name',
  'page-count'
)

page_text$getElementText() %>% flatten_chr()
page_count$getElementText() %>% unlist() %>% as.numeric()
open_text_button$clickElement()
close_text_button$clickElement()
next_page_button$clickElement()

# функция для парсинга 1 тома

parse_page <- function() {
  volume_text <- list()
  pages <- page_count$getElementText() %>% unlist() %>% as.numeric()
  
  for (i in seq(1, pages)) {
    open_text_button$clickElement()
    Sys.sleep(1)
    volume_text[[i]] <-  page_text$getElementText() %>% flatten_chr()
    Sys.sleep(1)
    close_text_button$clickElement()
    Sys.sleep(1)
    next_page_button$clickElement()
    Sys.sleep(1)
  }    
    return (volume_text)
}

# собираем тексты

vol1_text <- parse_page()
vol1 <- vol1_text %>% unlist() %>% paste(collapse = ' ')

write.table(vol1, file='turgenev_vol1.txt')
