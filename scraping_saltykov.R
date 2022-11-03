library(dplyr)
library(rvest)
library(stringr)
library(data.table)

options(java.parameters = "-Xmx1g")

setwd('C:/Users/79175/Documents/rwd/writers_project')
getwd()


get_volume_urls <- function(author_url) {
  volume_urls <- read_html(author_url) %>% 
    html_elements('.col-sm-12 a') %>% 
    html_attr('href')
  volume_urls <- volume_urls[str_detect(volume_urls, 
                                        pattern = 'vol', 
                                        negate = F)]
  
  paste('https://rvb.ru/saltykov-shchedrin/', volume_urls[!is.na(volume_urls)], sep='')
}


get_title_urls <- function(volume_url) {
  title_urls <- read_html(volume_url) %>% 
    html_elements('.table.table-borderless.table-hover a') %>% 
    html_attr('href')
  
  paste('https://rvb.ru/saltykov-shchedrin/', 
        unique(title_urls[str_detect(title_urls, 
                                     pattern = 'comm|index|article', 
                                     negate = T)]), 
        sep='')

}


get_title_comments_urls <- function(volume_url) {
  title_comments_urls <- read_html(volume_url) %>% 
    html_elements('.table.table-borderless.table-hover a') %>% 
    html_attr('href')
  
  paste('https://rvb.ru/saltykov-shchedrin/', 
        unique(title_comments_urls[str_detect(title_comments_urls, 
                                              pattern = 'comm|index|article',
                                              negate = F)]), 
        sep='')
}


#get_title_comments_urls('https://rvb.ru/dostoevski/tocvol9.htm')


# fixed (but if it doesn't work correctly, remove '.versusia6'):
parse_chapter_page <- function(chapter_url) {
  message('Parsing URL: ', chapter_url)
  
  html <- read_html(chapter_url)
  heading <- html %>% html_element('h1') %>% html_text2()
  chapter_number <- html %>% html_element('h2') %>% html_text2()
  chapter_text <- html %>% 
    html_elements('.epigr, .author, .date, .adressee, .versusia6, p') %>% html_text2()
  
  tibble(heading, chapter_number, chapter_text)
}



### Parsing corpus


corpus_url <- 'https://rvb.ru/saltykov-shchedrin/toc.htm'

volume_urls <- get_volume_urls(corpus_url)
  
title_urls <- list()
#sapply?
for (vol in volume_urls) {
  title_urls[[vol]] <- get_title_urls(vol)
}


title_urls <- unlist(title_urls, use.names = F)
title_urls_full <- list()


# corrupted url (joining this later):
title_urls <- title_urls[-str_which(title_urls, 
  'https://rvb.ru/saltykov-shchedrin/01text/vol_09/01text/0286.htm')]

title_urls[str_detect(title_urls, 
  'https://rvb.ru/saltykov-shchedrin/01text/vol_09/01text/0286.htm')]


title_urls[str_detect(title_urls, 'comm')]
title_urls[is.na(title_urls)]


title_urls_df <- data.frame(title_urls)

#title_urls_df <- title_urls_df %>% 
#  mutate(id = seq.int(1:nrow(title_urls_df))) %>% 
#  mutate(volume = str_extract(title_urls_df$title_urls, 'vol_[:digit:]+'))
  


# saving df with scraped urls to csv:
write.csv(title_urls_df,"title_urls_saltykov.csv", row.names = FALSE)


# testing parsing function:
test <- list()
for (t_url in title_urls[1:10]) {
  test[[t_url]] <- parse_chapter_page(t_url)
}
bind_rows(test, .id = 't_url') 
test[[8]]
names(test)

#test1 <- rbindlist(test, idcol = 'url')  with column of used urls
test1 <- rbindlist(test)
test1[is.na(test1$chapter_text)]

# merged rows into full text:
test2 <- test1 %>% 
  group_by(heading) %>% 
  summarise(chapter_text = paste(chapter_text, collapse = " "))

rm(test)
rm(test1)
rm(test2)


# parsing all urls

all_texts <- list()
for (t_url in title_urls) {
  all_texts[[t_url]] <- parse_chapter_page(t_url)
}

all_texts_list <- all_texts

bind_rows(all_texts, .id = 'title_url') 
all_texts[[8]][2,3]

all_texts_df <- rbindlist(all_texts, idcol = T)
all_texts_df[is.na(all_texts_df$chapter_text)]

names(all_texts_df)

full_texts <- all_texts_df %>% 
  group_by(.id, heading, chapter_number) %>% 
  summarise(text = paste(chapter_text, collapse = " "))

names(full_texts) <- c('url','heading', 'chapter', 'text')

View(head(full_texts))


# two urls were missed:

texts_urls <- full_texts$url

title_urls[!title_urls %in% texts_urls]

missed_urls <- c('https://rvb.ru/saltykov-shchedrin/01text/vol_01/03poems/0019.htm',
                 'https://rvb.ru/saltykov-shchedrin/01text/vol_01/03poems/0020.htm')
missed_texts <- list()
for (m_url in missed_urls) {
  missed_texts[[m_url]] <- parse_chapter_page(m_url)
}
missed_texts[['https://rvb.ru/saltykov-shchedrin/01text/vol_09/01text/0286.htm']] <- 
  tibble(heading = 'ÑÂÎÈÌ ÏÓÒÅÌ',
         chapter_number = 'Ðîìàí â ÷åòûðåõ ÷àñòÿõ. Ë. À. Îæèãèíîé',
         chapter_text = readLines('0286.txt'))

missed_texts_df <- rbindlist(missed_texts, idcol = T)
missed_texts <- missed_texts_df %>% 
  group_by(.id, heading, chapter_number) %>% 
  summarise(text = paste(chapter_text, collapse = " "))

names(missed_texts) <- c('url','heading', 'chapter', 'text')

saltykov_texts <- bind_rows(full_texts, missed_texts)
View(tail(saltykov_texts))

saltykov_texts <- saltykov_texts %>% 
  ungroup() %>% 
  mutate(volume = str_extract(saltykov_texts$url, 'vol_[:digit:]+'))

saltykov_texts <- saltykov_texts %>% 
  mutate(volume = str_trunc(volume, 2, 'left', ellipsis = ''))

saltykov_texts$volume <- as.numeric(saltykov_texts$volume)
str(saltykov_texts)

saltykov_texts <- saltykov_texts %>% 
  arrange(volume, url)


# saving corpus
write.csv(saltykov_texts,"saltykov_all.csv", row.names = FALSE)

