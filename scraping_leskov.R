library(dplyr)
library(rvest)
library(stringr)
library(data.table)

options(java.parameters = "-Xmx1g")

setwd('C:/Users/79175/Documents/rwd/writers_project')
getwd()


get_volume_urls <- function(author_url) {
  volume_urls <- read_html(author_url) %>% 
    html_elements('h3 a') %>% 
    html_attr('href')
  volume_urls <- volume_urls[str_detect(volume_urls, 
                                        pattern = 'vol', 
                                        negate = F)]
  
  paste('https://rvb.ru/leskov/', volume_urls[!is.na(volume_urls)], sep='')
}


get_title_urls <- function(volume_url) {
  title_urls <- read_html(volume_url) %>% 
    html_elements('.table.table-borderless.table-hover a') %>% 
    html_attr('href')
  
  paste('https://rvb.ru/leskov/', 
        unique(title_urls[str_detect(title_urls, 
                                     pattern = 'comm|index|article', 
                                     negate = T)]), 
        sep='')
  
}



get_chapter_urls <- function(title_url) {
  chapter_urls <- read_html(title_url) %>%
    html_elements('.table.table-borderless.table-hover a') %>%
    html_attr('href')
  title_url_trim <- title_url %>% str_trunc(36, ellipsis = '', side = "right")
  
  paste(title_url_trim, unique(chapter_urls[str_detect(title_urls, 
                                                       pattern = 'comm|index|article', 
                                                       negate = T)]), 
        sep='')
}


parse_chapter_page <- function(chapter_url) {
  message('Parsing URL: ', chapter_url)
  
  html <- read_html(chapter_url)
  heading <- html %>% html_element('h1') %>% html_text2()
  chapter <- html %>% html_element('h2') %>% html_text2()
  text <- html %>% 
    html_elements('.epigr, .author, .date, .adress, .location, .signature, p') %>% html_text2()
  
  tibble(heading, chapter, text)
}



### Parsing corpus


corpus_url <- 'https://rvb.ru/leskov/toc.htm'

volume_urls <- get_volume_urls(corpus_url)

title_urls <- list()
#sapply?
for (vol in volume_urls) {
  title_urls[[vol]] <- get_title_urls(vol)
}


title_urls <- unlist(title_urls, use.names = F)

title_urls[str_detect(title_urls, 'comm')]
title_urls[is.na(title_urls)]
title_urls[!str_detect(title_urls, '\\.htm$')]

# fixing bugs for the next loop:
str_which(title_urls, 'https://rvb.ru/leskov/01text/vol_02/009-1-27htm')

title_urls[35] <- 'https://rvb.ru/leskov/01text/vol_02/009-1-27.htm'

title_urls <- title_urls[-str_which(title_urls, 
                                    'https://rvb.ru/leskov/01text/vol_06/027.htm')]

#title_urls_full[str_detect(title_urls_full, 
#                      'https://rvb.ru/leskov/01text/vol_02/009-1-27htm')]


title_urls <- title_urls[-str_which(title_urls, 
                                    'https://rvb.ru/leskov/01text/vol_07/052.htm')]

title_urls <- title_urls[-str_which(title_urls, 
                                    'https://rvb.ru/leskov/01text/vol_11/01autobiography/223.htm')]

title_urls <- title_urls[-str_which(title_urls, 
                                    'https://rvb.ru/leskov/01text/vol_11/04letters/305.htm')]

title_urls <- title_urls[-str_which(title_urls, 
                                    'https://rvb.ru/leskov/01text/vol_11/04letters/306.htm')]

title_urls <- title_urls[-str_which(title_urls, 
                                    'https://rvb.ru/leskov/bio/kanva.htm')]


#title_urls_df <- data.frame(title_urls)

title_urls_full <- list()

# in case of an error with a next loop use:
str_which(title_urls, 'https://rvb.ru/leskov/01text/vol_11/04letters/305.htm')
# and run the code again: for (t_url in title_urls[341:654])


for (t_url in title_urls) {
  message('Scraping URL: ', t_url)
  
  html <- read_html(t_url) %>%
    html_elements('.table.table-borderless.table-hover a') %>%
    html_attr('href')
  
  if (length(html) > 0) {
    title_urls_full <- c(title_urls_full, get_chapter_urls(t_url))
    title_urls <- title_urls[title_urls != t_url]
  }
}

# cleaning

duplicated(c(title_urls, title_urls_full))

title_urls_full <- unique(c(title_urls, title_urls_full))
title_urls_full1[duplicated(title_urls_full1)]


title_urls_full[str_detect(title_urls_full, 'comm')]

title_urls_full <- title_urls_full[str_detect(title_urls_full, 'NA', negate = T)]

title_urls_full <- unlist(title_urls_full, use.names = F)

title_urls_full <- title_urls_full[!str_detect(title_urls_full, '#')]
title_urls_c <- title_urls_full[str_detect(title_urls_full, '#')]
title_urls_c <- str_remove(title_urls_c, '#[a-z]*[:digit:]*')
title_urls_c <- unique(title_urls_c)

title_urls_full <- c(title_urls_full, title_urls_c)

rm(title_urls_c)
rm(title_urls)


# testing parsing function:
test <- list()
for (t_url in title_urls_full[1:10]) {
  test[[t_url]] <- parse_chapter_page(t_url)
}
test <- bind_rows(test, .id = 'url') 
names(test)
is.na(test$text)

# merged rows into full text:
test1 <- test %>% 
  group_by(url, heading, chapter) %>% 
  summarise(text = paste(text, collapse = " "))

rm(test)
rm(test1)
rm(test2)

rm(html)
rm(vol)
rm(t_url)


### parsing all urls

title_urls_full <- title_urls_full[-str_which(title_urls_full, 
                                    'https://rvb.ru/leskov/01text/vol_02/000-full.htm')]
title_urls_full <- title_urls_full[!duplicated(title_urls_full)]

all_texts <- list()
for (t_url in title_urls_full) {
  all_texts[[t_url]] <- parse_chapter_page(t_url)
}

# in case of error (check index first):
# title_urls_full[627] <- 'https://rvb.ru/leskov/01text/vol_02/009-1-27.htm'

all_texts_list <- all_texts
names(all_texts[[1]])

bind_rows(all_texts, .id = 'url') 
#or
all_texts_df <- rbindlist(all_texts, idcol = T)
all_texts_df[is.na(all_texts_df$chapter_text)]
View(head(all_texts_df))

names(all_texts_df)

full_texts <- all_texts_df %>% 
  group_by(.id, heading, chapter) %>% 
  summarise(text = paste(text, collapse = " "))

names(full_texts) <- c('url','heading', 'chapter', 'text')

View(head(full_texts))

View(tail(full_texts))

leskov_texts <- full_texts %>% 
  ungroup() %>% 
  mutate(volume = str_extract(full_texts$url, 'vol_[:digit:]+'))

leskov_texts <- leskov_texts %>% 
  mutate(volume = str_trunc(volume, 2, 'left', ellipsis = ''))

View(head(leskov_texts))

leskov_texts$volume <- as.numeric(leskov_texts$volume)
str(leskov_texts)

leskov_texts <- leskov_texts %>% 
  arrange(volume, url)


# saving corpus
write.csv(leskov_texts,"leskov_all.csv", row.names = FALSE)


rm(all_texts)
rm(all_texts_df)
rm(all_texts_list)
rm(full_texts)

rm(t_url, corpus_url, title_urls_full, volume_urls)


# adding texts from removed links