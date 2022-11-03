library(dplyr)
library(rvest)
library(stringr)
library(data.table)

options(java.parameters = "-Xmx1g")

setwd('C:/Users/79175/Documents/rwd/writers_project')
getwd()

### creating functions

get_volume_urls <- function(author_url) {
  volume_urls <- read_html(author_url) %>% 
    html_elements('.col-sm-12 a') %>% 
    html_attr('href')
  volume_urls <- volume_urls[str_detect(volume_urls, 
                                        pattern = 'vol', 
                                        negate = F)]
  
  paste('https://rvb.ru/dostoevski/', volume_urls[!is.na(volume_urls)], sep='')
}


get_title_urls <- function(volume_url) {
  title_urls <- read_html(volume_url) %>% 
    html_elements('.table.table-borderless.table-hover a') %>% 
    html_attr('href')
  
  paste('https://rvb.ru/dostoevski/', 
        unique(title_urls[str_detect(title_urls, 
                                     pattern = 'comm|index|article', 
                                     negate = T)]), 
        sep='')

}


get_title_comments_urls <- function(volume_url) {
  title_comments_urls <- read_html(volume_url) %>% 
    html_elements('.table.table-borderless.table-hover a') %>% 
    html_attr('href')
  
  paste('https://rvb.ru/dostoevski/', 
        unique(title_comments_urls[str_detect(title_comments_urls, 
                                              pattern = 'comm|index|article',
                                              negate = F)]), 
        sep='')
}


#get_title_comments_urls('https://rvb.ru/dostoevski/tocvol9.htm')


get_chapter_urls <- function(title_url) {
  chapter_urls <- read_html(title_url) %>%
    html_elements('.table.table-borderless.table-hover a') %>%
    html_attr('href')
  title_url_trim <- title_url %>% str_trunc(38, ellipsis = '', side = "right")
  
  paste(title_url_trim, unique(chapter_urls[str_detect(title_urls, 
                                                       pattern = 'comm|index|article', 
                                                       negate = T)]), 
        sep='')
}


get_chapter_comments_urls <- function(title_url) {
  chapter_urls <- read_html(title_url) %>%
    html_elements('.table.table-borderless.table-hover a') %>%
    html_attr('href')
  title_url_trim <- title_url %>% str_trunc(38, ellipsis = '', side = "right")
  
  paste(title_url_trim, unique(chapter_urls[str_detect(title_urls, 
                                                       pattern = 'comm|index|article', 
                                                       negate = F)]), 
        sep='')
}


#get_chapter_urls('https://rvb.ru/dostoevski/01text/vol3/19.htm')


parse_chapter_page <- function(chapter_url) {
  message('Scraping URL: ', chapter_url)
  
  html <- read_html(chapter_url)
  heading <- html %>% html_element('h1') %>% html_text2()
  chapter_number <- html %>% html_element('h2') %>% html_text2()
  chapter_text <- html %>% html_elements('.epigr, .author, .date, .adressee, p') %>% html_text2()
  
  tibble(heading, chapter_number, chapter_text)
}




### Parsing corpus


corpus_url <- 'https://rvb.ru/dostoevski/toc.htm'

volume_urls <- get_volume_urls(corpus_url)
  
# getting links of titles

title_urls <- list()

for (vol in volume_urls) {
  title_urls[[vol]] <- get_title_urls(vol)
}
  
title_urls <- unlist(title_urls, use.names = F)
title_urls_full <- list()

# some of them contain nested chapter links
# if a link contain nested links, getting chapter links and removing it 
  
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

# checking for duplicates, comments, NA, links with # which duplicate each other

duplicated(c(title_urls, title_urls_full))

title_urls_full1 <- unique(c(title_urls, title_urls_full))

title_urls_full1[str_detect(title_urls_full1, 'comm')]

title_urls_full2 <- title_urls_full1[str_detect(title_urls_full1, 'NA', negate = T)]

title_urls_full2[str_detect(title_urls_full2, '#')]

# creating a dataframe

title_urls_full2 <- unlist(title_urls_full2, use.names = F)
title_urls_df <- data.frame(title_urls_full2)
title_urls_df <- title_urls_df %>% 
  mutate(id = seq.int(1:nrow(title_urls_df))) %>% 
  mutate(volume = str_extract(title_urls_df$title_urls_full2, 'vol[:digit:]+'))
  
rm(title_urls_full)

# saving df with scraped links to csv

write.csv(title_urls_df,"title_urls_df.csv", row.names = FALSE)


# testing parsing function:

test <- list()
for (t_url in title_urls_df$title_urls_full2[1:10]) {
  test[[t_url]] <- parse_chapter_page(t_url)
}
bind_rows(test, .id = 't_url') 
test[[8]][2,3]

#test1 <- rbindlist(test, idcol = 'url')  with column of used urls
test1 <- rbindlist(test)
test1[is.na(test1$chapter_text)]

# merging rows into full text

test2 <- test1 %>% 
  group_by(heading) %>% 
  summarise(chapter_text = paste(chapter_text, collapse = " "))

rm(test)
rm(test1)
rm(test2)


# parsing all links

all_texts <- list()
for (t_url in title_urls_full2) {
  all_texts[[t_url]] <- parse_chapter_page(t_url)
}
bind_rows(all_texts, .id = 't_url') 
all_texts[[8]][2,3]

all_texts_df <- rbindlist(all_texts)
all_texts_df[is.na(all_texts_df$chapter_text)]

names(all_texts_df)

full_texts <- all_texts_df %>% 
  arrange(heading, chapter_number) %>% 
  group_by(heading) %>% 
  summarise(chapter_text = paste(chapter_text, collapse = " "))

names(full_texts) <- c('title', 'text')

write.csv(full_texts,"dostoevsky_all.csv", row.names = FALSE)
