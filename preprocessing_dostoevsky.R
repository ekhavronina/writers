library(readr)
library(stringr)
library(dplyr)
library(tidytext)
library(stopwords)
library(quanteda)

setwd('C:/Users/79175/Documents/rwd')


dostoevsky_all <- read_csv('writers_project/dostoevsky_all.csv', 
                           locale = readr::locale(encoding = "cp1251"))


dostoevsky_all$clean <- gsub("[^[:alnum:]\\<>чЧёЁ\\(\\)]", " ", dostoevsky_all$text)
dostoevsky_all$clean <- str_replace_all(dostoevsky_all$clean, ">", "")
dostoevsky_all$clean <- str_replace_all(dostoevsky_all$clean, "<", "")
dostoevsky_all$clean <- str_replace_all(dostoevsky_all$clean, "\\)", "")
dostoevsky_all$clean <- str_replace_all(dostoevsky_all$clean, "\\(", "")
dostoevsky_all$clean <- str_replace_all(dostoevsky_all$clean, "[0-9]+", " ")
dostoevsky_all$clean <- str_replace_all(dostoevsky_all$clean, "[\n\t\r]+", " ")
dostoevsky_all$clean <- str_squish(dostoevsky_all$clean)


View(head(dostoevsky_all))
dostoevsky_all$clean[1]
dostoevsky_all$text[1]

dostoevsky_clean <- dostoevsky_all %>% 
  select(title, clean)

# saving
#write.csv(dostoevsky_clean,"dostoevsky_clean.csv", row.names = FALSE)



# importing cleaned data:
#dostoevsky_clean <- read_csv('writers_project/dostoevsky_clean.csv', 
#                             locale = readr::locale(encoding = "cp1251"))



### preprocessing

dostoevsky_gram <- dostoevsky_clean %>% 
  group_by(title) %>% 
  unnest_tokens(gram, clean)

dostoevsky_gram$word <- system2("mystem", c("-d", "-l", "-c"),
                                  input = dostoevsky_gram$gram, stdout = TRUE)

View(head(dostoevsky_gram))

# saving
#write.csv(dostoevsky_gram,"dostoevsky_gram.csv", row.names = FALSE)

dostoevsky_words <- read_csv('writers_project/dostoevsky_gram.csv')


dostoevsky_words <- dostoevsky_gram
dostoevsky_words$word <-gsub("[[:punct:]]", "", dostoevsky_words$word)

sw <- stopwords('ru')
dostoevsky_words <- dostoevsky_words %>% 
  select(title, word) %>% 
  filter(!word %in% sw)


View(head(dostoevsky_words))

# this list contains Roman alphabet
dos_freq <- dostoevsky_words %>% group_by(word) %>% count() %>% arrange(desc(n))


