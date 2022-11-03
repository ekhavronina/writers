library(tidyverse)
library(XML)

headers1 <- xmlParse('TEI-master/headers/header_azbuka_v5.xml')
print(headers1)

test <- xmlParse('test.xml')
print(test)

?xmlParse

xmlParse('TEI-master/headers/header_azbuka_v5.xml')

kek_system <- xmlTreeParse('TEI-master/headers/header_azbuka_v5.xml', encoding = "UTF-8")

Sys.getlocale("LC_CTYPE")

apply(kek_system$doc$children, 2, function(x) iconv(x, from = "UTF-8", to = "Windows-1252"))

kek_system[[1]]

xmlTreeParse('TEI-master/headers/header_azbuka_v5.xml', encoding = "UTF-8")


iconv(kek_system$doc$children, from = "UTF-8", to = "Windows-1252")

xmlToDataFrame(kek_system)

xmlToDataFrame('TEI-master/headers/header_azbuka_v5.xml') %>% 
  View()

