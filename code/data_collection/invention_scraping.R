library(rvest)
library(xml2)
library(httr)
library(pdftools)
library(tesseract)

df <- read.csv("data_clean/inventor_invention.csv")
table(df$ip_type)

# Scraping PTO data
pp <- filter(df, ip_type == "USPTO")
testurl <- pp$invention_url[1]
html <- read_html(testurl)
header <- html %>% 
  xml_find_all('//hr+//font') %>% 
  html_text()
abstract <- html %>% 
  xml_find_all('//center+//p') %>% 
  html_text()
html_text(html)

# Scraping PVP data
pvp <- filter(df, ip_type == "PVP")

testurl <- pvp$invention_url[55]
pdf <- tesseract::ocr(testurl)

pdf[5]

