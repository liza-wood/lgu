library(googlesheets4)
library(tidyverse)
url <- 'https://docs.google.com/spreadsheets/d/16MttMYLRf0Pf19r5O7szf5MAV2rnbOCqrs4RZFFSlZs/edit#gid=0'
practice <- read_sheet(url, sheet = 5)
a <- read_sheet(url, sheet = 2)
colnames(a)[8:ncol(a)] <- paste0(rep(c("theme", "text"), 10), rep(1:10, each = 2))
ap <- pivot_longer(a, cols = seq(8, ncol(a), by = 2), names_to = 'theme_number', values_to = 'theme') %>% 
  pivot_longer(cols = text1:text10, names_to = 'text_number', values_to = 'text') %>% 
  filter(str_extract(theme_number, '\\d+') == str_extract(text_number, '\\d+')) %>% 
  filter(!is.na(text))
table(ap$theme)
ap <- ap %>% 
  mutate(cat = str_remove_all(cat, '\\d+')) %>% 
  pivot_wider(names_from = cat, values_from = text)
v <- read_sheet(url, sheet = 3)


