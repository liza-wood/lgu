library(googlesheets4)
library(dplyr)
library(tidyr)
library(stringr)
# Set working director to Box
setwd("~/Box/lgu")

# This script reads in data from Google Sheets and organizes it into CSV files 
# Licensing data
url <- 'https://docs.google.com/spreadsheets/d/1OZVQGljPyGwO6dBag350YNigns0T4JET4YbuOkkhn30/edit#gid=135995447'
states <- read_sheet(url, sheet = 1)[c(1,2)] %>% 
  filter(!is.na(State) & Status %in% c("Done", "~ Done")) %>% 
  mutate(State = unlist(State))

df <- data.frame()
for(i in 1:nrow(states)){
  sheet <- read_sheet(url, sheet = i+1)
  sheet$uni_state <- states$State[i]
  df <- rbind(df, sheet)
}

# Fix this
rpl_na <- function(x) {ifelse(x == "NANA" | x == "NA" | x == "NULL", NA, x)}
df[,c(1:20,22:24,26:30)] <- data.frame(sapply(df[,c(1:20,22:24,26:30)], as.character))
df[,c(1:20,22:24,26:30)] <- data.frame(sapply(df[,c(1:20,22:24,26:30)], rpl_na))
df$pvp_id <- as.character(df$pvp_id)

# Lengthen
longfirst <- df %>% 
  select(variety_name, contains('inventor_first')) %>% 
  pivot_longer(cols = c(paste0('inventor_first', rep(1:8))),
               names_to = 'number', values_to = 'inventor_first') %>% 
  select(-number, -variety_name)
longlast <- df %>% 
  select(variety_name, contains('inventor_last')) %>% 
  pivot_longer(cols = c(paste0('inventor_last', rep(1:8))),
               names_to = 'number', values_to = 'inventor_last')
long <- cbind(longlast, longfirst) %>% 
  filter(!is.na(inventor_last))

df2 <-df %>% select(-c(paste0('inventor_first', rep(1:8)),
                      paste0('inventor_last', rep(1:8)))) %>% 
  left_join(long, multiple = 'all')

df2$inventor_first <- trimws(df2$inventor_first)
df2$inventor_first <- str_remove_all(df2$inventor_first, "\\.")
df2$inventor_last <- trimws(df2$inventor_last)
df2$inventor_last <- str_remove_all(df2$inventor_last, "\\,")
df2$inventor_first_1 <- str_extract(df2$inventor_first, "^\\w{1}")

invention <- select(df2, -c(number, inventor_last, 
                            inventor_first, inventor_first_1)) %>% 
  filter(!is.na(variety_name)) %>% 
  unique() %>% 
  mutate(invention_ID = 1:nrow(.))

write.csv(invention, "data_clean/invention_index.csv", row.names = F)

inventor <- select(df2, uni_state, inventor_last, 
                   inventor_first, inventor_first_1) %>% 
  filter(!is.na(inventor_last)) %>% 
  unique() %>% 
  mutate(inventor_ID = 1:nrow(.))

write.csv(inventor, "data_clean/inventor_index.csv", row.names = F)

df3 <- left_join(df2, invention)
df3 <- left_join(df3, inventor)
write.csv(df3, "data_clean/inventor_invention.csv", row.names = F)
