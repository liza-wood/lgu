library(googlesheets4)
library(dplyr)
library(tidyr)

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
write.csv(df, "data_clean/inventor_invention.csv", row.names = F)
