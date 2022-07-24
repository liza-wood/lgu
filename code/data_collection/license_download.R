library(googlesheets4)
library(dplyr)

# Set working director to Box
setwd("~/Box/lgu")

# This script reads in data from Google Sheets and organizes it into CSV files 
# Licensing data
url <- 'https://docs.google.com/spreadsheets/d/1Bm1gs84ZEdifiQ9ZAumyQKP_hOsd6KxyFX3HPTIBkTY/edit#gid=1541879545'
states <- read_sheet(url, sheet = 2)[c(1,2)] %>% 
  filter(!is.na(State) & Status == "Done")

df <- data.frame()
for(i in 1:nrow(states)){
  sheet <- read_sheet(url, sheet = i+3)
  sheet$state <- states$State[i]
  df <- rbind(df, sheet)
}

df[c(1:9, 12:14)] = data.frame(lapply(df[c(1:9, 12:14)], as.character))
write.csv(df, "data_clean/license.csv", row.names = F)