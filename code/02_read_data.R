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

# Master lists for reference tables
url <- 'https://docs.google.com/spreadsheets/d/1wBiJ2kaRC9Fr87qAlW9xKdj7-h1InEv4zQQaR5-hXW8/edit#gid=0'

company <- read_sheet(url, sheet = 1)
write.csv(company, "data_indices/company.csv", row.names = F)

other_licensee <- read_sheet(url, sheet = 2)
write.csv(other_licensee, "data_indices/other_licensee.csv", row.names = F)

agreements <- read_sheet(url, sheet = 3)
write.csv(agreements, "data_indices/agreements.csv", row.names = F)

crop <- read_sheet(url, sheet = 4)
write.csv(crop, "data_indices/crop.csv", row.names = F)


       