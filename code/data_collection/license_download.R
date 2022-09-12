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

# Fix this
rpl_na <- function(x) {ifelse(x == "NANA" | x == "NA" | x == "NULL", NA, x)}
df[,c(3,4,12:13)] <- data.frame(sapply(df[,c(3,4,12:13)], as.character))
df[3936, "variety_name"]
df[,c(3,4,12:13)] <- data.frame(sapply(df[,c(3,4,12:13)], rpl_na))
df[3936, "variety_name"]
df <- data.frame(sapply(df, unlist))

# This should be the number of licenses
df2 <- df %>% select(-department) %>% unique()

df3 <- df %>% 
  group_by(variety_name, invention_name, licensee, effective_date) %>% 
  mutate(dept_count = row_number()) %>% 
  pivot_wider(names_from = dept_count, values_from = department)

df2[c(1:9, 12:14)] = data.frame(lapply(df2[c(1:9, 12:14)], as.character))
write.csv(df2, "data_clean/license.csv", row.names = F)
