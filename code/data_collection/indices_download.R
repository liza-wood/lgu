library(googlesheets4)
library(dplyr)

# Set working director to Box
setwd("~/Box/lgu")

# Master lists for reference tables
url <- 'https://docs.google.com/spreadsheets/d/1wBiJ2kaRC9Fr87qAlW9xKdj7-h1InEv4zQQaR5-hXW8/edit#gid=0'

company <- read_sheet(url, sheet = 1)
write.csv(company, "data_indices/company.csv", row.names = F)

mistakes <- read_sheet(url, sheet = 2)
write.csv(mistakes, "data_indices/mistakes.csv", row.names = F)

other_licensee <- read_sheet(url, sheet = 3)
write.csv(other_licensee, "data_indices/other_licensee.csv", row.names = F)

agreements <- read_sheet(url, sheet = 4)
write.csv(agreements, "data_indices/agreements.csv", row.names = F)

# Crops indexed into categories based on FAO https://www.fao.org/3/a0135e/A0135E10.htm#app3
crop <- read_sheet(url, sheet = 5)
write.csv(crop, "data_indices/crop.csv", row.names = F)


       