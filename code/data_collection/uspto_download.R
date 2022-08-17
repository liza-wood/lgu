library(tidyverse)
library(lubridate)
library(stringr)
library(patentr)
setwd("~/Box/lgu/")
uni_names <- read.csv("data_indices/universities.csv")
# https://cran.r-project.org/web/packages/patentr/vignettes/intro.html
# https://github.com/JYProjs/patentr

getOption('timeout')
options(timeout=1000)

# Download all of the patents for 1976 to 2020
for(i in 2009:2012){ 
  for(j in c(1:52)){ 
    get_bulk_patent_data(
      year = i,         
      week = j,                   
      output_file = paste0("data_raw/uspto/uspto_",i, "_", j, ".csv")
    )
  }
}

# How to speed up code? Variations on read.csv vs fread, and for loops vs. map/apply functions
pt <- data.frame()
for(i in 1976:2001){
  for(j in 1:52){
    df <- data.table::fread(paste0("data_raw/uspto/uspto_",i, "_", j, ".csv"),
                   col.names = c("WKU", "Title", "App_Date",
                                 "Issue_Date", "Inventor",
                                 "Assignee", "ICL_Class",
                                 "References", "Claims")) 
    pt <- rbind(pt, df)
  }
}
# Removed this from function because I don't want to filter before saving 
plant_words <- paste(c("\\b[Pp]lant", "[Gg]ermplasm", "[Vv]ariet\\w+", 
               "[Cc]ultivar", "[Ss]cion", "[Rr]ootstock",
               "[Tt]ree", "[Gg]rass"), collapse = "|")

uni_names_p <- tolower(paste(uni_names$university_name, collapse = "|"))
pt <- pt %>% 
      mutate(university = case_when(
        str_detect(tolower(Assignee),"university|virginia tech|college") ~ T,
        T ~ F)) %>%
      mutate(university = case_when(
        Assignee == "University Patents, Inc." ~ F, T ~ university)) %>% 
      mutate(lgu = case_when(
        str_detect(tolower(Assignee), uni_names_p) ~ T, 
        T ~ F)) %>%
      mutate(plant = case_when(
        str_detect(Title, plant_words) | str_detect(WKU, '^PP') ~ T,
        T ~ F))

table(pt$lgu)
table(pt$plant)

uni_plant_patents <- pt %>% 
  filter(lgu == T & plant == T)

write.csv(uni_plant_patents, "data_raw/other_ip/uspto_uni_plant.csv", row.names = F)
