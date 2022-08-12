library(tidyverse)
library(lubridate)
library(stringr)
library(patentr)
setwd("~/Box/lgu/")
# https://cran.r-project.org/web/packages/patentr/vignettes/intro.html
# https://github.com/JYProjs/patentr

getOption('timeout')
options(timeout=600)

# Download all of the patents for 1976 to 2020
for(i in 1978:1990){
  for(j in 1:52){
    get_bulk_patent_data(
      year = i,         
      week = j,                   
      output_file = paste0("data_raw/uspto/uspto_",i, "_", j, ".csv")
    )
  }
}

pt <- data.frame()
for(i in 1976:1990){
  for(j in 1:52){
    df <- read.csv(paste0("data_raw/uspto/uspto_",i, "_", j, ".csv"),
                   col.names = c("WKU", "Title", "App_Date",
                                 "Issue_Date", "Inventor",
                                 "Assignee", "ICL_Class",
                                 "References", "Claims")) %>%
      mutate(App_Date = as_date(App_Date),
             Issue_Date = as_date(Issue_Date))
    pt <- rbind(pt, df)
  }
}
# Removed this from function because I don't want to filter before saving 
plant_words <- paste(c("\\b[Pp]lant", "[Gg]ermplasm", "[Vv]ariet\\w+", 
               "[Cc]ultivar", "[Ss]cion", "[Rr]ootstock",
               "[Tt]ree"), collapse = "|")

    
uni_plant_patents <- pt %>% 
      filter(str_detect(Assignee, "[Uu]niversity|[Vv]irginia Tech")) %>%
      filter(Assignee != "University Patents, Inc.") %>% 
      filter(str_detect(Title, plant_words) | str_detect(WKU, '^PP'))
    

write.csv(uni_plant_patents, "~/Desktop/uspto1990_2010.csv", row.names = F)
