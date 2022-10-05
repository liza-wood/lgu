library(tidyverse)
library(lubridate)
library(stringr)
library(patentr)
setwd("~/Box/lgu/")
uni_names <- read.csv("data_indices/universities2.csv")
# https://cran.r-project.org/web/packages/patentr/vignettes/intro.html
# https://github.com/JYProjs/patentr

getOption('timeout')
options(timeout=1200)

# Download all of the patents for 1976 to 2020 -- did this in chunks
for(i in 2020){ 
  for(j in c(5:52)){ 
    get_bulk_patent_data(
      year = i,         
      week = j,                   
      output_file = paste0("data_raw/uspto/uspto_",i, "_", j, ".csv")
    )
  }
}

# Map functions
x = 2019
y = 1:52
pt <- data.frame()
t1 <- Sys.time()
pt <- map_df(x, ~ map_df(y, function(y) {
  df <- data.table::fread(paste0("data_raw/uspto/uspto_",.x, "_", y, ".csv"),
                          col.names = c("WKU", "Title", "App_Date",
                                        "Issue_Date", "Inventor",
                                        "Assignee", "ICL_Class",
                                        "References", "Claims"),
                          fill=TRUE) 
  df$App_Date <- ymd(as.character(df$App_Date))
  df$Issue_Date <- ymd(as.character(df$Issue_Date))
  pt <- rbind(pt, df)
} ))
t2 <- Sys.time()
t2-t1 

# Removed this from function because I don't want to filter before saving 
plant_words <- paste(c("\\b[Pp]lant", "[Gg]ermplasm", "[Vv]ariet\\w+", 
               "[Cc]ultivar", "[Ss]cion", "[Rr]ootstock",
               "[Tt]ree", "[Gg]rass"), collapse = "|")

uni_names_p <- tolower(paste(uni_names$uni_name, collapse = "|"))
# Running this or else I get error with weird characters
pt$Assignee <- enc2utf8(pt$Assignee)
pt <- pt %>% 
      mutate(university = case_when(
        str_detect(tolower(Assignee),"university|virginia tech|college") ~ T,
        T ~ F)) %>%
      mutate(university = case_when(
        Assignee == "University Patents, Inc." ~ F, 
        T ~ university)) %>% 
      mutate(lgu = case_when(
        str_detect(tolower(Assignee), uni_names_p) ~ T, 
        T ~ F)) %>%
      mutate(ars = case_when(
        str_detect(Assignee, "Agriculture Research Service") ~ T, 
        T ~ F)) %>%
      mutate(plant = case_when(
        str_detect(WKU, '^PP') | str_detect(Title, plant_words) ~ T, # Title, plant_words) | 
        T ~ F)) 

inventors <- unique(pt$Inventor[pt$lgu == T])
inventors_split <- unlist(str_split(inventors, ";"))
inventors_split <- str_replace_all(inventors_split, "\\.", "\\\\\\.")
inventors_split <- str_remove(inventors_split, "\\{")
inventors_split <- str_remove(inventors_split, "\\}")
inventors_split[1:8]
inventors_p <- paste(inventors_split, collapse = "|")

pt <- pt %>% 
  mutate(uni_inventor = case_when(
    str_detect(Inventor, inventors_p) ~ T,
    T ~ F))

table(pt$lgu)
table(pt$uni_inventor)
table(pt$plant)
table(pt$ars)

uni_plant_patents <- pt %>% 
  filter((lgu == T | uni_inventor == T | ars == T) & plant == T)

write.csv(uni_plant_patents, "data_raw/other_ip/uspto_uni_plant_19.csv", row.names = F)
