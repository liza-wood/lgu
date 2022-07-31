library(tidyverse)
library(lubridate)
setwd("~/Box/lgu")
df <- read.csv("data_raw/other_ip/uspto.csv")
uni_names <- read.csv("data_indices/universities.csv")

# Choose only LGUs
lgu_pattern <- paste(uni_names$university_name, collapse = "|")
mistakes <- paste(c("University of Minnestoa", "University of Akransas", 
                    "University of Akranas", "Texas A \\& M University",
                    "University of Tennesse", "University of Ga", 
                    "University of Georgiea", "North Carollna State University",
                    "Rutger"), collapse = "|")
lgu_pattern <- paste(lgu_pattern, mistakes, sep = "|")
lgu_pattern <- str_replace_all(lgu_pattern, "\\bof\\b", "[Oo]f")

for(i in 1:nrow(df)){
  df$university[i] <- str_extract(df$Assignee[i], lgu_pattern)
}

df <- filter(df, !is.na(university))
df <- filter(df, !(str_detect(df$Title, "[Mm]ethod|[Aa]pparatus|^Process")) &
               !(str_detect(df$Claims, "1\\. A method")))

write.csv(df, "data_clean/uspto_lgus.csv", row.names = F)

# Plant patents definitely stay, but how to get utility patents
pps <- filter(df, str_detect(df$WKU, "^PP"))
summary(nchar(pps$Title))
table(year(pps$Issue_Date))

other <- filter(df, !(str_detect(df$WKU, "^PP")))
summary(nchar(other$Title))
other <- filter(other, nchar(other$Title) < 75)
table(year(other$Issue_Date))
