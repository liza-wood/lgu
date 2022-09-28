library(tidyverse)
library(lubridate)
setwd("~/Box/lgu")
df76 <- read.csv("data_raw/other_ip/uspto_uni_plant_76_95.csv")
df96 <- read.csv("data_raw/other_ip/uspto_uni_plant_96_00.csv")
df01 <- read.csv("data_raw/other_ip/uspto_uni_plant_01_05.csv")
df06 <- read.csv("data_raw/other_ip/uspto_uni_plant_06_10.csv")
df11 <- read.csv("data_raw/other_ip/uspto_uni_plant_11_15.csv")
df16 <- read.csv("data_raw/other_ip/uspto_uni_plant_16_18.csv")
df <- rbind(df76, df96, df01, df06, df11, df16)
uni_names <- read.csv("data_indices/universities2.csv")

# Choose only LGUs
lgu_pattern <- paste(uni_names$uni_name, collapse = "|")
mistakes <- paste(c("University of Minnestoa", "University of Akransas", 
                    "University of Akranas", "Texas A \\& M University",
                    "University of Tennesse", "University of Ga", 
                    "University of Georgiea", "North Carollna State University",
                    "Rutger"), collapse = "|")
lgu_pattern <- paste(lgu_pattern, mistakes, sep = "|")
lgu_pattern <- str_replace_all(lgu_pattern, "\\bof\\b", "[Oo]f")
lgu_pattern <- tolower(lgu_pattern)

for(i in 1:nrow(df)){
  df$university[i] <- str_extract(tolower(df$Assignee[i]), lgu_pattern)
}

#df <- filter(df, !is.na(university))
df <- filter(df, !(str_detect(df$Title, "[Mm]ethod|[Aa]pparatus|[Pp]rocess")) &
               !(str_detect(df$Claims, "1\\. A method")))

### TRY TO ID CROPS 
write.csv(df, "data_clean/uspto_lgus.csv", row.names = F)

# Plant patents definitely stay, but how to get utility patents
pps <- filter(df, str_detect(df$WKU, "^PP"))
summary(nchar(pps$Title))
table(year(pps$Issue_Date))

other <- filter(df, !(str_detect(df$WKU, "^PP")))
summary(nchar(other$Title))
other <- filter(other, nchar(other$Title) < 75)
table(year(other$Issue_Date))
