library(tidyverse)
library(lubridate)
df <- read.csv("~/Desktop/uspto1990_2010.csv")
uni_names <- read.csv("data_indices/universities.csv")

# Choose only LGUs
lgu_pattern <- paste(uni_names$university_name, collapse = "|")
for(i in 1:nrow(df)){
  df$university[i] <- str_extract(df$Assignee[i], lgu_pattern)
}

df <- filter(df, !is.na(university))
df <- filter(df, !(str_detect(df$Title, "[Mm]ethod|[Aa]pparatus")) &
               !(str_detect(df$Claims, "1\\. A method")))

# Plant patents definitely stay, but how to get utility patents
pps <- filter(df, str_detect(df$WKU, "^PP"))
summary(nchar(pps$Title))
table(year(pps$Issue_Date))

other <- filter(df, !(str_detect(df$WKU, "^PP")))
summary(nchar(other$Title))
other <- filter(other, nchar(other$Title) < 75)
table(year(other$Issue_Date))
