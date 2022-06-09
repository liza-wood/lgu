library(stringr)
library(ggplot2)
library(dplyr)
library(lubridate)
# Set working director to Box
setwd("~/Box/")

# Reading in the recent awards on NIFA funds for projects 
# https://portal.nifa.usda.gov/lmd4/recent_awards

# Plant breed in the title
df1 <- read.csv("lgu/plantbreed.csv")

# Breed in tite
# remove things that they fish and animal
rmv <- c("[Ff]ish", "[Aa]nimal", "[Pp]ork", "[Mm]ilk", "[Cc]hicken",
         "[Bb]ovine", "[Cc]attle", "[Oo]ysters", "[Aa]quaculture") 
rmv <- paste(rmv, collapse = "|")
df2 <- read.csv("data/breeding.csv") %>% 
  filter(!(str_detect(Grant.Title, rmv))) %>% 
  filter(!(str_detect(Program.Name, rmv)))

df <- full_join(df1, df2) %>% 
  filter(Award.Dollars > 0)

colleges <- str_remove(str_extract(df$Grantee.Name, "SAES.*"), "SAES \\- ")
colleges <- unique(colleges[!is.na(colleges)])
colleges <- paste(colleges, collapse = "|")

df <- df %>% 
  filter(str_detect(Grantee.Name, colleges))

df$year <- year(df$Award.Date)
df$state <- tools::toTitleCase(tolower(df$State.Name))
write.csv(df, "lgu/breeding_awards_combined.csv")

all <- read.csv("lgu/allawards.csv") %>% 
  filter(Award.Dollars > 0) %>% 
  mutate(breeding = case_when(
    Proposal.Number %in% df$Proposal.Number ~ T,
    T ~ F
  )) %>% 


all$year <- year(all$Award.Date)
all <- filter(all, year > 2001 & year < 2022)
df$year <- year(df$Award.Date)
df <- filter(df, year > 2001 & year < 2022)

ggplot(df, aes(x = year, y = Award.Dollars)) +
  geom_bar(stat = "identity")

table(df$State.Name)

ggplot(all, aes(x = year, y = Award.Dollars, fill = breeding)) +
  geom_bar(stat = "identity")

yr_sum <- all %>% 
  group_by(year, breeding) %>% 
  summarize(yrly_total_breeding = sum(Award.Dollars)) %>% 
  filter(breeding == T) %>% 
  select(-breeding) %>% 
  ungroup()

yr_sum <- all %>% 
  group_by(year) %>% 
  summarize(yrly_total = sum(Award.Dollars)) %>% 
  left_join(yr_sum) %>% 
  mutate(percent_breeding = yrly_total_breeding/yrly_total) %>% 
  unique()

ggplot(yr_sum, aes(x = year, y = percent_breeding)) +
  geom_bar(stat = "identity")

