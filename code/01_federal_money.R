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

# Breed in title
# remove things that they fish and animal
rmv <- c("[Ff]ish", "[Aa]nimal", "[Pp]ork", "[Mm]ilk", "[Cc]hicken",
         "[Bb]ovine", "[Cc]attle", "[Oo]ysters", "[Aa]quaculture") 
rmv <- paste(rmv, collapse = "|")
df2 <- read.csv("lgu/breeding.csv") %>% 
  filter(!(str_detect(Grant.Title, rmv))) %>% 
  filter(!(str_detect(Program.Name, rmv)))

df <- full_join(df1, df2) %>% 
  filter(Award.Dollars > 0)
# 457 breeding grants

colleges <- str_remove(str_extract(df$Grantee.Name, "SAES.*"), "SAES \\- ")
colleges <- unique(colleges[!is.na(colleges)])
colleges <- paste(colleges, collapse = "|")

df_lgu <- df %>% 
  filter(str_detect(Grantee.Name, colleges))

nrow(df_lgu)/nrow(df) # 76% of plant breeding grants
breeding_amt <- sum(df$Award.Dollars)
lgu_breeding_amt <- sum(df_lgu$Award.Dollars)
lgu_breeding_amt/breeding_amt

df_lgu$year <- year(df_lgu$Award.Date)
df_lgu$state <- tools::toTitleCase(tolower(df_lgu$State.Name))
write.csv(df_lgu, "lgu/breeding_awards_combined.csv")

all <- read.csv("lgu/allawards.csv") %>% 
  filter(Award.Dollars > 0) %>% 
  mutate(breeding = case_when(
    Proposal.Number %in% df$Proposal.Number ~ T,
    T ~ F
  )) 

all_lgu <- all %>% 
  filter(str_detect(Grantee.Name, colleges))

nrow(all_lgu)/nrow(all) #56% of all federal grants
amt <- sum(all$Award.Dollars)
lgu_amt <- sum(all_lgu$Award.Dollars)
lgu_amt/amt # 65% of dollars


all$year <- year(all$Award.Date)
all <- filter(all, year > 2001 & year < 2022)
df_lgu$year <- year(df_lgu$Award.Date)
df_lgu <- filter(df_lgu, year > 2001 & year < 2022)

ggplot(df_lgu, aes(x = year, y = Award.Dollars)) +
  geom_bar(stat = "identity")

table(df_lgu$State.Name)

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
  mutate(percent_breeding = round(100*(yrly_total_breeding/yrly_total),2)) %>% 
  unique()

ggplot(yr_sum, aes(x = year, y = percent_breeding)) +
  geom_bar(stat = "identity")

