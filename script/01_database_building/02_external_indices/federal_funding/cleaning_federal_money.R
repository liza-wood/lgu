library(stringr)
library(ggplot2)
library(dplyr)
library(lubridate)
# Set working director to Box
setwd("~/Box/lgu")

# Reading in the recent awards on NIFA funds for projects 
# https://portal.nifa.usda.gov/lmd4/recent_awards

# Plant breed in the title
df1 <- read.csv("data_raw/federal_funding/plantbreed.csv")

# Breed in title
# remove things that they fish and animal
rmv <- c("[Ff]ish", "[Aa]nimal", "[Pp]ork", "[Mm]ilk", "[Cc]hicken",
         "[Bb]ovine", "[Cc]attle", "[Oo]ysters", "[Aa]quaculture") 
rmv <- paste(rmv, collapse = "|")
df2 <- read.csv("data_raw/federal_funding/breeding.csv") %>% 
  filter(!(str_detect(Grant.Title, rmv))) %>% 
  filter(!(str_detect(Program.Name, rmv)))

df <- full_join(df1, df2) %>% 
  filter(Award.Dollars > 0) 
# 457 breeding grants

df <- unique(df)

colleges <- str_remove(str_extract(df$Grantee.Name, "SAES.*"), "SAES \\- ")
colleges <- unique(colleges[!is.na(colleges)])
colleges <- paste(colleges, collapse = "|")

df_lgu <- df %>% 
  filter(str_detect(Grantee.Name, colleges))

# Where does Cornell fall?
total_lgu_money <- sum(df_lgu$Award.Dollars)
total_lgu_awards <- nrow(df_lgu)
total_cornell_money <- sum(df_lgu$Award.Dollars[str_detect(df_lgu$Grantee.Name, 
                                                           "CORNELL")])
total_cornell_awards <- nrow(df_lgu[str_detect(df_lgu$Grantee.Name, "CORNELL"),])

100*(total_cornell_awards/total_lgu_awards)
100*(total_cornell_money/total_lgu_money)

total_ca_money <- sum(df_lgu$Award.Dollars[str_detect(df_lgu$Grantee.Name,                                              "CALIFORNIA")])
total_ca_awards <- nrow(df_lgu[str_detect(df_lgu$Grantee.Name, "CALIFORNIA"),])
100*(total_ca_awards/total_lgu_awards)
100*(total_ca_money/total_lgu_money)

total_ia_money <- sum(df_lgu$Award.Dollars[str_detect(df_lgu$Grantee.Name,                                              "IOWA")])
total_ia_awards <- nrow(df_lgu[str_detect(df_lgu$Grantee.Name, "IOWA"),])
100*(total_ia_awards/total_lgu_awards)
100*(total_ia_money/total_lgu_money)

total_was_money <- sum(df_lgu$Award.Dollars[str_detect(df_lgu$Grantee.Name,                                              "WASHINGTON")])
total_was_awards <- nrow(df_lgu[str_detect(df_lgu$Grantee.Name, "WASHINGTON"),])
100*(total_was_awards/total_lgu_awards)
100*(total_was_money/total_lgu_money)

total_fl_money <- sum(df_lgu$Award.Dollars[str_detect(df_lgu$Grantee.Name,                                              "FLORIDA")])
total_fl_awards <- nrow(df_lgu[str_detect(df_lgu$Grantee.Name, "FLORIDA"),])
100*(total_fl_awards/total_lgu_awards)
100*(total_fl_money/total_lgu_money)

nrow(df_lgu)/nrow(df) # 76% of plant breeding grants
breeding_amt <- sum(df$Award.Dollars)
lgu_breeding_amt <- sum(df_lgu$Award.Dollars)
lgu_breeding_amt/breeding_amt

df_lgu$year <- year(df_lgu$Award.Date)
df_lgu$state <- tools::toTitleCase(tolower(df_lgu$State.Name))
write.csv(df_lgu, "data_raw/federal_funding/breeding_awards_combined.csv")

all <- read.csv("data_raw/federal_funding/allawards.csv") %>% 
  filter(Award.Dollars > 0) %>% 
  mutate(breeding = case_when(
    Proposal.Number %in% df$Proposal.Number ~ T,
    T ~ F
  )) 

all <- unique(all)

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

