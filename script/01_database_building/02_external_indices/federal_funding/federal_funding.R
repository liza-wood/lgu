library(ggplot2)
library(lubridate)
library(dplyr)
# Set working director to Box
setwd("~/Box/lgu")

# CLEAN PUBLIC FUNDING ----
funding <- read.csv("data_raw/federal_funding/breeding_awards_combined.csv")
funding_by_yr_state <- funding %>% 
  group_by(state, year) %>% 
  summarize(funding_amt = sum(Award.Dollars)) %>% 
  rename(funding_yr = year) %>% 
  ungroup()

fund_df <- funding %>% 
  mutate(funding_yr_grpd = case_when(
    year %in% c(2002:2005) ~ "2002-2005",
    year %in% c(2006:2009) ~ "2006-2009",
    year %in% c(2010:2013) ~ "2010-2013",
    year %in% c(2014:2017) ~ "2014-2017",
    year %in% c(2018:2022) ~ "2018-2022")) %>% 
  group_by(state, funding_yr_grpd) %>% 
  mutate(funding_amt_grpd = sum(Award.Dollars)) %>% 
  select(state, funding_yr_grpd, funding_amt_grpd) %>% 
  unique() %>% 
  rename(uni_state = state)

# CLEAN LICENSING ----
df <- read.csv("data_clean/merged_df.csv") %>% 
  mutate(license_yr = year(effective_date)) %>% 
  filter(license_yr > 1999 & license_yr < 2021) %>% 
  mutate(license_yr_grpd = case_when(
    license_yr %in% (2002:2005) ~ "2002-2005",
    license_yr %in% (2006:2009) ~ "2006-2009",
    license_yr %in% (2010:2013) ~ "2010-2013",
    license_yr %in% (2014:2017) ~ "2014-2017",
    license_yr %in% (2018:2022) ~ "2018-2022")) %>% 
  mutate(funding_yr_grpd = case_when(
    (license_yr - 5) %in% (2002:2005) ~ "2002-2005",
    (license_yr - 5) %in% (2006:2009) ~ "2006-2009",
    (license_yr - 5) %in% (2010:2013) ~ "2010-2013",
    (license_yr - 5) %in% (2014:2017) ~ "2014-2017",
    (license_yr - 5) %in% (2018:2022) ~ "2018-2022")) %>% 
  select(-state)# %>% 
  #left_join(fund_df)

intersect(colnames(df), colnames(fund_df))
df <- merge(df, fund_df, all.x = T)

