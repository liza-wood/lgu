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

df$agreement_bi <- ifelse(df$agreement_type == "Exclusive license", 1,
                          ifelse(df$agreement_type == "Non-exclusive license", 0, NA))
df$funding_amt_grpd <- ifelse(!is.na(df$funding_yr_grpd) & is.na(df$funding_amt_grpd), 
                              0 , df$funding_amt_grpd)
df$funding_amt_grpd_log <- log(df$funding_amt_grpd)
df$funding_amt_grpd_log <- ifelse(df$funding_amt_grpd_log == "-Inf", 0, df$funding_amt_grpd_log)
df$local_sales_number <- ifelse(df$licensee_type == "other", 0, df$local_sales_number)
df$local_sales_number_log <- log(df$local_sales_number)
df$local_sales_number_log <- ifelse(df$local_sales_number_log == "-Inf", 0, df$local_sales_number_log)
df$pres <- ifelse(df$license_yr == 2000, "Clinton",
           ifelse(df$license_yr %in% 2001:2009, "Bush",
           ifelse(df$license_yr %in% 2010:2016, "Obama",
           ifelse(df$license_yr %in% 2017:2021, "Trump", "Other"))))
df$inregion <- ifelse(df$spatial_match == "Within region", 1,
               ifelse(df$spatial_match == "Outside region", 0, NA))


df$company_size <- factor(df$company_size, levels = c("Large ($10M+)",
                                                      "Medium ($1M - $10M)",
                                                      "Small ($220K - $1M)",
                                                      "Very small (<$200K)",
                                                      "Non-company", 
                                                      "Unknown"))

saveRDS(df, "data_clean/df_final.RDS")

