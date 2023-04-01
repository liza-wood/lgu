# Access files from Box: 
## 1. find the filepath to Box

"~/Box/"

df <- readRDS(file = "data/lgu.rds")

library(dplyr)
library(stringr)
# Set working directory to Box
setwd("~/Box/lgu")
# Read in Data
df <- readRDS("~/Box/lgu/data_clean/df_final.RDS")
df$global_sales_number
## average sales amount by state
df <- df %>% 
  mutate(global_sales_mill = global_sales_number/1000000)
mean(df$global_sales_mill, na.rm = T)

df %>% 
  group_by(uni_state) %>% 
  summarize(mean(global_sales_mill, na.rm = T))

df %>% 
  group_by(fao_class1) %>% 
  summarize(mean(global_sales_mill, na.rm = T))

## Big company, small company
mean(df6$global_sales_mill, na.rm = T)
df6$company_size <- ifelse(df6$global_sales_mill > 23199 ~ 'big', 'small')
## national or international with ifelse statements
table(df6$company_region)

# join
df_ca <- filter(df, uni_state == "California")
invention <- read.csv("data_clean/inventor_invention.csv")
invention_ca <- filter(invention, uni_state == "California")
awards_ca <- read.csv("data_clean/awards_ca.csv")

colnames(df_ca)
colnames(invention_ca)
df_ca <- select(df_ca, -c(inventor_first_name, inventor_last_name))
df_merged_ca <- left_join(df_ca, invention_ca)

colnames(awards_ca)
colnames(df_merged_ca)
df_merged2 <- left_join(df_merged_ca, awards)




license_nona <- filter(license, !is.na(licensee))
company %>% 
  group_by(name) %>% 
  count() %>% 
  arrange(-n)
df <- left_join(license_nona, company, by = c("licensee" = "name"))


df <- merge(license, company, by = c('licensee'), all.x=TRUE)