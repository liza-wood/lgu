
library(tidyverse)
df <- readRDS(file = "data/lgu.rds")

# 1. ----- Get rid of columns you don't want with select ----
colnames(df)

# rename a column name
colnames(df)[16] <- 'company_name'
# Select: uni_state, licensee, crop_name_common, effective date, fao_class1, ornamental, global sales number, company region
df2 <- df %>% select(uni_state, licensee, crop_name_common,
                    effective_date, fao_class1, ornamental,
                    company_name, global_sales_number, company_region)

# 2. Filtering----
# Look at the top rows

# Take CA and TX
unique(df$uni_state)
df3 <- filter(df2, uni_state == "California" | 
                uni_state == "Texas")

# Get rid of ornamental crops
df4 <- filter(df3, ornamental == F)

# Filter out not having company names
df5 <- filter(df4, !is.na(company_name))

# 4. Do that together with pipes
df5b <- df2 %>% 
  filter((uni_state == "California" | uni_state == "Texas") &
           ornamental == F & !is.na(company_name))

# 5. Mutate --- 
summary(df5b$global_sales_number)
##  global sales number divided by 1 million
df6 <- df5b %>% 
  mutate(global_sales_mill = global_sales_number/1000000)

summary(df6$global_sales_mill)

## Group by and count ---
# count up the different crop classes
df %>% 
  group_by(uni_state) %>% 
  count() %>% 
  arrange(-n)

df6 %>% 
  group_by(crop_name_common) %>% 
  count() %>% 
  arrange(-n)

df6 %>% 
  group_by(uni_state, crop_name_common) %>% 
  count() %>% 
  arrange(-n)

df6 %>% 
  group_by(fao_class1) %>% 
  count() %>% 
  arrange(-n)


## average sales amount by state
mean(df6$global_sales_mill, na.rm = T)
df6 %>% 
  group_by(uni_state) %>% 
  summarize(mean(global_sales_mill, na.rm = T))

df6 %>% 
  group_by(fao_class1) %>% 
  summarize(mean(global_sales_mill, na.rm = T))


# BONUS: ifelse
## Big company, small company
mean(df6$global_sales_mill, na.rm = T)
df6$company_size <- ifelse(df6$global_sales_mill > 23199 ~ 'big', 'small')
## national or international with ifelse statements
table(df6$company_region)


