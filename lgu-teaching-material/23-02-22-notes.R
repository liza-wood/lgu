df <- readRDS(file = "data/lgu.rds")

library(tidyverse)
test <- df %>% 
 filter(uni_state == "California" , uni_state == "Iowa")
# Summary
summary(df)

# What are these columns, actually?
# uni_state = state of university
# licensee = the company who buys the plant
# crop_name_common
# crop_name_scientific
# variety_name
# agreement_type = exclusive, non-exclusive, etc.
# effective_date = date of purchase/license
# invention_id = university-specific IDs
# ..

# 1. ----- Get rid of columns you don't want with select ----
## What did


# 2. ----
# Do just Davis

# 3. ---
#Filter out not having company names

# 4. Do that together with pipes

# 5. Mutate --- ## Create a new column
##  local sales number divided by 1 million

## Group by and summarize ---
## average sales amount by state


# Count: How many licenses per state? Per crop (FAO)? Per state x crop

# What companies are the oldest?



# BONUS: ifelse
## national or international with ifelse statements