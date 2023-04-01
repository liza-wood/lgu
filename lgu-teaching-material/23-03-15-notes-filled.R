
# Review: How to explore data ----
## 1. Load in libraries and data ----
library(tidyverse)

# Access files from Box: 
# Set working directory to Box
list.files("~/Box/lgu/data_clean")

# Read in licensing data 
licenses <- readRDS("~/Box/lgu/data_clean/df_final.RDS")

## 2. Take a high-level look at the data ----
# What do we know about these data? What kinds of variables do we have? Where is there missing data?
nrow(licenses)
colnames(licenses)
summary(licenses)
str(licenses)
head(licenses$crop_cat)

## 3. (Possibly) subset your data to what you want to work with ----
# Filter the data to include only 4 states -- these are what we have semi-complete ata for right now and it will make our lives easier

## New method:
# Make a vector of the state we want to keep
sample_states <- c("California", "Minnesota", "Idaho", "Louisiana")
# then filter the states we want using the %in% condition symbol
# (this is the same as saying uni_state == "California" | uni_state == "Idaho"... etc)

# Use filter to set conditions. Remember filtering conditions is about reducing the number of rows
licenses_sample <- filter(licenses, uni_state %in% sample_states)

# Another condition: Filter out ornamental plants (we could have done this with a pipe also)
licenses_sample <- filter(licenses_sample, ornamental == F)

# Use select to reduce the number of columns
colnames(licenses_sample)
licenses_sample <- licenses_sample %>% 
  select(uni_state, licensee, variety_name, agreement_type,
         effective_date, fao_class1, crop_cat, ornamental, 
         name, assets, yr_founded, global_sales_number, zipcode,
         company_location, uni_region, company_region, spatial_match)

# rename a badly-named column
colnames(licenses_sample)[9] <- 'company_name'

## 4. Summarize points of interest  ----

# How many licenses do we have from each state?
licenses_sample %>% 
  group_by(uni_state) %>% 
  count()

# Where are companies from?
licenses_sample %>% 
  select(company_name, company_location) %>% 
  unique() %>% 
  group_by(company_location) %>% 
  count() %>% 
  arrange(-n)

# What is the minimum and maximum global sales amounts of licensing companies
summary(licenses_sample$global_sales_number)

## 5. Repeat with invention data ----
# What are these data?
inventions <- read.csv("~/Box/lgu/data_clean/inventor_invention.csv")

# Take a look at the data:
nrow(inventions)
colnames(inventions)
## These are **unique** inventions from lgus (different from licneses because we could have the same invention licenses out multiple times)

# What kind of IP types do we have?
## Using table is the same as using group_by and count
table(inventions$ip_type)
## This doesn't add up to our nrow... are there missing data?
table(is.na(inventions$ip_type))

# How many inventions do we have for each state?
inventions %>% 
  group_by(uni_state) %>% 
  count()

# Filter to just our sample states
inventions_sample <- inventions %>% 
  filter(uni_state %in% sample_states)

# In our sample, do we have inventor names for all of them?
table(is.na(inventions_sample$inventor_last))

# isolate just the unique inventors
inventors_sample <- inventions_sample %>% 
  select(uni_state, inventor_last, inventor_first) %>% 
  unique() %>% 
  filter(!is.na(inventor_last))

## 6. Repeat with funding/awards data ----
## Note: These are already a sample from our selected states
awards_sample <- read.csv("data_clean/awards_selected_states.csv")

# Take a look
nrow(awards_sample)
head(awards_sample)

# Do we have any NAs for inventor_names in the awards?
table(is.na(awards_sample$inventor_last))

# join functions ----
## 1. Match the inventors to funding ----
##  What can we use to match money to inventions?

colnames(awards_sample)
colnames(inventors_sample)

# join functions will automatically join on columns with the same names, if they exist, so you can just try. But the default might not always be the best
# left_join keeps all the observations/rows from the data on the left, but drops observations on the right if they don't match anything on the left. So we will likely lose lots of funding awards if there is not a match in our inventions

# we will left join and it will default to shared names: uni_state, inventor_last, inventor_first
inventors_awards <- left_join(inventors_sample, awards_sample)
# warning is fine

## 2. Check your join! ----
# So, how many inventors were we able to match to at least one award?
inventors_awards %>% 
  filter(!is.na(Sponsor)) %>% 
  select(uni_state, inventor_last, inventor_first) %>% 
  unique() %>% 
  nrow()

# Who were they?
unique(inventors_awards[!is.na(inventors_awards$Sponsor), c('uni_state', 'inventor_last', 'inventor_first')])

# How many (and which!) were we not?
inventors_awards %>% 
  filter(is.na(Sponsor)) %>% 
  select(uni_state, inventor_last, inventor_first) %>% 
  unique() %>% 
  nrow()

unique(inventors_awards[is.na(inventors_awards$Sponsor), c('uni_state', 'inventor_last', 'inventor_first')])

# How do I know if this join was 'correct'? Do some tests
# Mistakes --
# Sheaffer
# Qualset
# Just not there --
# Wilson
# Brandt
# Thorpe

## 3. Customizing the 'by' argument ----

inventors_awards2 <- left_join(inventors_sample, awards_sample, 
                               by = c("uni_state", "inventor_last"))


inventors_awards2 %>% 
  filter(!is.na(Sponsor)) %>% 
  select(uni_state, inventor_last) %>% 
  unique() %>% 
  nrow()
# Went from 19 to 41 matches
# How might we check our work? What did the join function do with the 'inventor_first' columns?
colnames(inventors_awards2)
inventors_awards2 %>% 
  select(inventor_last, inventor_first.x, inventor_first.y) %>% 
  unique() %>% 
  arrange(inventor_last)

# May want to manually adjust spelling
# OR maybe just take the first letter of each name?
# NO need to know this, but it is preview of text manipulation
inventors_sample$inventor_first_1 <- stringr::str_extract(inventors_sample$inventor_first, "^\\w{1}")
awards_sample$inventor_first_1 <- stringr::str_extract(awards_sample$inventor_first, "^\\w{1}")

inventors_awards3 <- left_join(inventors_sample, awards_sample, 
                                by = c("uni_state", "inventor_last",
                                       "inventor_first_1"))

inventors_awards3 %>% 
  filter(!is.na(Sponsor)) %>% 
  select(uni_state, inventor_last) %>% 
  unique() %>% 
  nrow()
# Went from 19 to 41 to 37 matches

inventors_awards3 %>% 
  select(inventor_last, inventor_first_1, 
         inventor_first.x, inventor_first.y) %>% 
  unique() %>% 
  arrange(inventor_last)


# NEXT TIME join functions pt 2 ----
colnames(licenses_sample)
colnames(inventors_awards3)
# We want to combine the variety licenses with the variety inventors
# What do these two data sets have in common (UNIQUE IDs) that we can join by
combined <- left_join(licenses_sample, inventors_awards3)
nrow(licenses)
nrow(inventors_awards3)
nrow(combined)

#Wait, why?? 

# For every license an inventor has, it give us every award, so there's a TON of repetition... which is not what we want. And will require some manipulation
# Drafting below....
lubridate::mdy(awards_sample$StartDate)
# Assign as 
inventors_awards3$AwardAmt_n <- str_remove_all(inventors_awards3$AwardAmt,
                                                "\\$|\\s+|\\,|\\(|\\)")
inventors_awards3$AwardAmt_n <- as.numeric(inventors_awards3$AwardAmt_n)

inventor_award_grpd <- inventors_awards3 %>% 
  ungroup() %>% 
  group_by(uni_state, inventor_last, inventor_first_1) %>% 
  summarize(total = sum(AwardAmt_n))


