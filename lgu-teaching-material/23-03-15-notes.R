# Review: How to explore data ----
## 1. Load in libraries and data ----
library(tidyverse)

# Access files from Box: 
# Set working directory to Box
list.files('~/Box/lgu')

# Read in licensing data 
licenses <- readRDS("~/Box/lgu/data_clean/df_final.RDS")

## 2. Take a high-level look at the data ----
# What do we know about these data? What kinds of variables do we have? Where is there missing data?
nrow(licenses)
colnames(licenses)
summary(licenses)
str(licenses)

## 3. (Possibly) subset your data to what you want to work with ----
# Filter the data to include only 4 states -- these are what we have semi-complete ata for right now and it will make our lives easier

## New method:
# Make a vector of the state we want to keep
sample_states <- c("California", "Minnesota", "Idaho", "Louisiana")
# then filter the states we want using the %in% condition symbol
# (this is the same as saying uni_state == "California" | uni_state == "Idaho"... etc)

# Use filter to set conditions. Remember filtering conditions is about reducing the number of rows
licenses_sample <- licenses %>% 
  filter(uni_state %in% sample_states) %>% 
  filter(ornamental == F)
nrow(licenses_sample)

# Another condition: Filter out ornamental plants (we could have done this with a pipe also)


# Use select to reduce the number of columns
colnames(licenses_sample)
licenses_sample <- licenses_sample %>% 
  select(uni_state, licensee, variety_name, agreement_type,
         effective_date, fao_class1, crop_cat, 
         name, assets, yr_founded, global_sales_number,
         zipcode,
         company_location, uni_region, company_region, spatial_match)

# replace/rename a badly-named column
head(licenses_sample$name)
colnames(licenses_sample)[8] <- "company_name"
colnames(licenses_sample)
licenses_sample$company_name2 <- licenses_sample$company_name
colnames(licenses_sample)

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

## 5. Repeat with invention data ----
# What are these data?
inventions <- read.csv("~/Box/lgu/data_clean/inventor_invention.csv")

# Take a look at the data:
colnames(inventions)

## These are **unique** inventions from lgus (different from licneses because we could have the same invention licenses out multiple times)

# What kind of IP types do we have?
## Using table is the same as using group_by and count

## This doesn't add up to our nrow... are there missing data?


# How many inventions do we have for each state?



# Filter to just our sample states
inventions_sample <- inventions %>% 
  filter(uni_state %in% sample_states)

# In our sample, do we have inventor names for all of them?


# isolate just the unique inventors
inventors_sample <- inventions_sample %>% 
  select(uni_state, inventor_last, inventor_first) %>% 
  unique() %>% 
  filter(!is.na(inventor_last))
head(inventors_sample)

## 6. Repeat with funding/awards data ----
## Note: These are already a sample from our selected states
getwd()
awards_sample <- read.csv("~/Box/lgu/data_clean/awards_selected_states.csv")

# Take a look
colnames(awards_sample)


# Do we have any NAs for inventor_names in the awards?


# join functions ----
## 1. Match the inventors to funding ----
##  What can we use to match money to inventions?

colnames(awards_sample)
colnames(inventions_sample)

# join functions will automatically join on columns with the same names, if they exist, so you can just try. But the default might not always be the best
# left_join keeps all the observations/rows from the data on the left, but drops observations on the right if they don't match anything on the left. So we will likely lose lots of funding awards if there is not a match in our inventions

# we will left join and it will default to shared names: uni_state, inventor_last, inventor_first
inventors_awards <- left_join(inventors_sample, awards_sample)
# warning is fine

## 2. Check your join! ----
# So, how many inventors were we able to match to at least one award?
nrow(awards_sample)
nrow(inventors_sample)
nrow(inventors_awards)

inventors_awards %>% 
  filter(!is.na(Sponsor)) %>% 
  select(uni_state, inventor_last, inventor_first) %>% 
  unique() %>% 
  nrow()

# Who were they?
# printing out the invntors who don't have sponsors
unique(inventors_awards[is.na(inventors_awards$Sponsor), c('uni_state', 'inventor_last', 'inventor_first')])

# How many (and which!) were we not?


# How do I know if this join was 'correct'? Do some tests
# Mistakes --
# Sheaffer
# Qualset
# Just not there --
# Wilson
# Brandt
# Thorpe

## 3. Customizing the 'by' argument ----




# Went from 19 to 41 matches
# How might we check our work? What did the join function do with the 'inventor_first' columns?


# May want to manually adjust spelling
# OR maybe just take the first letter of each name?
# NO need to know this, but it is preview of text manipulation

# Went from 19 to 41 to 37 matches



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

# ggplot ---- 

## Overview ----
# 3 components of a ggplot:
## 1. data, 2. aesthetic mapping, 3. geometry

# First try to image what it is you want to visualize: picture it in your head and work backwards. What kinds of data so the axes? What kinds of geometries work best for those data? Are there additional mappings (color, shape, etc) that you will want to add to the plot?

## scatterplots (geom_point) ----
# Example: A scatter plot (points) that show the relationship between year founded and global sales
# Let's work with licenses

ggplot(licenses, 
       aes(x = yr_founded, y = global_sales_number)) +
  geom_point()

# What if I wanted the dots to be blue? 
ggplot(licenses, 
       aes(x = yr_founded, y = global_sales_number)) +
  geom_point(color = "darkblue")

# What if I wanted company_region to be a color
ggplot(licenses, 
       aes(x = yr_founded, y = global_sales_number)) +
  geom_point(aes(color = company_region))

## barplots (geom_bar or geom_col) ----
# Example: A bae that show the counts of licenses by state
# only needs an x argument
ggplot(licenses, aes(x = uni_state)) +
  geom_bar()

# Another way? You can pipe data into plots, which can also help you order bars
licenses %>% 
  group_by(uni_state) %>% 
  count() %>% 
  arrange(-n) %>% 
  ggplot(aes(x = factor(uni_state, levels = .$uni_state), y = n)) +
  geom_col()

# Easier to read?
licenses %>% 
  group_by(uni_state) %>% 
  count() %>% 
  arrange(-n) %>% 
  ggplot(aes(x = factor(uni_state, levels = .$uni_state), y = n)) +
  geom_col() +
  coord_flip()

## boxplots (geom_boxplot) ----
# Boxplots are good at showing distributions
# Example: The range of company size for each 'fao' crop categorization, or crop_cat
ggplot(licenses, 
       aes(x = fao_class1, y = global_sales_number)) +
  geom_boxplot()

# Apply the same logics to put them in an order

ggplot(licenses, 
       aes(x = fao_class1, y = global_sales_number)) +
  geom_boxplot() +
  coord_flip()

# filter out the NAS, non_food, medicinal, sugar and spice crops
remove_these_crops <- c("sugar_crops", "spice_crops",
                        "non_food", "medicinal")
licenses %>% 
  filter(!is.na(fao_class1)) %>% 
  filter(!(fao_class1 %in% remove_these_crops)) %>% 
  mutate(fao_class1_f = factor(fao_class1, levels = 
                                 c("cereals", "oilseeds",
                                   "root_tuber", "grasses_fodder",
                                   "veg_melon", "fruits_nuts",
                                   "leguminous"))) %>% 
  ggplot(aes(x = fao_class1_f, y = global_sales_number)) +
  geom_boxplot() +
  coord_flip()
