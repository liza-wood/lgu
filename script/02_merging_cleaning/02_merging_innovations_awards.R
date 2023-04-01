## 5. Repeat with invention data ----
# What are these data?
inventions <- read.csv("~/Box/lgu/data_clean/inventor_invention.csv")

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
sample_states <- c("California", "Minnesota", "Idaho", "Louisiana")
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
#inventors_awards2 <- left_join(inventors_sample, awards_sample, 
#                               by = c("uni_state", "inventor_last"))
#
#
#inventors_awards2 %>% 
#  filter(!is.na(Sponsor)) %>% 
#  select(uni_state, inventor_last) %>% 
#  unique() %>% 
#  nrow()
## Went from 19 to 41 matches
## How might we check our work? What did the join function do with the 'inventor_first' #columns?
#colnames(inventors_awards2)
#inventors_awards2 %>% 
#  select(inventor_last, inventor_first.x, inventor_first.y) %>% 
#  unique() %>% 
#  arrange(inventor_last)

# May want to manually adjust spelling
# OR maybe just take the first letter of each name?
# NO need to know this, but it is preview of text manipulation
inventors_sample$inventor_first_1 <- stringr::str_extract(inventors_sample$inventor_first, "^\\w{1}")

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
  select(uni_state, inventor_last, inventor_first_1, 
         inventor_first.x, inventor_first.y) %>% 
  unique() %>% 
  arrange(inventor_last)


# Invetor and award to invention ----
colnames(inventors_awards3)
inventors_awards_sample <- inventors_awards3

library(lubridate)

summary(inventors_awards_sample$end_year)

inventors_awards_sample_allyrs <- inventors_awards_sample %>% 
  group_by(uni_state, inventor_last, inventor_first_1) %>% 
  summarize(funding = sum(amt)) 

write.csv(inventors_awards_sample_allyrs, 'data_clean/funds_df.csv', row.names = F)

inventors_awards_sample_cat <- inventors_awards_sample %>% 
  mutate(award_date_range = case_when(
    end_year %in% 2000:2009 ~ "2000-2009",
    end_year %in% 2010:2019 ~ "2010-2019",
    end_year >= 2020 ~ "After 2019"
  )) %>% 
  filter(!is.na(award_date_range)) %>% 
  group_by(uni_state, inventor_last, inventor_first_1, award_date_range) %>% 
  summarize(funding = sum(amt)) %>% 
  mutate(date_range = award_date_range) 

# Join inventors and inventions ----
colnames(inventions_sample)
inventions_sample <- inventions_sample %>% 
  mutate(inventor_first_1 = str_extract(inventor_first, "^\\w{1}")) 

inventions_sample_cat <- inventions_sample %>% 
  filter(!is.na(inventor_last)) %>% 
  mutate(invention_date_range = case_when(
    year_filed < 2000 ~ "Before 2000",
    year_filed %in% 2000:2009 ~ "2000-2009",
    year_filed %in% 2010:2019 ~ "2010-2019",
    year_filed >= 2020 ~ "After 2019"
  )) %>% 
  filter(!is.na(invention_date_range)) %>% 
  mutate(date_range = invention_date_range) 

inventors_inventions <- left_join(inventions_sample, inventors_awards_sample_allyrs)

inventors_inventions_cat <- left_join(inventions_sample_cat, inventors_awards_sample_cat)


