
inventors <- read.csv("~/Box/lgu/data_clean/inventor_index.csv")

# Filter to just our sample states
sample_states <- c("California", "Montana", "Minnesota", "Idaho", 
                   "Louisiana", "Michigan")
inventors_sample <- inventors %>% 
  filter(uni_state %in% sample_states) 

# In our sample, do we have inventor names for all of them?
table(is.na(inventors_sample$inventor_last))

awards_sample <- read.csv("~/Box/lgu/data_clean/awards_selected_states.csv")

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

inventors_awards2 <- left_join(inventors_sample, awards_sample, 
                               by = c("uni_state", "inventor_last",
                                      "inventor_first_1"))

inventors_awards2 %>% 
  filter(!is.na(Sponsor)) %>% 
  select(uni_state, inventor_last) %>% 
  unique() %>% 
  nrow()
# Went from 25 to 53 matches

inventors_awards2 %>% 
  select(uni_state, inventor_last, inventor_first_1, 
         inventor_first.x, inventor_first.y) %>% 
  unique() %>% 
  arrange(inventor_last)


# Invetor and award to invention ----
colnames(inventors_awards2)
inventors_awards_sample <- inventors_awards2

library(lubridate)

summary(inventors_awards_sample$end_year)

inventors_awards_sample_allyrs <- inventors_awards_sample %>% 
  group_by(uni_state, inventor_last, inventor_first_1) %>% 
  summarize(funding = sum(amt)) 

write.csv(inventors_awards_sample_allyrs, '~/Box/lgu/data_clean/funds_df.csv', row.names = F)

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


