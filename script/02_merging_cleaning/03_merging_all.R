# Joining inventors, inventions, and awards with licenses and companies ----
# Read in licensing data 
licenses <- readRDS("~/Box/lgu/data_clean/df_final.RDS")
funds <- read.csv('data_clean/funds_df.csv')
innovations <- read.csv('data_clean/inventor_invention.csv')

# Filter the data to include only 4 states -- these are what we have semi-complete ata for right now and it will make our lives easier

sample_states <- c("California", "Minnesota", "Idaho", "Louisiana")

# Use filter to set conditions. Remember filtering conditions is about reducing the number of rows
licenses_sample <- filter(licenses, uni_state %in% sample_states) %>% 
  select(-inventor_last_name, -inventor_first_name) %>% 
  unique()


# We want to combine the variety licenses with the variety inventors
# What do these two data sets have in common (UNIQUE IDs) that we can join by
# This has each license, but with multiple inventors
combined <- left_join(licenses_sample, innovations)
# This add the funding of each inventor per license
combined2 <- left_join(combined, funds) 
# Now we are just giving a dollar value per each license but summing up all of the funding for all of its inventors (over all time -- THIS IS COARSE)
combined3 <- combined2 %>% 
  select(-c(number, inventor_first, inventor_last, inventor_first_1, inventor_ID)) %>% 
  group_by(uni_state, crop_name_common, variety_name, licensee, effective_date) %>% 
  mutate(funding = sum(funding, na.rm = T)) %>% 
  unique()

write.csv(combined3, 'data_clean/final_df.csv', row.names = F)
