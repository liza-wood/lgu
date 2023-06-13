library(tidyverse)

# Merge all of the data sets ----
# Victoria's join: This is the inventors we have on our list as plant breeders, and all of the awards they've ever gotten. This is interesting on its own and hasn't been explore yet
# Helps us understand the trend in breeder funding: Where does the money come from?
inv_award <- read.csv("~/Box/lgu/data_clean/inventor_awards.csv")
# Adam's join: The relationship between licenses and the companies that buy those liceses
lic_co <- read.csv("~/Box/lgu/data_clean/licenses_joined.csv")
# Liza's join: Right now just the relationship between plants and inventors -- not that interesting until I get further details about the plants
inv_lic <- read.csv('~/Box/lgu/data_clean/inventor_invention.csv')

## Naming notes -----
# inv = inventor (could be pi but called awards recievers PIs which we didnt like)
# lic = license (/inventions)
# award = award
# co = company

# Filtering to pilot states ----
# Reducing the number of the license data frames down to only the 'pilot' data for the states that we currently have clean award data for (I have about twice as much data but it is not clean still)
# Look at the states repped in each data
table(inv_award$uni_state)
table(lic_co$uni_state)
table(inv_lic$uni_state)
# Reduce down the our limiting award pilot data
states <- unique(inv_award$uni_state)
states
# Does not match with who Adam identifies as "top", but it is a start!
lic_co <- filter(lic_co, uni_state %in% states)
inv_lic <- filter(inv_lic, uni_state %in% states)

# Join 1: Link awards to inventions ----
colnames(inv_award)
colnames(inv_lic)

colnames(inv_lic)[16] <- "inventor_lastname"
colnames(inv_lic)[18] <- "inventor_first_initial"

# Can think about eventually cleaning and indexing the sponsor list to match it to our licensee list:
sponsors <- str_remove_all(tools::toTitleCase(inv_award$Sponsor), '\\.|,')
sponsors <- sort(unique(sponsors))
sponsors

# This is super 'long' because it takes every award any inventor has ever gotten, and associates it with every plant they've ever bred. Cleaning and playing with time frames here is defnitely something of interest for the future
inventor_allmoney <- left_join(inv_lic, inv_award)


# Example
# How many varieties has Shaw developed?
shaw_varieties <- filter(inv_lic, inventor_lastname == "Shaw")
shaw_awards <- filter(inv_award, inventor_lastname == "Shaw")
shaw_varieties_licenses <- filter(lic_co, variety_name %in% shaw_varieties$variety_name)

# We could create a different temporal thing
table(inv_award$start_year)
table(inv_award$year_range)
table(inv_award$grant_period)
table(inv_lic$year_filed)

inv_award_dates <- inv_award %>% 
  # Victoria's year range was a little off so I am going to make one slightly more advanced
  mutate(Y1994 = ifelse(start_year == 1994, T, F),
         Y1995 = ifelse(start_year == 1995 | start_year < 1995 & end_year >= 1995, T, F),
         Y1996 = ifelse(start_year == 1996 | start_year < 1996 & end_year >= 1996, T, F),
         Y1997 = ifelse(start_year == 1997 | start_year < 1997 & end_year >= 1997, T, F),
         Y1998 = ifelse(start_year == 1998 | start_year < 1998 & end_year >= 1998, T, F),
         Y1999 = ifelse(start_year == 1999 | start_year < 1999 & end_year >= 1999, T, F),
         Y2000 = ifelse(start_year == 2000 | start_year < 2000 & end_year >= 2000, T, F),
         Y2001 = ifelse(start_year == 2001 | start_year < 2001 & end_year >= 2001, T, F),
         Y2002 = ifelse(start_year == 2002 | start_year < 2002 & end_year >= 2002, T, F),
         Y2003 = ifelse(start_year == 2003 | start_year < 2003 & end_year >= 2003, T, F),
         Y2004 = ifelse(start_year == 2004 | start_year < 2004 & end_year >= 2004, T, F),
         Y2005 = ifelse(start_year == 2005 | start_year < 2005 & end_year >= 2005, T, F),
         Y2006 = ifelse(start_year == 2006 | start_year < 2006 & end_year >= 2006, T, F),
         Y2007 = ifelse(start_year == 2007 | start_year < 2007 & end_year >= 2007, T, F),
         Y2008 = ifelse(start_year == 2008 | start_year < 2008 & end_year >= 2008, T, F),
         Y2009 = ifelse(start_year == 2009 | start_year < 2009 & end_year >= 2009, T, F),
         Y2010 = ifelse(start_year == 2010 | start_year < 2010 & end_year >= 2010, T, F),
         Y2011 = ifelse(start_year == 2011 | start_year < 2011 & end_year >= 2011, T, F),
         Y2012 = ifelse(start_year == 2012 | start_year < 2012 & end_year >= 2012, T, F),
         Y2013 = ifelse(start_year == 2013 | start_year < 2013 & end_year >= 2013, T, F),
         Y2014 = ifelse(start_year == 2014 | start_year < 2014 & end_year >= 2014, T, F),
         Y2015 = ifelse(start_year == 2015 | start_year < 2015 & end_year >= 2015, T, F),
         Y2016 = ifelse(start_year == 2016 | start_year < 2016 & end_year >= 2016, T, F),
         Y2017 = ifelse(start_year == 2017 | start_year < 2017 & end_year >= 2017, T, F),
         Y2018 = ifelse(start_year == 2018 | start_year < 2018 & end_year >= 2018, T, F),
         Y2019 = ifelse(start_year == 2019 | start_year < 2019 & end_year >= 2019, T, F),
         Y2020 = ifelse(start_year == 2020 | start_year < 2020 & end_year >= 2020, T, F),
         Y2021 = ifelse(start_year == 2021 | start_year < 2021 & end_year >= 2021, T, F),
         Y2022 = ifelse(start_year == 2022 | start_year < 2022 & end_year >= 2022, T, F)) %>% 
  mutate(match_to_year_filed = case_when(
    start_year <= 1999 ~ '2000-2004',
    start_year <= 2010 ~ '2005-2012',
    start_year <= 2020 ~ '2013-2020',
    T ~ NA)) %>% 
  group_by(uni_state, inventor_lastname, inventor_first_initial, match_to_year_filed) %>% 
  summarize(timeperiod_funds = sum(amt, na.rm = T))

inv_lic <- inv_lic %>% 
  mutate(year_filed_range = case_when(
        year_filed <= 1999 ~ 'pre-2000',
        year_filed <= 2004 ~ '2000-2004',
        year_filed <= 2012 ~ '2005-2012',
        year_filed <= 2020 ~ '2013-2020',
        T ~ NA)) %>% 
  filter(!is.na(year_filed))

# We also want to account for number of inventions
inventor_counts_dates <- inv_lic %>% 
  group_by(uni_state, inventor_lastname, inventor_first_initial, year_filed_range) %>% 
  count() 

merge1 <- left_join(inv_award_dates, inventor_counts_dates,
                    by = c("uni_state", "inventor_lastname", "inventor_first_initial",
                           "match_to_year_filed" = "year_filed_range"))

# Average award amount divded by the number of varieties they've licensed
merge1$avg_funds <- merge1$timeperiod_funds/merge1$n
merge2 <- left_join(inv_lic, merge1,
                    by = c("uni_state", "inventor_lastname", "inventor_first_initial",
                            "year_filed_range" = "match_to_year_filed")) %>% 
  filter(!is.na(n))

# Now, we have many inventors per variety, but mainly we want to associate a sum of money with the variety now. SO we can cut inventor out, in a way, and just get one value per variety

merge2_reduced <- merge2 %>% 
  # remove is old award values -- total_funds, n
  select(-c(timeperiod_funds, n)) %>% 
  # group by everything but inventor info, which should be equivalent to each invention
  group_by(uni_state, crop_name_common, variety_name, invention_url, 
           year_filed, ip_type, notes, other_links, pvp_id, pto_id, 
           grin_id, soybase, cannot_find, misc, year_filed_range) %>%
  # this is creating the total average funds per variety -- if multiple inventor it will sum, if only one inventor is will remain the same (sum with itself)
  summarize(total_avg_funds = sum(avg_funds, na.rm = T))


# Join 2: Link awards + inventions to licenses/companies ----

# The thing linking them is the plant, so the variety name
colnames(merge2_reduced)
colnames(lic_co)

# We want licenses on the left because we'll keep licenses we don't have inventor data for
merge3 <- left_join(lic_co, merge2_reduced)
# So now the NA values in the total_funds column means we don't have any data for those

# Selection columns ----
# Okay, do we really need all of these columns? Let's reduce just a little
colnames(merge3)

# So remember here each row represents a license -- so a purchase between a university's variety and a company/other.
merge4 <- select(merge3, 
              # First select license data
              uni_state, crop_name_common, variety_name,
              fao_class1, ornamental, crop_cat,
              # Also additional data about the IP of that crop
              ip_type, year_filed, 
              # Then info about the company that bought it
              licensee, dnb_name, agreement_type, effective_date, end_date,
              company_type, yr_founded, country, state, domestic, licensee_type,
              highest_employee_number, highest_sales_number, size_range_employee,
              size_range_sales, year_filed_range,
              # And the award amount recieved by the breeders/inventors associated
              # Note that there names are removed now
              total_avg_funds
)
write.csv(merge4, "~/Box/lgu/data_clean/joined_inventions_licenses_awards_timeperiod.csv",
          row.names = F)


# But as an intermediate step, we can sum up all the money per inventor so that our join doesnt blow up
inv_award_total <- inv_award %>% 
  group_by(uni_state, inventor_lastname, inventor_first_initial) %>% 
  summarize(total_funds = sum(amt, na.rm = T))

# We want to get a count of the inventions each inventor has created

## DO WE WANT TO SEPARATE OUT EXCLUSIVE/NOTEXCLUSIVE
inventor_counts <- inv_lic %>% 
  group_by(uni_state, inventor_lastname, inventor_first_initial) %>% 
  count()

df.5 <- left_join(inv_award_total, inventor_counts)

# Average award amount divded by the number of varieties they've licensed
df.5$avg_funds <- df.5$total_funds/df.5$n

df1 <- left_join(inv_lic, df.5)

# Now, we have many inventors per variety, but mainly we want to associate a sum of money with the variety now. SO we can cut inventor out, in a way, and just get one value per variety

df1_reduced <- df1 %>% 
  # remove is old award values -- total_funds, n
  select(-c(total_funds, n)) %>% 
  # group by everything but inventor info, which should be equivalent to each invention
  group_by(uni_state, crop_name_common, variety_name, invention_url, 
           year_filed, ip_type, notes, other_links, pvp_id, pto_id, 
           grin_id, soybase, cannot_find, misc) %>%
  # this is creating the total average funds per variety -- if multiple inventor it will sum, if only one inventor is will remain the same (sum with itself)
  summarize(total_avg_funds = sum(avg_funds, na.rm = T))
  

# Join 2: Link awards + inventions to licenses/companies ----

# The thing linking them is the plant, so the variety name
colnames(df1_reduced)
colnames(lic_co)

# We want licenses on the left because we'll keep licenses we don't have inventor data for
df2 <- left_join(lic_co, df1_reduced)
# So now the NA values in the total_funds column means we don't have any data for those

# Selection columns ----
# Okay, do we really need all of these columns? Let's reduce just a little
colnames(df2)

# So remember here each row represents a license -- so a purchase between a university's variety and a company/other.
df3 <- select(df2, 
               # First select license data
               uni_state, crop_name_common, variety_name,
               fao_class1, ornamental, crop_cat,
               # Also additional data about the IP of that crop
               ip_type, year_filed, 
               # Then info about the company that bought it
               licensee, dnb_name, agreement_type, effective_date, end_date,
               company_type, yr_founded, country, state, domestic, licensee_type,
               highest_employee_number, highest_sales_number, size_range_employee,
               size_range_sales,
               # And the award amount recieved by the breeders/inventors associated
               # Note that there names are removed now
               total_avg_funds
                )
write.csv(df3, "~/Box/lgu/data_clean/joined_inventions_licenses_awardstotal.csv",
          row.names = F)
