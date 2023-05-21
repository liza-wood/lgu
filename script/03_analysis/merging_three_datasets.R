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

# This is super 'long' because it takes every award any inventor has ever gotten, and associates it with every plant they've ever bred. Cleaning and playing with time frames here is defnitely something of interest for the future
inventor_allmoney <- left_join(inv_lic, inv_award)

# But as an intermediate step, we can sum up all the money per inventor so that our join doesnt blow up
inv_award_total <- inv_award %>% 
  group_by(uni_state, inventor_lastname, inventor_first_initial) %>% 
  summarize(total_funds = sum(amt, na.rm = T))

df1 <- left_join(inv_lic, inv_award_total)

# Now, we have many inventors per variety, but mainly we want to associate a sum of money with the variety now. SO we can cut inventor out, in a way, and just get one value per variety
grp_by_cols <- colnames(df1)[-c(15:21)]
grp_by_cols <- paste(grp_by_cols, collapse = ", ")
df1_reduced <- df1 %>% 
  # group by everything but inventor info, which should be equivalent to each invention
  group_by(uni_state, crop_name_common, variety_name, invention_url, 
           year_filed, ip_type, notes, other_links, pvp_id, pto_id, 
           grin_id, soybase, cannot_find, misc) %>%
  summarize(total_funds = sum(total_funds, na.rm = T))
  

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
               total_funds
                )
write.csv(df3, "~/Box/lgu/data_clean/joined_inventions_licenses_awardstotal.csv",
          row.names = F)
