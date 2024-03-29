library(dplyr)
library(stringr)
# Set working directory to Box
setwd("~/Box/lgu")

# Read in data
## Licensing data
license <- read.csv("data_clean/license.csv") %>% 
  filter(!is.na(crop_name_common)) %>%  # removing inventions
  filter(crop_name_common != "Unknown") # removing the few unknown crops from NH
## Company database, scraped with RSelenium
company <- read.csv("data_clean/company_db_full.csv") %>% 
  unique() %>% 
  rename(company_type = type)
company$licensee_type <- "company"
## Other kinds of licensees
other_licensee <- read.csv("data_indices/other_licensee.csv")
other_licensee$licensee_type <- "other"
## Crop master list
crop <- read.csv("data_indices/crop.csv") %>% 
  select(crop_name_common:orn_use)
## Agreement master list
agreement <- read.csv("data_indices/agreements.csv")

# --- Checking the data: Do we have our "official names" right? ----
## Crops ----
# Crops indexed into categories based on FAO https://www.fao.org/3/a0135e/A0135E10.htm#app3
length(unique(crop$crop_name_common)) # we have 335 crops on our master list (this includes the names in pvps that are not in the licenses)
length(unique(license$crop_name_common)) # have 166 in the licenses
# Here are the differences: the crops in common that are not in the master list
unique(license$crop_name_common[!(license$crop_name_common %in% unique(crop$crop_name_common))]) # zero means we are okay
# These are those that are in PVP but not in the licenses
unique(crop$crop_name_common[!(crop$crop_name_common %in% unique(license$crop_name_common))])

license <- license %>% 
  left_join(crop) %>% 
  unique() %>% 
  mutate(crop_cat = case_when(
    fao_class1 == "sugar_crops" ~ "Sugar, spice & medicinal",
    fao_class1 == "spice_crops" ~ "Sugar, spice & medicinal",
    fao_class1 == "medicinal" ~ "Sugar, spice & medicinal",
    fao_class1 == "cereals" ~ "Cereals",
    fao_class1 == "fruits_nuts" ~ "Fruits & nuts",
    fao_class1 == "grasses_fodder" ~ "Grasses & fodder",
    fao_class1 == "leguminous" ~ "Leguminous",
    fao_class1 == "non_food" ~ "Non-food (cotton & tabacco)",
    fao_class1 == "oilseeds" ~ "Oilseeds",
    fao_class1 == "root_tuber" ~ "Roots & tubers",
    fao_class1 == "veg_melon" ~ "Vegetables & melons",
    ornamental == T ~ "Ornamental",
    T ~ fao_class1
  ))

table(license$crop_cat)


## Agreement types ----
length(unique(agreement$official_name)) # we have 15 agreement types on our master list
length(unique(license$agreement_type)) # But we have 22 in our licenses
table(agreement$official_name)
table(license$agreement_type)

# For now I will ID these as unspecified
exl <- c("Amendment: License: Plant: Exclusive: New IP",
         "Exclusive",
         "Exclusive License",
         "Exclusive Option")
nexl <- c("Amendment: License: Plant: Non-Exclusive: New IP",
          "Non- Exclusive Option",
          "Master; non-exclusive",
          "Non- Exclusive License")
unsp <- c("Amended and restated license", 
          "Assignment license", 
          "Commercial Germplasm",
          "Exclusive + Non-Exclusive",
          "Experimental license", 
          "Interinstitutional agreement", 
          "License agreement",
          "License: Plant: Hybrid",
          "License: Plant: Parental Use",
          "Master agreement",
          "Sublicense")
license$agreement_type[license$agreement_type %in% exl] <- "Exclusive license"
license$agreement_type[license$agreement_type %in% nexl] <- "Non-exclusive license"
license$agreement_type[license$agreement_type %in% unsp] <- "Unspecified license"
table(license$agreement_type)


## Licensees (companies and other) ----
table(company$db_type)
company <- unique(select(company, - c(link, description)))
length(unique(company$official_name)) ; nrow(company) # 1161 and 1189, which means we have a lot of duplicates
duplicates <- company$official_name[which(duplicated(company$official_name))] # any duplications?
unique(sort(duplicates))
rpl_na <- function(x){ifelse(x == "", NA, x)}
company <- data.frame(sapply(company, rpl_na))

# This function goes through and selects the company profile that 1. has the most info about it, 2. has total employee number, 3. has earlier est. year -- these are the decision criteria I made for keeping one version of a duplicate company over another
dup_df <- data.frame()
for(i in unique(sort(duplicates))){
  dups <- filter(company, official_name == i)
  nas <- data.frame(apply(dups, 1, is.na))
  sums <- data.frame("totals" = apply(nas, 2, sum)) 
  morenas <- which(sums$total == max(sums$total))
  higher_emp <- which(dups$total_employee_number == max(dups$total_employee_number))
  older <- which(dups$yr_founded == min(dups$yr_founded))
  if(length(morenas) == 1){
    solo <- dups[morenas,]
  } else if (length(higher_emp) == 1){
    solo <- dups[higher_emp,]
  } else if (length(older) == 1){
    solo <- dups[older,]
    } else {
      solo <- dups # cannot filter yet
    }
  dup_df <-rbind(dup_df, solo)
}

# Need to manually choose for Beck's
dup_df <- filter(dup_df, !(official_name == "Beck's Superior Hybrids" & 
                             is.na(incorp_location)))

company <- company %>% filter(!(official_name %in% duplicates)) %>% 
  rbind(dup_df)
company$official_name[which(duplicated(company$official_name))] # any duplications?
# Nope
length(unique(company$official_name)) ; nrow(company)
# 1164 companies in the company master list

countries <- c("Spain", "Australia", "Chile", "Japan", "Italy",
               "New Zealand", "Belgium", "Bermuda", "United Kingdom",
               "Germany", "Netherlands", "Canada", "Norway", "France",
               "Bulgaria", "Turkey", "Switzerland", "Denmark",
               "Israel", "South Africa", "Sri Lanka", "India",
               "Honduras", "Ireland", "Argentina", "Romania",
               "Morocco", "Venezuela", "Costa Rica", "United Arab Emirates",
               "Mexico", "Bahamas", "Portugal", "Austria", "China", "Poland",
               "Brazil")
countries.p <- paste(countries, collapse = "|")
states <- read.csv("data_indices/states_abbr.csv")

# Merge ----
company <- rename(company, licensee = official_name)
other_licensee <- other_licensee %>% 
  rename(licensee = official_name) %>% 
  select(licensee, type, licensee_type)
# Need to read in other companies
length(unique(company$licensee)) # we have 1158 on the downloaded master list
length(unique(other_licensee$licensee)) # 141 on other
1158+141 # 1199

license$licensee <- str_remove_all(tools::toTitleCase(trimws(license$licensee)),
                                   "[:punct:]")
company$licensee <- str_remove_all(tools::toTitleCase(trimws(company$licensee)), 
                                   "[:punct:]")
other_licensee$licensee <- str_remove_all(tools::toTitleCase(trimws(other_licensee$licensee)), 
                                          "[:punct:]")

#summary(company$local_sales_number)
#company <- company %>% 
#  mutate(company_size = case_when(
#    local_sales_number <= 200000 ~ "Very small (<$200K)",
#    local_sales_number > 200000 & local_sales_number <= 1000000 ~ "Small ($220K - $1M)",
#    local_sales_number > 1000000 & local_sales_number <= 10000000 ~ "Medium ($1M - $10M)",
#    local_sales_number > 10000000 ~ "Large ($10M+)",
#    T ~ "Unknown"
#  ))
#table(company$company_size)

# Which companies are in the license list but not the database?
mistakes <- unique(license$licensee[!(license$licensee %in% unique(company$licensee) | 
                                        license$licensee %in% unique(other_licensee$licensee))])
sort(unique(mistakes)) # These are three that I didn't get in the third public scrape; could have
write.csv(company, "data_clean/company_db_full_clean.csv")
write.csv(license, "data_clean/license_clean.csv")

df <- merge(license, company, by = c('licensee'), all.x=TRUE)
df <- merge(df, other_licensee, by = c('licensee'), all.x = TRUE) %>% unique()
# This has 73 fewer than original -- maybe that is because of the 5 mistakes?
df <- df %>% 
  mutate(licensee_type = case_when(
    is.na(licensee_type.x) & !is.na(licensee_type.y) ~ licensee_type.y,
    T ~ licensee_type.x
  )) %>% 
  select(-c(licensee_type.x, licensee_type.y)) %>% 
  unique() %>% 
  mutate(domestic = ifelse(is.na(domestic), F, T))
table(df$licensee_type)

df$company_size <- ifelse(df$licensee_type == "other", "Non-company", df$company_size)
table(df$company_size)


western <- c("California", "Oregon", "Washington", "Idaho", "Nevada",
                     "Arizona", "Utah", "New Mexico", "Colorado", "Wyoming", "Montana",
                     "Alaska", "Hawaii")
northcentral <- c("North Dakota", "South Dakota", "Kansas",
                          "Nebraska", "Minnesota", "Iowa", "Missouri", "Wisconsin", 
                          "Illinois", "Michigan", "Indiana", "Ohio")
southern <- c("Texas", "Oklahoma", "Arkansas", "Louisiana", 
                      "Kentucky", "Tennessee", "Mississippi", "Alabama", "Florida", 
                      "Georgia", "South Carolina", "North Carolina", "Virginia")
northeast <- c("West Virginia", "Maryland", "Delaware", 
                       "New Jersey", "Pennsylvania", "New York", "Connecticut", 
                       "Rhode Island", "Massachusetts", "Vermont", "New Hampshire", "Maine")

sum(table(df$domestic))
df <- df %>% 
  mutate(uni_region = case_when(
    uni_state %in% western ~ "West",
    uni_state %in% northcentral ~ "North Central",
    uni_state %in% northeast ~ "Northeast",
    uni_state %in% southern ~ "South",
    T ~ "Unknown"
  )) %>% 
  mutate(company_region = case_when(
    domestic == F ~ "International",
    company_location %in% western ~ "West",
    company_location %in% northcentral ~ "North Central",
    company_location %in% northeast ~ "Northeast",
    company_location %in% southern ~ "South",
    T ~ "Unknown"
  )) %>% 
  mutate(spatial_match = case_when(
    uni_region == company_region ~ "Within region",
    company_region == "Unknown" ~ "Unknown",
    company_type == "Non-company" ~ "Unknown",
    uni_region != company_region ~ "Outside region",
    T ~ "Unknown"
  ))

table(df$spatial_match)

# Syngenta's top-10 universities: https://www.syngenta-us.com/thrive/community/top-10-ag-universities.html
# Other ranking: https://www.topuniversities.com/university-rankings/university-subject-rankings/2015/agriculture-forestry#sorting=rank+region=+country=+faculty=+stars=false+search

# prestige score should be average Q ranking + faculty numbers 
top10uni <- c("New York", "California", "Texas", "Iowa", "Illinois", "Florida",
              "Indiana", "Ohio", "North Carolina") # Georgia and Washington?

df$topuni <- ifelse(df$uni_state %in% top10uni, T, F)

df$agreement_bi <- ifelse(df$agreement_type == "Exclusive license", 1,
                          ifelse(df$agreement_type == "Non-exclusive license", 0, NA))
#df$funding_amt_grpd <- ifelse(!is.na(df$funding_yr_grpd) & is.na(df$funding_amt_grpd), 
#                              0 , df$funding_amt_grpd)
#df$funding_amt_grpd_log <- log(df$funding_amt_grpd)
#df$funding_amt_grpd_log <- ifelse(df$funding_amt_grpd_log == "-Inf", 0, #df$funding_amt_grpd_log)
df$local_sales_number <- ifelse(df$licensee_type == "other", 0, df$local_sales_number)
#df$local_sales_number_log <- log(df$local_sales_number)
#df$local_sales_number_log <- ifelse(df$local_sales_number_log == "-Inf", 0, df$local_sales_number_log)
#df$pres <- ifelse(df$license_yr == 2000, "Clinton",
#                  ifelse(df$license_yr %in% 2001:2009, "Bush",
#                         ifelse(df$license_yr %in% 2010:2016, "Obama",
#                                ifelse(df$license_yr %in% 2017:2021, "Trump", "Other"))))
df$inregion <- ifelse(df$spatial_match == "Within region", 1,
                      ifelse(df$spatial_match == "Outside region", 0, NA))


df$company_size <- factor(df$company_size, levels = c("Large ($10M+)",
                                                      "Medium ($1M - $10M)",
                                                      "Small ($220K - $1M)",
                                                      "Very small (<$200K)",
                                                      "Non-company", 
                                                      "Unknown"))

write.csv(df, "data_clean/license_df.csv", row.names = F)

