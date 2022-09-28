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
length(unique(crop$crop_name_common)) # we have 168 crops on our master list
length(unique(license$crop_name_common)) # have 165 in the licenses
# Here are the differences: the crops in common that are not in the master list
unique(license$crop_name_common[!(license$crop_name_common %in% unique(crop$crop_name_common))])
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
length(unique(company$official_name)) ; nrow(company) # 1164 and 1192, which means we have a lot of duplicates
duplicates <- company$official_name[which(duplicated(company$official_name))] # any duplications?
unique(sort(duplicates))
rpl_na <- function(x){ifelse(x == "", NA, x)}
company <- data.frame(sapply(company, rpl_na))

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
dup_df <- filter(dup_df, !(official_name == "Beck's Superior Hybrids" & is.na(incorp_location)))

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
length(unique(company$licensee)) # we have 1164 on the downloaded master list
length(unique(other_licensee$licensee)) # 141 on other
1164+141 # 1305
length(unique(license$licensee)) 
1305-1257 # 48 missing

license$licensee <- str_remove_all(tools::toTitleCase(trimws(license$licensee)),
                                   "[:punct:]")
company$licensee <- str_remove_all(tools::toTitleCase(trimws(company$licensee)), 
                                   "[:punct:]")
other_licensee$licensee <- str_remove_all(tools::toTitleCase(trimws(other_licensee$licensee)), 
                                          "[:punct:]")

# Company data ---

company$local_sales_number <- str_remove_all(company$local_sales_number, "\\$|\\,")
company$local_sales_number <- ifelse(str_detect(company$local_sales_number, "\\.\\d\\d\\smillion"), 
                 paste0(str_remove_all(company$local_sales_number, "\\.|million|\\s"), "000"),
                 ifelse(str_detect(company$local_sales_number, "\\dM\\b"), 
                        paste0(str_remove_all(company$local_sales_number, "\\.|M|\\s"), "0000"),
                 ifelse(str_detect(company$local_sales_number, "\\dK\\b"), 
                        paste0(str_remove_all(company$local_sales_number, "\\.|K|\\s"), "0"),
                 ifelse(str_detect(company$local_sales_number, "\\dB\\b"), 
                        paste0(str_remove_all(company$local_sales_number, "\\.|B|\\s"), "0000000"),
                        company$local_sales_number))))
company$local_sales_number <- as.numeric(company$local_sales_number)

company$global_sales_number <- str_remove_all(company$global_sales_number, "\\$|\\,")
company$global_sales_number <- ifelse(str_detect(company$global_sales_number, "\\.\\d\\d\\smillion"), 
              paste0(str_remove_all(company$global_sales_number, "\\.|million|\\s"), "000"),
                              ifelse(str_detect(company$global_sales_number, "\\dM\\b"), 
              paste0(str_remove_all(company$global_sales_number, "\\.|M|\\s"), "0000"),
                              ifelse(str_detect(company$global_sales_number, "\\dK\\b"), 
              paste0(str_remove_all(company$global_sales_number, "\\.|K|\\s"), "0"),
                              ifelse(str_detect(company$global_sales_number, "\\dB\\b"), 
              paste0(str_remove_all(company$global_sales_number, "\\.|B|\\s"), "0000000"),
                                    company$global_sales_number))))
company$global_sales_number <- as.numeric(company$global_sales_number)

company$assets <- str_remove_all(company$assets, "\\$|\\,")
company$assets <- ifelse(str_detect(company$assets, "\\.\\d\\d\\smillion"), 
             paste0(str_remove_all(company$assets, "\\.|million|\\s"), "000"),
                               ifelse(str_detect(company$assets, "\\dM\\b"), 
             paste0(str_remove_all(company$assets, "\\.|M|\\s"), "0000"),
                               ifelse(str_detect(company$assets, "\\dK\\b"), 
             paste0(str_remove_all(company$assets, "\\.|K|\\s"), "0"),
                                ifelse(str_detect(company$assets, "\\dB\\b"), 
             paste0(str_remove_all(company$assets, "\\.|B|\\s"), "0000000"),
                                       company$assets))))
company$assets <- as.numeric(company$assets)
# DOUBLE CHECK ASSETS

company$local_employee_number <- as.numeric(str_remove_all(company$local_employee_number, "\\,"))
company$global_employee_number <- as.numeric(str_remove_all(company$global_employee_number, "\\,"))

location <- c()
for(j in 1:nrow(company)){
  for(i in 1:nrow(states)){
    if(!is.na(company$address[j]) &  
       (str_detect(company$address[j], paste0("\\s", states$abbr[i], ",")) |
       str_detect(company$address[j], paste0("\\b", states$abbr[i])) |
       str_detect(company$address[j], states$state[i]))){
      location[j] <- states$state[i]
    } else {
      next
    }
  }
}

location <- c(location, rep("", nrow(company) - length(location)))
company$company_location <- location
company$company_location <- ifelse(is.na(company$company_location), 
                           str_extract(company$address, countries.p), company$company_location)
company$company_location <- ifelse(company$company_location == "", NA, company$company_location)

company$domestic <- ifelse(company$company_location %in% countries, F, T)
table(company$domestic)

summary(company$local_sales_number)
company <- company %>% 
  mutate(company_size = case_when(
    local_sales_number <= 200000 ~ "Very small (<$200K)",
    local_sales_number > 200000 & local_sales_number <= 1000000 ~ "Small ($220K - $1M)",
    local_sales_number > 1000000 & local_sales_number <= 10000000 ~ "Medium ($1M - $10M)",
    local_sales_number > 10000000 ~ "Large ($10M+)",
    T ~ "Unknown"
  ))
table(company$company_size)

# Which companies are in the license list but not the database?
mistakes <- unique(license$licensee[!(license$licensee %in% unique(company$licensee) | 
                                        license$licensee %in% unique(other_licensee$licensee))])
sort(unique(mistakes)) # These are three that I didn't get in the third public scrape; could have

df <- merge(license, company, by = c('licensee'), all.x=TRUE)
df <- merge(df, other_licensee, by = c('licensee'), all.x = TRUE) %>% unique()
# This has 73 fewer than original -- maybe that is because of the 5 mistakes?
df <- df %>% 
  mutate(licensee_type = case_when(
    is.na(licensee_type.x) & !is.na(licensee_type.y) ~ licensee_type.y,
    T ~ licensee_type.x
  )) %>% 
  select(-c(licensee_type.x, licensee_type.y, notes)) %>% 
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


write.csv(df, "data_clean/merged_df.csv", row.names = F)

