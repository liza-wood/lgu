library(dplyr)
library(stringr)
# Set working director to Box
setwd("~/Box/")

# Read in data
## Licensing data
license <- read.csv("lgu/license.csv") %>% 
  filter(!is.na(crop_name_common))# removing inventions
## Company database, scraped with RSelenium
company <- read.csv("lgu/company_db.csv")
company$licensee_type <- "company"
## Other licensee master list
other_licensee <- read.csv("lgu/other_licensee.csv")
other_licensee$licensee_type <- "other"
## Crop master list
crop <- read.csv("lgu/crop.csv")
## Agreement master list
agreement <- read.csv("lgu/agreements.csv")

# --- Checking the data: Do we have our "official names" right? ----
## Crops
length(unique(crop$crop_name_common)) # we have 77 crops on our master list
length(unique(license$crop_name_common)) # have 73 in the licenses...
# It looks like we have all matches, which is good
unique(license$crop_name_common[!(license$crop_name_common %in% unique(crop$crop_name_common))])

minorberry <- c("Black chokeberry", "Black raspberry", "Brambleberry")
majorberry <- c("Blackberry", "Blueberry", "Red raspberry", "Strawberry")
forage <- c("Alfalfa")
lawngrass <- c("Bermuda grass", "Bermuda grass hybrid", "Creeping Bentgrass", "Lawngrass",
               "St. Augustine grass")
orngrass <- c("Crowngrass", "Elephant grass", "Meadow Bromegrass", 
              "Prairie cord grass", "Switchgrass", "Timothy grass")
ornshrub <- c("Bay Cherry","Butterfly bush", "Crape myrtle",  "Indian shot",
              "Japanese barberry", "Leadtree", "Lily of the Incas", "Pincushions",
              "Prayer plant", "Rhododendron", "Sand cherry", "Sweet fern", "Sweetgale",
              "Witherod Viburnum")
grape <- c("Grape (currant) rootstock", "Grape (riverbank) rootstock", "Grape (rufo) rootstock")
nuttree <- c("Almond", "Walnut", "Walnut rootstock", "Pistachio")
stonefruit <- c("Cherry (sour)", "Cherry (sweet)", "Peach",
                "Peach rootstock", "Prune Plum")
fruittree <- c("Apple", "Fig", "Papaya", "Pear")
fieldcrops <- c("Corn", "Cotton", "Soybean")
minorgrain <- c("Barley", "Durum wheat", "Nude oats", "Oat", "Pearl millet", "Sorghum", "Teff")
majorgrain <- c("Asian rice", "Wheat")
legume <- c("Black-eyed pea", "Dry bean", "Faba bean", "Pea", "Peanut")
oilseed <- c("Camelina", "Safflower", "Winter canola")
rootstarch <- c("Potato", "Sweet potato", "Taro")

license <- license %>% 
  mutate(crop = case_when(
    crop_name_common %in% minorberry ~ "Berry (minor)",
    crop_name_common %in% majorberry ~ "Berry (major)",
    crop_name_common %in% forage ~ "Other",
    crop_name_common %in% lawngrass ~ "Lawn grass",
    crop_name_common %in% orngrass ~ "Ornamental grass",
    crop_name_common %in% ornshrub ~ "Ornamental shrub",
    crop_name_common %in% grape ~ "Grape",
    crop_name_common %in% nuttree ~ "Nut tree",
    crop_name_common %in% fruittree ~ "Fruit tree (other)",
    crop_name_common %in% stonefruit ~ "Fruit tree (stone fruit)",
    crop_name_common %in% fieldcrops ~ "Field crops",
    crop_name_common %in% minorgrain ~ "Grain (minor)",
    crop_name_common %in% majorgrain ~ "Grain (major)",
    crop_name_common %in% legume ~ "Legume",
    crop_name_common %in% oilseed ~ "Oil seed",
    crop_name_common %in% rootstarch ~ "Roots (starch)",
    T ~ "Other"
  ))

table(license$crop)

license <- license %>% 
  mutate(crop_fewer = case_when(
    crop_name_common %in% minorberry ~ "Berry",
    crop_name_common %in% majorberry ~ "Berry",
    crop_name_common %in% forage ~ "Other",
    crop_name_common %in% lawngrass ~ "Lawn grass",
    crop_name_common %in% orngrass ~ "Ornamental",
    crop_name_common %in% ornshrub ~ "Ornamental",
    crop_name_common %in% grape ~ "Perennial (fruit/nut)",
    crop_name_common %in% nuttree ~ "Perennial (fruit/nut)",
    crop_name_common %in% fruittree ~ "Perennial (fruit/nut)",
    crop_name_common %in% stonefruit ~ "Perennial (fruit/nut)",
    crop_name_common %in% fieldcrops ~ "Field crops",
    crop_name_common %in% minorgrain ~ "Grain",
    crop_name_common %in% majorgrain ~ "Grain",
    crop_name_common %in% legume ~ "Legume",
    crop_name_common %in% oilseed ~ "Oil seed",
    crop_name_common %in% rootstarch ~ "Roots (starch)",
    T ~ "Other"
  ))

table(license$crop_fewer)

## Agreement types
length(unique(agreement$official_name)) # we have 15 agreement types on our master list
length(unique(license$agreement_type)) # have 11 -- what is happening here? 
table(agreement$official_name)
table(license$agreement_type)

# For now
unsp <- c("Amended and restated license", "Assignment license", "Exclusive + Non-Exclusive",
          "Experimental license", "Interinstitutional agreement", "License agreement",
          "Sublicense")
license$agreement_type[license$agreement_type %in% unsp] <- "Unspecified license"
table(license$agreement_type)


## Licensees (companies and other)
length(unique(company$official_name)) # we have 277 -- this must mean there is a duplicate
company$official_name[which(duplicated(company$official_name))] # any duplications?

length(unique(other_licensee$official_name)) # There are 38 here


# Merge ----
company <- rename(company, "licensee" = "official_name")
other_licensee <- other_licensee %>% 
  rename("licensee" = "official_name") %>% 
  select(licensee, type, licensee_type)

license$licensee <- str_remove_all(tools::toTitleCase(trimws(license$licensee)),
                                   "[:punct:]")
company$licensee <- str_remove_all(tools::toTitleCase(trimws(company$licensee)), 
                                   "[:punct:]")
other_licensee$licensee <- str_remove_all(tools::toTitleCase(trimws(other_licensee$licensee)), 
                                          "[:punct:]")

# Which companies are in the license list but not the database?
mistakes <- unique(license$licensee[!(license$licensee %in% unique(company$licensee) | 
                                        license$licensee %in% unique(other_licensee$licensee))])

mistakes


# Company data ---

company$rev <- str_remove_all(company$revenue, "\\$|\\,")
company$rev <- ifelse(str_detect(company$rev, "million"), 
                 paste0(str_remove_all(company$rev, "\\.|million|\\s"), "00000"),
                 ifelse(str_detect(company$rev, "billion"), 
                        paste0(str_remove_all(company$rev, "\\.|billion|\\s"), "0000000"),
                        company$rev))
company$rev <- as.numeric(company$rev)
company$employees <- as.numeric(str_remove_all(company$employees, "\\,"))

summary(company$rev)
summary(company$employees)

company <- company %>% 
  mutate(company_size = case_when(
    rev <= 350000 ~ "Very small (<$350K)",
    rev > 350000 & rev <= 10000000 ~ "Small ($328K - $10M)",
    rev > 10000000 & rev <= 100000000 ~ "Medium ($10M - $100M)",
    rev > 100000000 ~ "Large (100M+)",
    T ~ "Unknown"
  ))
table(company$company_size)

company %>% 
  group_by(company_size) %>% 
  summarize(median(employees, na.rm = T))

countries <- c("Spain", "Australia", "Chile", "Japan", "Italy",
                     "New Zealand", "Belgium", "Bermuda", "United Kingdom",
                     "Germany", "Netherlands", "Canada", "Norway", "France")
countries.p <- paste(countries, collapse = "|")
states <- read.csv("lgu/states_abbr.csv")

location <- c()
for(j in 1:nrow(company)){
  for(i in 1:nrow(states)){
    if(!is.na(company$address[j]) &  str_detect(company$address[j], paste0("\\s", states$abbr[i], ","))){
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
company$location <- ifelse(company$company_location == "", NA, company$company_location)

company$domestic <- ifelse(company$location %in% countries, F, T)
table(company$domestic)


df <- merge(license, company, by = c('licensee'), all.x=TRUE)
df <- merge(df, other_licensee, by = c('licensee'), all.x = TRUE)
df <- df %>% 
  mutate(licensee_type = case_when(
    is.na(licensee_type.x) & !is.na(licensee_type.y) ~ licensee_type.y,
    T ~ licensee_type.x
  )) %>% 
  select(-c(licensee_type.x, licensee_type.y, notes.x, notes.y))
table(df$licensee_type)

table(df$company_size)
df$company_size <- ifelse(df$licensee_type == "other", "Non-company", df$company_size)
table(df$company_size)

#df <- license %>% 
 # left_join(company) %>% 
#  left_join(other_licensee)

#print(df$licensee[1])
#cat(df$licensee[1])
#print(company$licensee[39])
#cat(company$licensee[39])

western <- c("California", "Oregon", "Washington", "Idaho", "Nevada", "Arizona", "Utah", "New Mexico", "Colorado", "Wyoming", "Montana", "Alaska", "Hawaii")
northcentral <- c("North Dakota", "South Dakota", "Kansas", "Nebraska", "Minnesota", "Iowa", "Missouri", "Wisconsin", "Illinois", "Michigan", "Indiana", "Ohio")
southern <- c("Texas", "Oklahoma", "Arkansas", "Louisiana", "Kentucky", "Tennessee", "Mississippi", "Alabama", "Florida", "Georgia", "South Carolina", "North Carolina", "Virginia")
northeast <- c("West Virginia", "Maryland", "Delaware", "New Jersey", "Pennsylvania", "New York", "Connecticut", "Rhode Island", "Massachusetts", "Vermont", "New Hampshire", "Maine")

df <- df %>% 
  mutate(uni_region = case_when(
    state %in% western ~ "West",
    state %in% northcentral ~ "North Central",
    state %in% northeast ~ "Northeast",
    state %in% southern ~ "South",
    T ~ "Unknown"
  )) %>% 
  mutate(company_region = case_when(
    location %in% western ~ "West",
    location %in% northcentral ~ "North Central",
    location %in% northeast ~ "Northeast",
    location %in% southern ~ "South",
    domestic == F ~ "International",
    T ~ "Unknown"
  )) %>% 
  mutate(spatial_match = case_when(
    uni_region == company_region ~ "Within region",
    company_region == "Unknown" ~ "Unknown",
    uni_region != company_region ~ "Outside region",
    T ~ "Unknown"
  ))

table(df$spatial_match)
table(df$company_size)

# Syngenta's top-10 universities: https://www.syngenta-us.com/thrive/community/top-10-ag-universities.html
# Other ranking: https://www.topuniversities.com/university-rankings/university-subject-rankings/2015/agriculture-forestry#sorting=rank+region=+country=+faculty=+stars=false+search

# prestige score should be average Q ranking + faculty numbers 
top10uni <- c("New York", "California", "Texas", "Iowa", "Illinois", "Florida",
              "Indiana", "Ohio", "North Carolina") # Georgia and Washington?

df$topuni <- ifelse(df$state %in% top10uni, T, F)


write.csv(df, "lgu/merged_df.csv", row.names = F)

