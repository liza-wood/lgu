library(dplyr)
library(stringr)
# Set working directory to Box
setwd("~/Box/lgu")

# Read in data
## Licensing data
license <- read.csv("data_clean/license.csv") %>% 
  filter(!is.na(crop_name_common))# removing inventions
## Company database, scraped with RSelenium
company <- read.csv("data_clean/company_db_full.csv")
company$licensee_type <- "company"
## Other kinds of licensees
other_licensee <- read.csv("data_indices/other_licensee.csv")
other_licensee$licensee_type <- "other"
## Crop master list
crop <- read.csv("data_indices/crop.csv")
## Agreement master list
agreement <- read.csv("data_indices/agreements.csv")

# --- Checking the data: Do we have our "official names" right? ----
## Crops ----
length(unique(crop$crop_name_common)) # we have 157 crops on our master list
length(unique(license$crop_name_common)) # have 177 in the licenses
# Here are the differences: the crops in common that are not in the master list
unique(license$crop_name_common[!(license$crop_name_common %in% unique(crop$crop_name_common))])

## There will be one cell just called Berries
minorberry <- c("Black chokeberry", "Black raspberry", "Brambleberry")
majorberry <- c("Blackberry", "Blueberry", "Red raspberry", "Strawberry", "Berries")
forage <- c("Alfalfa")
lawngrass <- c("Bermuda grass", "Bermuda grass hybrid", 
               "Creeping Bentgrass", "Lawngrass",
               "St. Augustine grass")
orngrass <- c("Crowngrass", "Elephant grass", "Meadow Bromegrass", 
              "Prairie cord grass", "Switchgrass", "Timothy grass",
              "Smooth cordgrass")
ornshrub <- c("Bay Cherry","Butterfly bush", "Crape myrtle",  "Indian shot",
              "Japanese barberry", "Leadtree", "Lily of the Incas", "Pincushions",
              "Prayer plant", "Rhododendron", "Sand cherry", "Sweet fern", "Sweetgale",
              "Witherod Viburnum", "Eastern White Cedar")
grape <- c("Grape (currant) rootstock", 
           "Grape (riverbank) rootstock", "Grape (rufo) rootstock")
nuttree <- c("Almond", "Walnut", "Walnut rootstock", "Pistachio")
stonefruit <- c("Cherry (sour)", "Cherry (sweet)", "Peach",
                "Peach rootstock", "Prune Plum")
fruittree <- c("Apple", "Fig", "Papaya", "Pear", "Black fig")
fieldcrops <- c("Corn", "Cotton", "Soybean")
minorgrain <- c("Barley", "Durum wheat", "Nude oats", 
                "Oat", "Pearl millet", "Sorghum", "Teff",
                "Rye", "Sea oats")
majorgrain <- c("Asian rice", "Wheat", "Soft red wheat", "Rice", "Long grain rice")
legume <- c("Black-eyed pea", "Dry bean", "Faba bean", "Pea", "Peanut")
oilseed <- c("Camelina", "Safflower", "Winter canola")
rootstarch <- c("Potato", "Sweet potato", "Taro")
vegetable <- c("Sweet and chili pepper")
other <- c("Stevia")


license <- license %>% 
  mutate(crop = case_when(
    crop_name_common %in% minorberry ~ "Berry (minor)",
    crop_name_common %in% majorberry ~ "Berry (major)",
    crop_name_common %in% forage ~ "Other",
    crop_name_common %in% lawngrass ~ "Lawn grass",
    crop_name_common %in% orngrass ~ "Ornamental",
    crop_name_common %in% ornshrub ~ "Ornamental",
    crop_name_common %in% grape ~ "Grape",
    crop_name_common %in% nuttree ~ "Nut tree",
    crop_name_common %in% fruittree ~ "Fruit tree (other)",
    crop_name_common %in% stonefruit ~ "Fruit tree (stone fruit)",
    crop_name_common %in% fieldcrops ~ "Field crops",
    crop_name_common %in% minorgrain ~ "Grain (minor)",
    crop_name_common %in% majorgrain ~ "Grain (major)",
    crop_name_common %in% legume ~ "Legume",
    crop_name_common %in% oilseed ~ "Oil seed",
    crop_name_common %in% rootstarch ~ "Roots and starches",
    crop_name_common %in% other ~ "Other",
    crop_name_common %in% vegetable ~ "Vegetable",
    T ~ "Missing"
  ))

table(license$crop)

license <- license %>% 
  mutate(crop_intermed = case_when(
    crop_name_common %in% minorberry ~ "Berry",
    crop_name_common %in% majorberry ~ "Berry",
    crop_name_common %in% forage ~ "Other",
    crop_name_common %in% lawngrass ~ "Ornamental",
    crop_name_common %in% orngrass ~ "Ornamental",
    crop_name_common %in% ornshrub ~ "Ornamental",
    crop_name_common %in% grape ~ "Perennial (fruit)",
    crop_name_common %in% nuttree ~ "Perennial (nut)",
    crop_name_common %in% fruittree ~ "Perennial (fruit)",
    crop_name_common %in% stonefruit ~ "Perennial (fruit)",
    crop_name_common %in% fieldcrops ~ "Field crops",
    crop_name_common %in% minorgrain ~ "Grain (minor)",
    crop_name_common %in% majorgrain ~ "Grain (major)",
    crop_name_common %in% legume ~ "Legume",
    crop_name_common %in% oilseed ~ "Oil seed",
    crop_name_common %in% rootstarch ~ "Roots and starches",
    crop_name_common %in% other ~ "Other",
    crop_name_common %in% vegetable ~ "Other",
    T ~ "Missing"
  ))

license <- license %>% 
  mutate(crop_fewer = case_when(
    crop_name_common %in% minorberry ~ "Berry",
    crop_name_common %in% majorberry ~ "Berry",
    crop_name_common %in% forage ~ "Other",
    crop_name_common %in% lawngrass ~ "Ornamental", #Lawn grass",
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

## Agreement types ----
length(unique(agreement$official_name)) # we have 15 agreement types on our master list
length(unique(license$agreement_type)) # But we have 12 in our licenses...
table(agreement$official_name)
table(license$agreement_type)

# For now I will ID these as unspecified
unsp <- c("Amended and restated license", 
          "Assignment license", 
          "Exclusive + Non-Exclusive",
          "Experimental license", 
          "Interinstitutional agreement", 
          "License agreement",
          "Master agreement",
          "Sublicense")
license$agreement_type[license$agreement_type %in% unsp] <- "Unspecified license"
table(license$agreement_type)


## Licensees (companies and other) ----
length(unique(company$official_name)) ; nrow(company) # we have 427 in both
company$official_name[which(duplicated(company$official_name))] # any duplications?
# LOADS

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

#multi_nats <- c("Syngenta Seeds, Inc.", "Bayer CropScience LP", "Syngenta Crop Protection, LLC",
#                "BASF Agrochemical Products BV", "Bayer Corportation", "Bayer", "Syngenta",
#                "BASF Corportation", "Monsanto Company", "West Bred Inc a Division of Monsanto",
#                "BASF Plant Science, LP", "BASF Agricultural Solutions Seed", "Planasa")
#

# Merge ----
company <- rename(company, licensee = official_name)
other_licensee <- other_licensee %>% 
  rename(licensee = official_name) %>% 
  select(licensee, type, licensee_type)
# Need to read in other companies
length(unique(company$licensee)) # we have 1075 on the downloaded list
length(unique(other_licensee$licensee)) # 140 on other
1075+140
length(unique(license$licensee)) # have 1266 on the licenses -- still 22 in the still_missing from company_cleaning and 30 just don't have any data
1266-(22+30)

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
       str_detect(company$address[j], paste0("\\s", states$abbr[i], ",")) |
       str_detect(company$address[j], paste0("\\b", states$abbr[i])) |
       str_detect(company$address[j], states$state[i])){
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
    local_sales_number <= 220000 ~ "Very small (<$220K)",
    local_sales_number > 220000 & local_sales_number <= 1000000 ~ "Small ($220K - $1M)",
    local_sales_number > 1000000 & local_sales_number <= 10000000 ~ "Medium ($1M - $10M)",
    local_sales_number > 10000000 ~ "Large ($10M+)",
    T ~ "Unknown"
  ))
table(company$company_size)

# Which companies are in the license list but not the database?
mistakes <- unique(license$licensee[!(license$licensee %in% unique(company$licensee) | 
                                        license$licensee %in% unique(other_licensee$licensee))])
#still missing = 22 and nothing = 30 --> 52 that should have no trace
mistakes
113-52 # 61 that are actually in the licenses that I don't have record of

df <- merge(license, company, by = c('licensee'), all.x=TRUE)
df <- merge(df, other_licensee, by = c('licensee'), all.x = TRUE)
df <- df %>% 
  mutate(licensee_type = case_when(
    is.na(licensee_type.x) & !is.na(licensee_type.y) ~ licensee_type.y,
    T ~ licensee_type.x
  )) %>% 
  select(-c(licensee_type.x, licensee_type.y, notes))
table(df$licensee_type)

table(company$company_size)
table(df$company_size)
df$company_size <- ifelse(df$licensee_type == "other", "Non-company", df$company_size)
table(df$company_size)


western <- toupper(c("California", "Oregon", "Washington", "Idaho", "Nevada",
                     "Arizona", "Utah", "New Mexico", "Colorado", "Wyoming", "Montana",
                     "Alaska", "Hawaii"))
northcentral <- toupper(c("North Dakota", "South Dakota", "Kansas",
                          "Nebraska", "Minnesota", "Iowa", "Missouri", "Wisconsin", 
                          "Illinois", "Michigan", "Indiana", "Ohio"))
southern <- toupper(c("Texas", "Oklahoma", "Arkansas", "Louisiana", 
                      "Kentucky", "Tennessee", "Mississippi", "Alabama", "Florida", 
                      "Georgia", "South Carolina", "North Carolina", "Virginia"))
northeast <- toupper(c("West Virginia", "Maryland", "Delaware", 
                       "New Jersey", "Pennsylvania", "New York", "Connecticut", 
                       "Rhode Island", "Massachusetts", "Vermont", "New Hampshire", "Maine"))


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

# Syngenta's top-10 universities: https://www.syngenta-us.com/thrive/community/top-10-ag-universities.html
# Other ranking: https://www.topuniversities.com/university-rankings/university-subject-rankings/2015/agriculture-forestry#sorting=rank+region=+country=+faculty=+stars=false+search

# prestige score should be average Q ranking + faculty numbers 
top10uni <- c("New York", "California", "Texas", "Iowa", "Illinois", "Florida",
              "Indiana", "Ohio", "North Carolina") # Georgia and Washington?

df$topuni <- ifelse(df$state %in% top10uni, T, F)


write.csv(df, "data_clean/merged_df.csv", row.names = F)

