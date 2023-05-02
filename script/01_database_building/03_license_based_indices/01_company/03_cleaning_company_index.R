# Company cleaning script
# Bring together database_h with others to combine and make sure we have all we can
library(dplyr)
library(stringr)

# Set working director to Box
setwd("~/Box/lgu")
# The main "master" list before any scraping
## 1128 companies on our master list
master <- read.csv("data_indices/company.csv") 
## 149 of them have no link
nolinks <- master %>% filter(is.na(dnb_link)) 
# This leaves, in theory, 979 companies that should have db information
master_links <- master %>% filter(!is.na(dnb_link))  

# Verify based on what is in licenses
license <- read.csv("data_clean/license.csv") %>% 
  filter(!is.na(crop_name_common))
other_licensee <- read.csv("data_indices/other_licensee.csv")
length(unique(license$licensee)) - nrow(other_licensee) - nrow(nolinks)
# Once you remove licensees that are not companies and have no official link
# we have 965 licensees to get data for
needtomatch <- license %>% 
  filter(!(licensee %in% other_licensee$official_name)) %>% 
  filter(!(licensee %in% nolinks$official_name)) %>% 
  select(licensee) %>% 
  filter(!is.na(licensee)) %>% 
  unique()
nrow(needtomatch)
# off from above calculation by 11, unsure what those are 

# Now I am putting together the multiple versions of the scraped company databases I have
# 1. This scrape is ideally the most comprehensive, but did not get everything
## 918 records -- so missing something like ~70 records
## It also does not have exact names
db_h <- read.csv("data_clean/company_db_h.csv")
db_h$db_type <- "herbert"

# 2. These lists were scraped from X public facing in three rounds
## These lists have official names associated
first <- read.csv("data_clean/company_db.csv") %>% 
  filter(!is.na(name))
first_multi <- read.csv("data_clean/company_db_multi.csv") %>% 
  filter(!is.na(name)) %>% 
  mutate(duplicate = duplicated(name)) %>% 
  filter(duplicate == F) %>% 
  select(-duplicate) 
second <- read.csv("data_clean/company_db_2.csv") %>% 
  filter(!is.na(name)) 
third <- read.csv("data_clean/company_db_3.csv") %>% 
  filter(!is.na(name))
db_public <- rbind(first, first_multi, second, third) 
db_public$db_type <- "public"

## Removed because overlap -- these were eventually removed from the master index, but
## they remained from the first public scrape
rm <- c("Johnson Seed Co. (now part Seed of NWGG)", # is NWGG
        "Eddie Mercer Agri-Services, Inc.", # is its own with no link now, don't want it to be Helena
        "Whitgro, Inc.", # is NWGG
        "Discovery Garden's, LLC", "Cal-Ore Seed, Inc.") # Is California-Oregon
db_public <- filter(db_public, !(official_name %in% rm))
## Correcting misspellings
db_public$official_name[db_public$official_name == "Birdsong Coorporation"] <- "Birdsong Corporation"
db_public$official_name[db_public$official_name == "Proven Winners North Amercia, LLC"] <- "Proven Winners North America, LLC"
# Try to add on "official_name" to db_h when possible by merging the two
db_public_slim <- db_public %>% select(official_name, name, description)
db_public_slim$name <- trimws(str_remove_all(db_public_slim$name, "\\.|\\,"))
db_h <- db_h %>% select(-official_name) %>% 
  mutate(name = trimws(str_remove_all(name, "\\.|\\,"))) %>% 
  left_join(db_public_slim)

# These are the database records where the name in the db did not match the
# official name on the master, and I forgot to align them
need_official <- db_h %>% filter(is.na(official_name)) %>% unique()
need_official$name

# Should be removed because exists under other name
## Bros Holding Company, LLC -- Blue River
# Should be removed because I have no idea what it is
## NEW ZEALAND LIMITED - maybe berries?
## WAIKOKOWAI FARMS LIMITED

# I have to do this because when I scraped XH in a hurry I wasn't able to track official_name, so now I need to go figure out what official name it was actually referencing
db_h <- db_h %>% 
  mutate(official_name = case_when(
    name =="Altria Client Services LLC" ~ "Altria Client Services Inc.",                                  
    name =="Bayer Cropscience Inc" ~ "Bayer CropScience LP",                                             
    name =="Bergeson Nursery LLC" ~ "Bergeson Nursery",                                                  
    #name =="Bros Holding Company, LLC" ~ "",                                                
    name =="Channel Bio LLC" ~ "Channel Bio Corp.",                                                       
    name =="Columbia County Grain Growers Inc" ~ "Columbia County Grain Growers",   
    name == "G and P Seed Co Inc" ~ "G&P Seed Company, Inc.",                                             
    #name == "Giberson Farms Inc" ~ "Giberson Farms",                                                    
    name == "GRASS MASTER CC" ~ "GrassMasters, S.A.",                                                    
    name == "Hirzel Canning Company" ~ "Hirzel Canning Co",                             
    name == "HYBRID SEED COMPANY LIMITED" ~ "Hybrid Seed Company New Zealand Ltd.",  
    name == "Jennings Turf Farms" ~ "Phillip Jennings Turf Farms, LLC",    
    name == "Kornegay Farms" ~ "Kornegay Family Farms, LLC",      
    name == "London Springs Inc"  ~ "London Spring Farms LLC", 
    name == "Mars Food Europe BV"  ~ "Mars Food Europe CV",                           
    #name == "NEW ZEALAND LIMITED" ~ "",    MAYBE New Zealand Berryfruit Propagators Ltd.
    name == "Nicks Organic Farm LLC" ~ "Nick's Organic Farm LLC", 
    name == "Northwest Grain Growers Inc" ~ "Northwest Grain Growers, Inc.",
    name == "Norton Creek Farms Inc" ~ "Norton Creek Farms",        
    name == "Rineridge Farms Inc" ~ "Rine Ridge Farms, Inc.",                                             
    name == "RM Crowe Holding LP" ~ "RM Crowe Equipment Leasing LLC",   
    name == "Sociedad Agricola Pehuen de Curico Limitada" ~ "Viverosur Nursery",      
    name == "Van Well Nursery Inc" ~ "VanWell Nursery",                                                   
    name == "Schillinger Seed Co" ~ "Schillinger Seed Inc",    
    name == "Brittingham Farms Inc" ~ "Brittingham Plant Farms",   
    name == "Sidhu & Sons Nursery Ltd" ~ "Sidhu & Sons Nursery LTD (Canada)",
    name == "Pasley's Grain Seed & Feed LLC" ~ "Pasley's Grain, Seed and Feed LLC",              
    name == "Maughan Farms" ~ "Maughan Farms, Inc.",   
    name == "Golden Peanut Company LLC" ~ "Golden Peanut Company",    
    name == "Valley Agronomics LLC" ~ "Valley Agronomics, LLC",  
    name == "Mid Columbia Producers Inc" ~ "Mid-Columbia Producers Inc",          
    name == "J+m Orchards Inc" ~ "J & M Orchards",                                                       
    name == "Clarkson Grain Company Inc" ~ "Clarkson Grain Co Inc",    
    name == "Herd Farms" ~ "Herd Farms",                                                               
    name == "HMCLAUSE" ~ "HM Clause",                                                                
    name == "Washington Fruit & Produce Co" ~ "Washington Fruit & Produce LLC",                 
    name == "Pomeroy Grain Growers Inc" ~ "Pomeroy Grain Growers, Inc.",
    name == "Mountain View Orchards LLC" ~ "Mountain View Orchards Corporation",   
    name == "Bejo Seeds Inc" ~ "Bejo Seeds, Inc.",   
    #name == "WAIKOKOWAI FARMS LIMITED" ~ "",                                                 
    name == "DUTOIT AGRI (PTY) LTD" ~ "Dutiot Group, Ltd.",  
    name == "Valley Roz Orchards Inc" ~ "Valley Roz Orchards, LP",
    name == "The Burchell Nursery Inc" ~ "Burchell Nursery, Inc.",    
    name == "Jackson S Farming Company of Autryville" ~ "Jackson's Farming Company",   
    name == "R & N Aebi Farms LLC" ~ "Aebi Farms",                                                     
    name == "Technology Crops LLC" ~ "Technology Crops LLC",   
    name == "Cargill Incorporated" ~ "Cargill, Inc.",                                                    
    name == "The McGregor Company" ~ "The McGregor Company",                                              
    name == "Legacy Fruit Packers LLC" ~ "Legacy Fruit Packers, LLC",
    name == "Stockmen's Supply Inc" ~ "Stockmen's Supply Inc.", 
    name == "Saatzucht Donau GesmbH & Co KG" ~ "Saatzucht Donau GesmbH & CoKG",                           
    name == "KOBAYASHI NURSERYCO LTD" ~ "Kobayashi Nursery Co.",   
    name == "Salish Coast Enterprises LLC" ~ "Skagit Valley Malting",   
    name == "Willcross Seed Inc" ~ "Willcross Seed",                                                     
    name == "Rupp Seeds Inc"  ~ "Rupp Seeds, Inc.",      
    name == "Agspring Idaho LLC" ~ "Thresher Artisan",
    name == "Washington State Crop Improvement Association" ~ "Washington State Crop Improvement Association (WSCIA)",                          
    name == "Harris Moran Seed Company" ~ "Harris Moran Seed Company",
    name == "Wellman Seeds Inc" ~ "Wellman Seeds, Inc",     
    name == "Primeland Cooperatives" ~ "Primeland Cooperatives",                                          
    name == "Wannamaker Seeds International LLC" ~ "Wannamaker Seeds",     
    name == "Perdue Agribusiness LLC" ~ "Perdue AgriBusiness LLC",   
    name == "Helena Agri-Enterprises LLC" ~ "Helena Agri-Enterprises",
    name == "Corteva Agriscience LLC" ~ "Corteva Agriscience",                                            
    name == "SEVEN STAR FRUITS PRIVATE LIMITED" ~ "Seven Star Fruits Pvt. Ltd.",  
    name == "Norfarms Seeds Inc" ~ "Norfarm Seeds, Inc.",   
    name == "Centro Internacional de Mejoramiento de Maíz y Trigo Internacional AC" ~ "CIMMYT", 
    name == "Columbia Grain International LLC" ~ "Columbia Grain International", 
    name == "Brian Vculek" ~ "Brian Vculek Farm", 
    name == "Crites Seed Inc" ~ "Crites Seeds, Inc.",    
    name == "HELIX SEMENTES E MUDAS LTDA" ~ "Helix Sementes e Mudas Ltda",
    T ~ official_name)) %>% 
  filter(!(name %in% c("", "Wisconsin Crop Improvement Association",
                       "South Dakota Crop Improvement Association",
                       "Cal-Ore Seed Inc", # scraped but don't want
                       "Norcal Nursery LLC"))) %>%  # There is Inc and LLC in the database, want Inc
  # addressing some mismatch
  mutate(official_name = case_when(
    name == "Norcal Nursery Inc" ~ "NorCal Nursery", # Don't know if it is Inc or LLC
    name == "Nourse Farm" ~ "Nourse Farms, Inc.",
    name == "CHS Inc" ~ "CHS SunBasin Growers",
    official_name == "Glibert's Nursery, Inc." ~ "Gilbert's Nursery, Inc.",
    T ~ official_name
  )) 

# These are the manual adjustments that I missed
missing_manual <- anti_join(db_h, master, by = "official_name")
# These are then what is missing in the db_h
missing_db_h <- anti_join(master_links, db_h, by = "official_name") %>% select(official_name, dnb_link)

# Can I find them from the data I have from the public db?
missing_db_h <- left_join(missing_db_h, db_public, by = "official_name")
# This is what I can take from public-facing and add to h
add_to_db_h <-  missing_db_h %>% filter(!is.na(name)) %>% 
  select(name, website, industry, address, employees, revenue, 
         dnb_link, year_started, official_name, db_type) %>% 
  rename(local_employee_number = employees, local_sales_number = revenue,
         yr_founded = year_started, link = dnb_link)
db_h <- full_join(db_h, add_to_db_h)

# These are links that I didn't get from public facing and so don't have data for
still_missing <-  missing_db_h %>% filter(is.na(name))
## Could scrape again?? for address

db_h <- db_h %>% 
  # Right now this gets rid of the 3 from the dbh scrape that I don;t know what they are
  filter(!is.na(official_name))

nothing <- nolinks %>% 
  filter(is.na(state)) %>% 
  rename(address = state, link = other_link) %>% 
  select(official_name, address, link) %>% 
  mutate(db_type = "cannot find")
nolinks <- nolinks %>% 
  filter(!is.na(state)) %>% 
  rename(address = state, link = other_link) %>% 
  select(official_name, address, link) %>% 
  mutate(db_type = "no link but location")

db_full <- full_join(db_h, nolinks) %>% full_join(nothing)# 30 just cannot be found, another 119 have no db links but still have locations
table(db_full$db_type)

# Company data ---

db_full$local_sales_number <- str_remove_all(db_full$local_sales_number, "\\$|\\,")
db_full$local_sales_number <- str_replace(db_full$local_sales_number, "\\s?million\\s?", "M")
db_full$local_sales_number <- str_replace(db_full$local_sales_number, "\\s?thousand\\s?", "K")
db_full$local_sales_number <- str_replace(db_full$local_sales_number, "\\s?billion\\s?", "B")
db_full$local_sales_number <- trimws(db_full$local_sales_number)

db_full <- db_full %>% 
  mutate(local_sales_number = case_when(
    # x.xx M = xxx0000; +4 zeros
    str_detect(local_sales_number, '\\d{0,3}\\.\\d{2}M$') ~ paste0(str_remove_all(local_sales_number, "\\.|M"), "0000"),
    # x.x M = x00000; +5 zeros
    str_detect(local_sales_number, '\\d{0,3}\\.\\d{1}M$') ~ paste0(str_remove_all(local_sales_number, "\\.|M"), "00000"),
    # xxxM = xxx000000; +6 zeros
    str_detect(local_sales_number, '\\d{1,3}M$') ~ paste0(str_remove_all(local_sales_number, "\\.|M"), "000000"),
    # x.xx K = xxx0; + 1 zeros
    str_detect(local_sales_number, '\\d{1,3}\\.\\d{2}K$') ~ paste0(str_remove_all(local_sales_number, "\\.|K"), "0"),
    # x.x K = xx00; + 2 zeros
    str_detect(local_sales_number, '\\d{1,3}\\.\\d{1}K$') ~ paste0(str_remove_all(local_sales_number, "\\.|K"), "00"),
    # xK = x000; + 3 zeros
    str_detect(local_sales_number, '\\d{1,3}K$') ~ paste0(str_remove_all(local_sales_number, "\\.|K"), "000"),
    # x.xx K = xxx0; + 7 zeros
    str_detect(local_sales_number, '\\d{1,3}\\.\\d\\dB$') ~ paste0(str_remove_all(local_sales_number, "\\.|B"), "0000000"),
    # x.x K = xx00; + 8 zeros
    str_detect(local_sales_number, '\\d{1,3}\\.\\dB$') ~ paste0(str_remove_all(local_sales_number, "\\.|B"), "00000000"),
    # xK = x000000000; + 9 zeros
    str_detect(local_sales_number, '\\d{1,3}B$') ~ paste0(str_remove_all(local_sales_number, "\\.|B"), "000000000"),
    local_sales_number == "" ~ NA_character_,
    T ~ NA_character_
  ))

db_full$local_sales_number <- as.numeric(db_full$local_sales_number)


db_full$global_sales_number <- str_remove_all(db_full$global_sales_number, "\\$|\\,")
db_full$global_sales_number <- str_replace(db_full$global_sales_number, "\\s?million\\s?", "M")
db_full$global_sales_number <- str_replace(db_full$global_sales_number, "\\s?thousand\\s?", "K")
db_full$global_sales_number <- str_replace(db_full$global_sales_number, "\\s?billion\\s?", "B")
db_full$global_sales_number <- trimws(db_full$global_sales_number)

db_full <- db_full %>% 
  mutate(global_sales_number = case_when(
    # x.xx M = xxx0000; +4 zeros
    str_detect(global_sales_number, '\\d{0,3}\\.\\d{2}M$') ~ paste0(str_remove_all(global_sales_number, "\\.|M"), "0000"),
    # x.x M = x00000; +5 zeros
    str_detect(global_sales_number, '\\d{0,3}\\.\\d{1}M$') ~ paste0(str_remove_all(global_sales_number, "\\.|M"), "00000"),
    # xxxM = xxx000000; +6 zeros
    str_detect(global_sales_number, '\\d{1,3}M$') ~ paste0(str_remove_all(global_sales_number, "\\.|M"), "000000"),
    # x.xx K = xxx0; + 1 zeros
    str_detect(global_sales_number, '\\d{1,3}\\.\\d{2}K$') ~ paste0(str_remove_all(global_sales_number, "\\.|K"), "0"),
    # x.x K = xx00; + 2 zeros
    str_detect(global_sales_number, '\\d{1,3}\\.\\d{1}K$') ~ paste0(str_remove_all(global_sales_number, "\\.|K"), "00"),
    # xK = x000; + 3 zeros
    str_detect(global_sales_number, '\\d{1,3}K$') ~ paste0(str_remove_all(global_sales_number, "\\.|K"), "000"),
    # x.xx K = xxx0; + 7 zeros
    str_detect(global_sales_number, '\\d{1,3}\\.\\d\\dB$') ~ paste0(str_remove_all(global_sales_number, "\\.|B"), "0000000"),
    # x.x K = xx00; + 8 zeros
    str_detect(global_sales_number, '\\d{1,3}\\.\\dB$') ~ paste0(str_remove_all(global_sales_number, "\\.|B"), "00000000"),
    # xK = x000000000; + 9 zeros
    str_detect(global_sales_number, '\\d{1,3}B$') ~ paste0(str_remove_all(global_sales_number, "\\.|B"), "000000000"),
    global_sales_number == "" ~ NA_character_,
    T ~ NA_character_
  ))

db_full$global_sales_number <- as.numeric(db_full$global_sales_number)

db_full$assets <- str_remove_all(db_full$assets, "\\$|\\,")
db_full$assets <- str_replace(db_full$assets, "\\s?million\\s?", "M")
db_full$assets <- str_replace(db_full$assets, "\\s?thousand\\s?", "K")
db_full$assets <- str_replace(db_full$assets, "\\s?billion\\s?", "B")
db_full$assets <- trimws(db_full$assets)

db_full <- db_full %>% 
  mutate(assets = case_when(
    # x.xx M = xxx0000; +4 zeros
    str_detect(assets, '\\d{0,3}\\.\\d{2}M$') ~ paste0(str_remove_all(assets, "\\.|M"), "0000"),
    # x.x M = x00000; +5 zeros
    str_detect(assets, '\\d{0,3}\\.\\d{1}M$') ~ paste0(str_remove_all(assets, "\\.|M"), "00000"),
    # xxxM = xxx000000; +6 zeros
    str_detect(assets, '\\d{1,3}M$') ~ paste0(str_remove_all(assets, "\\.|M"), "000000"),
    # x.xx K = xxx0; + 1 zeros
    str_detect(assets, '\\d{1,3}\\.\\d{2}K$') ~ paste0(str_remove_all(assets, "\\.|K"), "0"),
    # x.x K = xx00; + 2 zeros
    str_detect(assets, '\\d{1,3}\\.\\d{1}K$') ~ paste0(str_remove_all(assets, "\\.|K"), "00"),
    # xK = x000; + 3 zeros
    str_detect(assets, '\\d{1,3}K$') ~ paste0(str_remove_all(assets, "\\.|K"), "000"),
    # x.xx K = xxx0; + 7 zeros
    str_detect(assets, '\\d{1,3}\\.\\d\\dB$') ~ paste0(str_remove_all(assets, "\\.|B"), "0000000"),
    # x.x K = xx00; + 8 zeros
    str_detect(assets, '\\d{1,3}\\.\\dB$') ~ paste0(str_remove_all(assets, "\\.|B"), "00000000"),
    # xK = x000000000; + 9 zeros
    str_detect(assets, '\\d{1,3}B$') ~ paste0(str_remove_all(assets, "\\.|B"), "000000000"),
    assets == "" ~ NA_character_,
    T ~ NA_character_
  ))

db_full$assets <- as.numeric(db_full$assets)

# DOUBLE CHECK ASSETS
db_full$local_employee_number <- as.numeric(str_remove_all(db_full$local_employee_number, "\\,"))
db_full$global_employee_number <- as.numeric(str_remove_all(db_full$global_employee_number, "\\,"))
db_full$total_employee_number <- as.numeric(str_remove_all(db_full$total_employee_number, "\\,"))

# Then there are the ones from NC that didn't make it to master, so for now I will categorize these as mistakes
mistakes <- read.csv("data_indices/mistakes.csv") %>% 
  select(official_name) %>% 
  mutate(db_type = "mistake")

# More recently I went and grabbed some of these 'left-behinds' from D&B
mistakes_db <- read.csv("data_raw/lgu_licensees_mistakes.csv")
colnames(mistakes_db)[c(1,3,14,15,17,19,20,21,22,27,31,36,37)] <- c("name", "duns", "website", "local_sales_number", "assets", "local_employee_number", "total_employee_number", "description", "type", "parent","industry", "naics_code", "naics_description")
mistakes_db$address <- paste(mistakes_db$Address.Line.1, mistakes_db$City, 
                             mistakes_db$State.Or.Province, mistakes_db$Country.Region, sep = ", ")
mistakes_db$state <- mistakes_db$State.Or.Province
mistakes_db$naics2017 <- paste(mistakes_db$naics_code, mistakes_db$naics_description, sep = " - ")
mistakes_db$currency <- "USD"
mistakes_db$corp_link <- NA
mistakes_db$yr_founded <- NA
mistakes_db$incorp_location <- NA
mistakes_db$link <- NA
mistakes_db$global_employee_number <- NA
mistakes_db$global_sales_number <- NA
mistakes_db$other_link <- NA
mistakes_db$db_type <- "hoovers_direct"

mistakes_db <- mistakes_db %>% 
  mutate(official_name = case_when(
    # cannot find: Alternative Agricultural Products, Inc
    name =="Wina, LLC" ~ "The Whitetail Institute",                                  
    name == "ZERAIM GEDERA LTD" ~ "Zeraim Gedera Seed Co.",  
    # Southern states cooperative in has several locations that are all separately listed
    name == "Vaughn Nursery LLC" ~ "Vaughn Nursery", 
    name == "Schlabach's Nursery" ~ "Schalbach Nursery",
    name == "Severn Peanut Company, Inc." ~ "Severn Peanut Company",
    name == "Somerset Cukes Inc" ~ "Virginia Fork Produce Company",
    name == "Wilco Peanut Co., Ltd." ~ "Wilco Peanut Company",
    name == "Williston Peanuts, Inc." ~ "Williston Peanut, Inc.",
    name == "Eure Seed Farms, Inc." ~ "Eure Seed Farms",
    # Cannot find: Southern Farmers Seed Cooperative
    name == "Plant Pathways, Inc." ~ "The Plant Pathways Company, Inc.",
    name == "Shingleton Farms" ~ "Shingleton Farms, Inc",
    name == "Ryes Greenhouses, LLC" ~ "Ryes Greenhouses",
    # Cannot find: Sansabar LLC
    name == "Sullivan Farms, Inc." ~ "Sullivan Farms",
    name == "Tull Hill Farms, Inc" ~ "Tull Hill Farms, Inc.",
    name == "W E Bailey" ~ "W. E. Bailey & Son, Inc.",
    name == "Wadson's Farm Ltd." ~ "Wadson's Farm",
    name == "Wf Partnership" ~ "WF Partnership",
    # Cannot find: silverfox nursery
    name == "Tarheel Native Trees" ~ "Tarheel Native Trees, Inc",
    name == "Universal Corporation" ~ "Universal Leaf Tobacco",
    name == "SUNSEEDS OOD" ~ "Sunseeds Company",
    name == "W. Atlee Burpee Company" ~ "W. Atlee Burpee Co.",
    # Cannot find Russell Halverson
    name == "Archer-Daniels-Midland Company" ~ "Archer Daniels Midland Company",
    name == "Boomkwekerij René Nicolai" ~ "Boomkwekerij Rene Nicolai n.v.",
    name == "Sino-Green Turf Co.,Ltd" ~ "Sino-Green Turf Co Ltd",
    T ~ name)) %>% 
  select(colnames(db_full))

db_full <- full_join(db_full, mistakes_db)
db_full$zipcode <- as.numeric(str_extract(db_full$address, "\\d{5}"))

us.latlong <- read.csv("~/Box/osa_networks/data_combined/zipcodes/us-zip.csv") %>% 
  mutate(country = "USA") %>% 
  select(-city, -state)
ca.latlong <- read.csv("~/Box/osa_networks/data_combined/zipcodes/ca-zip.csv") %>% 
  mutate(country = "Canada") %>% 
  select(-city, -state)
nl.latlong <- read.csv("~/Box/osa_networks/data_combined/zipcodes/nl_postal_codes.csv") %>% 
  mutate(country = "Netherlands") %>% 
  select(-city, -state)
sw.latlong <- read.csv("~/Box/osa_networks/data_combined/zipcodes/ch_postal_codes.csv") %>% 
  mutate(country = "Switzerland") %>% 
  select(-city, -state)
de.latlong <- read.csv("~/Box/osa_networks/data_combined/zipcodes/de_postal_codes.csv") %>% 
  mutate(country = "Germany") %>% 
  select(-city, -state)
fr.latlong <- read.csv("~/Box/osa_networks/data_combined/zipcodes/fr_postal_codes.csv") %>% 
  mutate(country = "France") %>% 
  select(-city, -state)
it.latlong <- read.csv("~/Box/osa_networks/data_combined/zipcodes/it_postal_codes.csv") %>% 
  mutate(country = "Italy") %>% 
  select(-city, -state)

latlong <- rbind(us.latlong, ca.latlong, nl.latlong, sw.latlong, de.latlong, fr.latlong, it.latlong)

db_full <- left_join(db_full, us.latlong, by = c("zipcode" = "postcode"))

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

location <- c()
for(j in 1:nrow(db_full)){
  for(i in 1:nrow(states)){
    if(!is.na(db_full$address[j]) &  
       (str_detect(db_full$address[j], paste0("\\s", states$abbr[i], ",")) |
        str_detect(db_full$address[j], paste0("\\b", states$abbr[i])) |
        str_detect(db_full$address[j], states$state[i]))){
      location[j] <- states$state[i]
    } else {
      next
    }
  }
}

location <- c(location, rep("", nrow(db_full) - length(location)))
db_full$state <- location
db_full$country <- ifelse(!is.na(db_full$state), "USA", db_full$country)

db_full$country <- ifelse(is.na(db_full$country), 
                             str_extract(db_full$address, countries.p), 
                          db_full$country)

db_full$domestic <- ifelse(db_full$country %in% countries, F, T)
table(db_full$domestic)

write.csv(db_full, "data_clean/company_db_full.csv", row.names = F)

summary(db_full$global_employee_number)
summary(db_full$local_employee_number)
table(db_full$db_type)
#nothing = 30; don't know at all
#no links = 119; found their website but no D&B trace, so I have their address only
#40 mistakes
#3 need to re look in D&B


