# Company cleaning script
# Bring together database_h with others to combine and make sure we have all we can
library(dplyr)
library(stringr)

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

nothing <- select(nolinks, official_name, other_link, state) %>% 
  filter(is.na(state)) %>% 
  mutate(db_type = "cannot find")
nolinks <- select(nolinks, official_name, other_link, state) %>% 
  rename(address = state, link = other_link) %>% 
  filter(!is.na(address)) %>% 
  mutate(db_type = "no link but location")
db_full <- full_join(db_h, nolinks) %>% full_join(nothing)# 30 just cannot be found, another 119 have no db links but still have locations
table(db_full$db_type)

# Then there are the ones from NC that didn't make it to master, so for now I will categorize these as mistakes
mistakes <- read.csv("data_indices/mistakes.csv") %>% 
  select(official_name) %>% 
  mutate(db_type = "mistake")
db_full <- full_join(db_full, mistakes)
write.csv(db_full, "data_clean/company_db_full.csv", row.names = F)

#nothing = 30; don't know at all
#no links = 119; found their website but no D&B trace
#40 mistakes


