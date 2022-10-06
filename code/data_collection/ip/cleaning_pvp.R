
# PVPO database
# https://www.ams.usda.gov/services/plant-variety-protection/application-status
library(stringr)
library(lubridate)
library(tidyverse)

setwd("~/Box/lgu")
uni_names <- read.csv("data_indices/universities.csv")
df <- read.csv("data_raw/other_ip/pvpo.csv")
df$year <- ifelse(df$Issued.Date == "", NA, year(mdy(df$Issued.Date)))
table(df$year)
table(is.na(df$year)) # I still want abandoned applications
df$year <- ifelse(df$Issued.Date == "", year(mdy(df$Status.Date)), df$year)

uni_pattern <- c("university|virginia tech")
other_pattern <- c("agricultural research service|the state of oregon|agricultural experiment station|agricultural and forestry experiment station|idaho research foundation|ksu research foundation|kansas state university research foundation|the state of idaho|board of trustees of michigan state university|state of oregon")

total <- df %>% 
  filter(year < 2022)
total$applicant <- ifelse(str_detect(tolower(total$Applicant), 
                                     uni_pattern), "University",
                          ifelse(str_detect(tolower(total$Applicant), 
                                            other_pattern), "University",      
                                 ifelse(str_detect(total$Applicant,
                                                   "[Gg]overnment|[Aa]gency|[Dd]epartment"),
                                        "Government", "Company")))


lgu_pattern <- paste(uni_names$university_name, collapse = "|")
for(i in 1:nrow(total)){
  total$university[i] <- str_extract(tolower(total$Applicant)[i], tolower(lgu_pattern))
}

total <- total %>% 
  mutate(university = case_when(
    Applicant %in% c("Idaho Research Foundation, Inc.",
                     "The State of Idaho") ~ "university of idaho",
    Applicant %in% c("KSU Research Foundation",
                     "Kansas State University Research Foundation") ~ "kansas state university",
    Applicant == "Louisiana Agricultural Experiment Station" ~ "louisiana state university",
    Applicant %in% c("Michigan Agricultural Experiment Station",
                     "Board of Trustees of Michigan State University") ~ "michigan state university",
    Applicant == "Minnesota Agricultural Experiment Station" ~ "university of minnesota",
    Applicant == "Mississippi Agricultural and Forestry Experiment Station" ~ "mississippi state university",
    Applicant == "Oklahoma Agricultural Experiment Station (OAES)" ~ "oklahoma state university",
    Applicant %in% c("The State of Oregon",
                     "State of Oregon, by and through the State Board of Higher Education on behalf of Oregon State Univ") ~ "oregon state university",
    T ~ university
  ))

crops <- read.csv("~/Box/lgu/data_indices/crop.csv")

uni <- filter(total, applicant == "University") 
rm <- c("Texas Tech University", 
        "University Patents, Inc.", 
        "Alabama A&M University",
        "University of Manitoba", 
        "Southern Illinois University and Kenneth L. Diesburg",
        "Virginia State University and U.S. Government, as represented by the Secretary of Agriculture",
        "Pogue Agri Partners, Inc. and Antonio Narro Autonomous Agrarian University",
        "Texas Tech University and Halliburton Energy Services, Inc.",
        "University of Saskatchewan")

uni <- uni %>% filter(!(Applicant %in% rm))
crops$crop_name_scientific <- str_remove_all(crops$crop_name_scientific, "\\.|\\?\\?")
uni_hold <- uni
for(i in 1:nrow(uni)){
  for(j in 1:nrow(crops)){
    if(str_detect(tolower(uni$Common.Name[i]), tolower(crops$crop_name_common[j])) == T &
       !is.na(str_detect(tolower(uni$Common.Name[i]), tolower(crops$crop_name_common[j])))){
      uni$crop_name_common[i] <- crops$crop_name_common[j]
    } else if (str_detect(toupper(uni$Common.Name[i]), crops$ip_name[j]) == T &
               !is.na(str_detect(toupper(uni$Common.Name[i]), crops$ip_name[j]) == T)){
      uni$crop_name_common[i] <- crops$crop_name_common[j]
    } else if (str_detect(tolower(uni$Common.Name[i]), tolower(crops$crop_name_scientific[j])) == T &
               !is.na(str_detect(tolower(uni$Common.Name[i]), tolower(crops$crop_name_scientific[j])))){
      uni$crop_name_common[i] <- crops$crop_name_common[j]
    } else {next}
  }
}

uni <- uni %>% 
  left_join(crops) %>% 
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

states <- read.csv("~/Box/lgu/data_indices/universities.csv") %>% 
  mutate(university = tolower(university_name))

uni <- left_join(uni, states) %>% 
  mutate(uni_state = case_when(
    str_detect(Applicant, "North Carolina Agricultural Research Service") ~ "North Carolina",
    Applicant == "Idaho Research Foundation, Inc." ~ "Idaho",
    Applicant == "KSU Research Foundation" ~ "Kansas",
    Applicant == "Louisiana Agricultural Experiment Station" ~ "Louisiana",
    Applicant == "Michigan Agricultural Experiment Station" ~ "Michigan",
    Applicant == "Minnesota Agricultural Experiment Station" ~ "Minnesota",
    Applicant == "Mississippi Agricultural and Forestry Experiment Station" ~ "Mississippi",
    Applicant == "Oklahoma Agricultural Experiment Station (OAES)" ~ "Oklahoma",
    Applicant == "The State of Oregon" ~ "Oregon",
    Applicant == "Arkansas Agricultural Experiment Station" ~ "Arkansas",
    Applicant == "Cornell Agricultural Experiment Station" ~ "New York",
    Applicant == "Delaware Agricultural Experiment Station" ~ "Delaware",
    Applicant == "Florida Agricultural Experiment Station" ~ "Florida",
    Applicant == "Idaho Agricultural Experiment Station" ~ "Idaho",
    Applicant == "Illinois Agricultural Experiment Station" ~ "Illinois",
    Applicant == "Kansas Agricultural Experiment Station" ~ "Kansas",
    Applicant == "Kentucky Agricultural Experiment Station" ~ "Kentucky",
    Applicant == "Maryland Agricultural Experiment Station" ~ "Maryland",
    Applicant == "Minnesota Agricultural Experiment Station and USDA-ARS" ~ "Minnesota",
    Applicant == "Montana Agricultural Experiment Station" ~ "Montana",
    Applicant == "NDSU Research Foundation,  ND Agricultural Experiment Station" ~ "North Dakota",
    Applicant == "Nebraska Agricultural Experiment Station" ~ "Nebraska",
    Applicant == "New Hampshire Agricultural Experiment Station" ~ "New Hampshire",
    Applicant == "New Jersey Agricultural Experiment Station" ~ "New Jersey",
    Applicant == "North Carolina Agricultural Experiment Station" ~ "North Carolina",
    Applicant == "North Dakota Agricultural Experiment Station" ~ "North Dakota",
    Applicant == "Oklahoma Agricultural Experiment Station and The United States Government as represented by the Secretary of Agriculture" ~ "Oklahoma",
    Applicant == "Pennsylvania Agricultural Experiment Station" ~ "Pennsylvania",
    Applicant == "Pennsylvania Agricultural Experiment Station and USDA-ARS" ~ "Pennsylvania",
    Applicant == "Rhode Island Agricultural Experiment Station" ~ "Rhode Island",
    Applicant == "South Carolina Agricultural Experiment Station" ~ "South Carolina",
    Applicant == "South Carolina Agricultural Experiment Station and USDA-ARS" ~ "South Carolina",
    Applicant == "South Dakota Agricultural Experiment Station" ~ "South Dakota",
    Applicant == "Tennessee Agricultural Experiment Station" ~ "Tennessee",
    Applicant == "Texas Agricultural Experiment Station" ~ "Texas",
    Applicant == "Texas Agricultural Experiment Station and Agriculture & Agri-Food Canada" ~ "Texas",
    Applicant == "Texas Agricultural Experiment Station/ USDA-ARS" ~ "Texas",
    Applicant == "Utah Agricultural Experiment Station" ~ "Utah",
    Applicant == "Virginia Agricultural Experiment Station" ~ "Virginia",
    Applicant == "Wisconsin Agricultural Experiment Station" ~ "Wisconsin",
    T ~ uni_state
  ))


uni$issue_year <- year(mdy(uni$Issued.Date))
colnames(uni)[1] <- "id"
uni <- filter(uni, !is.na(uni_state))
write.csv(total, "data_clean/pvpo_total.csv", row.names = F)
write.csv(uni, "data_clean/pvpo_lgu.csv", row.names = F)
