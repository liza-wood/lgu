library(tidyverse)
library(lubridate)
setwd("~/Box/lgu")
df76 <- read.csv("data_raw/other_ip/uspto_uni_plant_76_95.csv")
df96 <- read.csv("data_raw/other_ip/uspto_uni_plant_96_00.csv")
df01 <- read.csv("data_raw/other_ip/uspto_uni_plant_01_05.csv")
df06 <- read.csv("data_raw/other_ip/uspto_uni_plant_06_10.csv")
df11 <- read.csv("data_raw/other_ip/uspto_uni_plant_11_15.csv")
df16 <- read.csv("data_raw/other_ip/uspto_uni_plant_16_18.csv")
df19 <- read.csv("data_raw/other_ip/uspto_uni_plant_19.csv")
df20 <- read.csv("data_raw/other_ip/uspto_uni_plant_20.csv")
df <- rbind(df76, df96, df01, df06, df11, df16, df19, df20)
uni_names <- read.csv("data_indices/universities2.csv")

#df <- filter(df, !is.na(university))
df <- filter(df, !(str_detect(df$Title, "[Mm]ethod|[Aa]pparatus|[Pp]rocess")) &
               !(str_detect(df$Claims, "1\\. A method")))

### TRY TO ID CROPS 
#df <- read.csv("data_clean/uspto_lgus.csv") 
df$issue_year <- year(lubridate::ymd(df$Issue_Date))
crops <- read.csv("~/Box/lgu/data_indices/crop.csv")
crops$crop_name_common <- str_remove_all(crops$crop_name_common,"\\.")
crops$crop_name_scientific <- str_remove_all(crops$crop_name_scientific,"\\.")
crops$ip_name <- str_remove_all(crops$ip_name,"\\.")
df$Title <- str_remove_all(df$Title,"\\.")
df$crop_name_common <- ""

for(i in 1:nrow(df)){
  for(j in 1:nrow(crops)){
    if(str_detect(tolower(df$Title[i]), 
                  paste0(tolower(crops$crop_name_common[j]),"\\b")) == T &
       !is.na(str_detect(df$Title[i], crops$crop_name_common[j]))){
      df$crop_name_common[i] <- crops$crop_name_common[j]
    } else if(str_detect(tolower(df$Title[i]), 
                         paste0(tolower(crops$ip_name[j]),"\\b")) == T &
              !is.na(str_detect(df$Title[i], crops$ip_name[j]))) {
      df$crop_name_common[i] <- crops$crop_name_common[j]
    } else if(str_detect(tolower(df$Title[i]), 
                         paste0(tolower(crops$crop_name_scientific[j]),"\\b")) == T &
              !is.na(str_detect(df$Title[i], crops$crop_name_scientific[j]))){
      df$crop_name_common[i] <- crops$crop_name_common[j]
    }else {next}
  }
}

# Manually ID the crop common names
df$crop_name_common[df$WKU == "PP0056529"] <- "Asparagus"
df$crop_name_common[str_detect(df$Title, "[Gg]rass (named)? ‘Tift")] <- "Fountaingrasses"
df$crop_name_common[df$WKU == "PP028194"] <- "Little bluestem"
df$crop_name_common[df$WKU == "PP028193"] <- "Little bluestem"
df$crop_name_common[df$WKU == "PP028033"] <- "Little bluestem"
df$crop_name_common[df$WKU == "PP0067270"] <- "Norway maple"
df$crop_name_common[df$WKU == "PP0099821"] <- "Indian hawthorn"
df$crop_name_common[df$WKU == "PP029844"] <- "Beach evening primrose"
df$crop_name_common[df$WKU == "PP0069760"] <- "Peach"
df$crop_name_common[df$WKU == "PP0069124"] <- "Peach"
df$crop_name_common[df$WKU == "PP0075345"] <- "Forsythia"
df$crop_name_common[str_detect(df$Title, "Barberry")] <- "Japanese barberry"
# Need to add Hop and Nandina to the crop list

checking_pp <- filter(df, crop_name_common == "" & str_detect(WKU, "^PP"))
checking_up <- filter(df, crop_name_common == "" & !(str_detect(WKU, "^PP")))

# Plant patents definitely stay, but how to get utility patents
df_plants <- filter(df, crop_name_common != "")

df_plants <- df_plants %>% 
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

# Fix something with Rugers and Texas A&M -- want to ID the school
# Choose only LGUs
lgu_pattern <- paste(uni_names$uni_name, collapse = "|")
mistakes <- paste(c("University of Minnestoa", "University of Akransas", 
                    "University of Arkanas",
                    "University of Akranas", "Texas A \\& M University",
                    "University of Tennesse", "University of Ga", 
                    "University of Georgiea", "North Carollna State University",
                    "North Carolina State Univ\\. at Raleigh",
                    "Regents of the University of Minn\\.",
                    "Rutger", "Regents U\\.C\\.",
                    "Universtiy of Massachusetts"), collapse = "|")
lgu_pattern <- paste(lgu_pattern, mistakes, sep = "|")
lgu_pattern <- str_replace_all(lgu_pattern, "\\bof\\b", "[Oo]f")
lgu_pattern <- tolower(lgu_pattern)

for(i in 1:nrow(df_plants)){
  df_plants$university[i] <- str_extract(tolower(trimws(df_plants$Assignee[i])), lgu_pattern)
}

table(df_plants$university)

# Manually correct the mistakes
df_plants <- df_plants %>% 
  mutate(university = case_when(
    university %in% c("university of minnestoa",
                      "regents of the university of minn.") ~ "university of minnesota",
    university %in% c("university of arkanas",
                      "university of akransas", 
                      "university of akranas") ~ "university of arkansas",
    university == "texas a & m university" ~ "texas a&m",
    university == "university of tennesse"~ "university of tennessee",
    university == "universtiy of massachusetts"~ "university of massachusetts",
    university %in% c("university of ga",
                      "university of georgiea") ~ "university of georgia",
    university %in% c("north carollna state university",
                      "north carolina state univ. at raleigh") ~ "north carolina state university",
    university == "rutger" ~ "rutgers university",
    university == "regents u.c." ~ "university of california",
    T ~ university
  ))

table(df_plants$university)

# These are the assignees that I couldn't ID a university for
# Do I change up NC ARS or Cornell Foundation?
unique(df_plants$Assignee[is.na(df_plants$university)])

states <- uni_names %>% 
  mutate(university = tolower(uni_name))

df_plants <- left_join(df_plants, states) %>% 
  filter(!is.na(uni_state))

colnames(df_plants)[1] <- "id"

write.csv(df_plants, "data_clean/uspto_lgus.csv", row.names = F)
