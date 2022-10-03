
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

total <- df %>% 
  filter(year < 2022)
total$applicant <- ifelse(str_detect(tolower(total$Applicant), 
                              "university|virginia tech|agricultural research service"), "University",
                     ifelse(str_detect(total$Applicant,
                                       "[Gg]overnment|[Aa]gency|[Dd]epartment"),
                            "Government", "Company"))


lgu_pattern <- paste(uni_names$university_name, collapse = "|")
for(i in 1:nrow(total)){
  total$university[i] <- str_extract(tolower(total$Applicant)[i], tolower(lgu_pattern))
}

crops <- read.csv("~/Box/lgu/data_indices/crop.csv")

uni <- filter(total, applicant == "University") 
rm <- c("Texas Tech University", "University Patents, Inc.", "Alabama A&M University",
        "University of Manitoba", "Southern Illinois University and Kenneth L. Diesburg",
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

uni2 <- left_join(uni, crops) %>% unique()
uni2$dup <- duplicated(uni2$Application..)


write.csv(total, "data_clean/pvpo_total.csv", row.names = F)
write.csv(uni, "data_clean/pvpo_lgu.csv", row.names = F)
