# Awards
library(stringr)
library(dplyr)
# Set working director to Box
setwd("~/Box/lgu")
awards <- read.csv("data_raw/university_awards/California_awards.csv",
                   skip = 2)
breeding <- awards %>% 
  filter(Admin.Dept == "Plant Sciences") %>% 
  filter(str_detect(Project.Title, "breed|seed|germplasm"))

breeding %>% 
  mutate(Total = as.numeric(str_remove_all(Total, "\\(|\\)|\\$|\\,"))) %>% 
  group_by(Sponsor.Type) %>% 
  summarize(Total = sum(Total, na.rm = T)) %>% 
  arrange(Total)

# Crop master list from licenses
crops <- read.csv("crop.csv")
morecrops <- "walnut|rosaceae|armillaria|strawberry|pistachio|grape|almond|legume|forage|cassave|pine|garbanzos|juglans|pima"
# juglans is walnut
# pima is bunchgrass
crop <- paste(crops$crop_name_common, collapse = "|")
family <- paste(crops$crop_name_scientific..genus.species., collapse = "|")
crops <- tolower(paste(crop, family, morecrops, sep = "|"))

breeding <- breeding %>% 
  mutate(crop = str_extract(tolower(Project.Title), crops))
