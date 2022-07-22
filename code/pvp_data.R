
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

total <- df %>% 
  filter(year < 2021)
total$applicant <- ifelse(str_detect(total$Applicant, 
                                     "[Uu]niversity|Virginia Tech"), "University",
                     ifelse(str_detect(total$Applicant,
                                       "[Gg]overnment|[Aa]gency|[Dd]epartment"),
                            "Government", "Company"))
lgu_pattern <- paste(uni_names$university_name, collapse = "|")


for(i in 1:nrow(total)){
  total$university[i] <- str_extract(total$Applicant[i], lgu_pattern)
}

uni <- filter(total, !is.na(university))
write.csv(uni, "data_raw/other_ip/pvpo_university.csv", row.names = F)
table(uni$university)
table(uni$year)

ggplot(uni) +
  geom_bar(aes(x = year), stat = "count")


table(uni$Common.Name)

crops <- read.csv("crop.csv")
crop <- tolower(paste(crops$crop_name_common, collapse = "|"))
family <- tolower(paste(crops$crop_name_scientific..genus.species., collapse = "|"))
morecrops <- "field corn|lettuce|tomato|fescue|watermelon"

total <- total %>% 
  mutate(crop = case_when(
    str_detect(tolower(Common.Name), crop) == T ~ 
                 str_extract(tolower(Common.Name), crop),
    str_detect(tolower(Scientific.Name), family) == T ~ 
      str_extract(tolower(Scientific.Name), family),
    T ~ "X Not yet"))

uni <- total %>% 
  filter(applicant == "University")



# ------
total %>% 
  group_by(applicant) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(Percent = round(100*(n/sum(n)), 1))

ggplot(total, aes(x = factor(year), fill = applicant)) +
  geom_bar() +
  #stat_count(geom = "text", colour = "white", size = 2,
  #              aes(label = ..count..),
  #              position=position_stack(vjust=0.5))
  theme_linedraw() +
  labs(y = "Count", x = "Year", fill = "Applicant",
       title = "Number of PVP certificates 2011-2021") +
  scale_fill_manual(values = colors9[c(1,3,8)]) +
  theme(plot.title = element_text(hjust = 0.5, size = 10)) 

ggsave("~/Desktop/pvp.png", width = 5.5, height = 3)
private <- anti_join(total, public)
nrow(public)/nrow(total)

table(total$Common.Name)
total$crop <- tools::toTitleCase(tolower(total$Common.Name))
table(total$crop)
## There will be one cell just called Berries
minorberry <- c("Black chokeberry", "Black raspberry", "Brambleberry")
majorberry <- c("Blackberry", "Blueberry", 
                "Red raspberry", "Strawberry", "Berries",
                "Raspberry")
forage <- c("Alfalfa", "White Clover", "Wheatgrass, Thickspike",
            "Wheatgrass", "Vetch, Hairy",
            "Vetch, Chickling", "Trefoil, Birdsfoot",
            "Timothy", "Sunn Hemp", "Clover",
            "Clover, Crimson", "Clover, Red",
            "Clover, White", "Pea, Forage",
            "Radish, Fodder", "Meadowbrome",
            "Orchardgrass", "Brome, Meadow",
            "Bromegrass, Smooth")
lawngrass <- c("Bermuda grass", "Bermuda grass hybrid",
               "Bermudagrass",
               "Creeping Bentgrass", "Bentgrass",
               "Lawngrass",
               "St. Augustine grass", "Zoysia",
               "Seashorepaspalum", "Seashore Paspalum",
               "Ryegrass", "Annual Bluegrass",
               "Bahiagrass", "Fescue")
orngrass <- c("Crowngrass", "Elephant grass", 
              "Meadow Bromegrass", 
              "Prairie cord grass", "Switchgrass", 
              "Timothy grass",
              "Smooth cordgrass",
              "Tufted Hairgrass",
              "Saltmarsh Grass",
              "Big Bluestem",
              "Brachiariax")
ornshrub <- c("Bay Cherry","Butterfly bush", 
              "Crape myrtle",  "Indian shot",
              "Japanese barberry", "Leadtree", 
              "Lily of the Incas", "Pincushions",
              "Prayer plant", "Rhododendron", 
              "Sand cherry", "Sweet fern", "Sweetgale",
              "Witherod Viburnum", "Eastern White Cedar",
              "Magnolia", "Coleus")
grape <- c("Grapevine",
           "Grape")
nuttree <- c("Almond", "Walnut", "Walnut rootstock", "Pistachio")
stonefruit <- c("Cherry (sour)", "Cherry (sweet)", "Peach",
                "Peach rootstock", "Prune Plum")
fruittree <- c("Apple", "Fig", "Papaya", "Pear", 
               "Black fig", "Orange")
fieldcrops <- c("Corn", "Cotton", "Soybean", "Field Corn",
                "Corn, Sweet")
minorgrain <- c("Barley", "Durum wheat", "Nude oats", 
                "Oat", "Pearl millet", "Sorghum", "Teff",
                "Rye", "Sea oats", "Wildrye",
                "Triticale", "Millet", "Millet, Pearl",
                "Quinoa")
majorgrain <- c("Asian rice", "Wheat", 
                "Soft red wheat", "Rice", "Long grain rice",
                "Wheat, Durum", "Buckwheat", "Durum Wheat",
                "Club Wheat")
legume <- c("Black-eyed Pea", "Dry Bean", 
            "Faba Bean", "Pea", "Peanut",
            "Bean", "Bean, Lablab", "Chickpea",
            "Cowpea", "Field Bean",
            "Field Pea", "Garden Bean", "Garden Pea",
            "Lentil", "Lima Bean",
            "Pea, Edible", "Pea, Edible-Podded",
            "Pea, Field", "Pea, Garden")
oilseed <- c("Camelina", "Safflower", "Winter canola",
             "Sesame", "Rape", "Rapeseed", "Meadowfoam",
             "Flax", "Chia", "Canola")
rootstarch <- c("Potato", "Sweet potato", "Taro",
                "Beet","Turnip", "Carrot", "Onion",
                "Radish")
vegetable <- c("Pepper",  "Tomato",
               "Swiss Chard", "Squash", "Artichoke",
               "Arugula", "Asparagus", "Broccoli",
               "Celery", "Chicory", "Cucumber",
               "Eggplant", "Endive", "Kale", "Mustard, India",
               "Mustard, White", "Mustard, Ethiopian",
               "Pumpkin", "Spinach", "Nightshade", "Lettuce")
annualfruit <- c("Watermelon", "Muskmelon")
other <- c("Stevia", "Tobacco", "Salicornia", "Hemp")
flower <- c("Zinnia", "Wallflower", "Vinca", "Verbena, Prairie",
            "Thistle", "Sunflower", "Stock, Common",
            "Sneezeweed, Annual", 
            "Anthurium", "Baby's Breath", "Calla Lily",
            "Fuchsia", "Marigold", "Orchid", "Lupin",
            "Forget-Me-not", "Phlox", "Daisy, English",
            "Daisy, Blackfoot", "China Aster")
herbs <- c("Basil", "Salvia", "Oregona", "Parsley",
           "Coriander")

total <- total %>% 
  mutate(crop = case_when(
    crop %in% minorberry ~ "Fruits",
    crop %in% majorberry ~ "Fruits",
    crop %in% forage ~ "Forage and cover crops",
    crop %in% lawngrass ~ "Ornamentals and grasses",
    crop %in% orngrass ~ "Ornamentals and grasses",
    crop %in% ornshrub ~ "Ornamentals and grasses",
    crop %in% grape ~ "Fruits",
    crop %in% nuttree ~ "Nut trees",
    crop %in% fruittree ~ "Fruits",
    crop %in% stonefruit ~ "Fruits",
    crop %in% fieldcrops ~ "Field crops",
    crop %in% minorgrain ~ "Grains",
    crop %in% majorgrain ~ "Grains",
    crop %in% legume ~ "Legumes",
    crop %in% oilseed ~ "Oil seeds",
    crop %in% rootstarch ~ "Roots and starches",
    crop %in% other ~ "Other",
    crop %in% vegetable ~ "Vegetables",
    crop %in% annualfruit ~ "Fruits",
    crop %in% flower ~ "Flowers and herbs",
    crop %in% herbs ~ "Flowers and herbs",
    T ~ "Ornamentals and grasses" # just going with grass for now
  ))
table(total$crop)

total %>% 
  filter(!(crop %in% c("Other", "Flowers and herbs"))) %>% 
  group_by(year, crop) %>% 
  count() %>% 
  ggplot(aes(x = year, y = n)) +
  geom_bar(stat = "identity") +
  geom_smooth() +
  labs(x = "Year", y = "Count", 
       title = "PVP certificates issued by crop 2011-2021") +
  facet_wrap(~crop, scale = "free_y") +
  ylim(c(0, NA)) +
  theme(plot.title = element_text(hjust = 0.5, size = 10)) 

ggsave("~/Desktop/pvp_crop.png", width = 7, height = 4)

?scale_y_continuous



