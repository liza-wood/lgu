library(googlesheets4)
library(tidyverse)
url <- 'https://docs.google.com/spreadsheets/d/16MttMYLRf0Pf19r5O7szf5MAV2rnbOCqrs4RZFFSlZs/edit#gid=0'

## Identify we the codes we landed on by taking newer first
## A did a unique set
a_new <- read_sheet(url, sheet = 2)
colnames(a_new)[8:ncol(a_new)] <- paste0(rep(c("theme", "text"), 10), rep(1:10, each = 2))
a_new <- pivot_longer(a_new, cols = seq(8, ncol(a_new), by = 2), names_to = 'theme_number', values_to = 'theme') %>% 
  pivot_longer(cols = text1:text10, names_to = 'text_number', values_to = 'text') %>% 
  filter(str_extract(theme_number, '\\d+') == str_extract(text_number, '\\d+')) %>% 
  filter(!is.na(text))
table(a_new$theme)

## V did a unique set
v_new <- read_sheet(url, sheet = 3)
colnames(v_new)[8:ncol(v_new)] <- paste0(rep(c("theme", "text"), 16), rep(1:16, each = 2))
v_new <- pivot_longer(v_new, cols = seq(8, ncol(v_new), by = 2), names_to = 'theme_number', values_to = 'theme') %>% 
  pivot_longer(cols = text1:text16, names_to = 'text_number', values_to = 'text') %>% 
  filter(str_extract(theme_number, '\\d+') == str_extract(text_number, '\\d+')) %>% 
  filter(!is.na(text))
table(v_new$theme)

new_codes <- rbind(a_new, v_new) %>% mutate(theme = tolower(theme))

table(new_codes$theme)

new_codes <- new_codes %>% 
  mutate(theme = case_when(
    theme %in% c("body_shape", "body_size") ~ "body_size/shape",
    theme %in% c("breeding_lines_ge", "reproduction", "reproduction/breeding") ~ "breeding/reproduction",
    theme %in% c("climate", "climate_conditions") ~ "climate/growth_conditions",
    theme %in% c("flower_shape", "flower_size") ~ "flower_size/shape",
    theme %in% c("fruit_shape", "fruit_size") ~ "fruit_size/shape",
    theme == "genetic_manipulation" ~ "genetic_manipulation/description",
    theme %in% c("seasonality/growth_rate", "growth_rate") ~ "growth_rate/season",
    theme %in% c("leaf_shape", "leaf_size") ~ "leaf_size/shape",
    theme == "seed_color" ~ "fruit_color", # this is for grain
    T ~ theme
  ))

table(new_codes$theme)

## Older codes -- only need Adams because they did duplicate

a_p <- read_sheet(url, sheet = 5)
colnames(a_p)[8:ncol(a_p)] <- paste0(rep(c("theme", "text"), 15), rep(1:15, each = 2))
a_p <- pivot_longer(a_p, cols = seq(8, ncol(a_p), by = 2), names_to = 'theme_number', values_to = 'theme') %>% 
  pivot_longer(cols = text1:text15, names_to = 'text_number', values_to = 'text') %>% 
  filter(str_extract(theme_number, '\\d+') == str_extract(text_number, '\\d+')) %>% 
  filter(!is.na(text))
table(a_p$theme)

a_p <- a_p %>% 
  mutate(theme = case_when(
    theme %in% c("body_shape", "body_size") ~ "body_size/shape",
    theme == "reproduction" ~ "breeding/reproduction",
    theme == "climate" ~ "climate/growth_conditions",
    theme %in% c("flower_shape", "flower_size") ~ "flower_size/shape",
    theme %in% c("fruit_shape", "fruit_size") ~ "fruit_size/shape",
    theme == "genetic_manipulation" ~ "genetic_manipulation/description",
    theme %in% c("seasonality/growth_rate", "growth_rate", 
                 "growth_habit", "seasonality", "maturity") ~ "growth_rate/season",
    theme %in% c("leaf_shape", "leaf_size") ~ "leaf_size/shape",
    theme == "nutrtion" ~ "chemical_composition", # this is for grain
    T ~ theme
  ))

table(a_p$theme)
table(unique(a_p$theme) %in% unique(new_codes$theme)) # this should all be true
# except maybe 1 because fruit smell is on the later

codes <- rbind(new_codes, a_p)
codes$id <- as.character(codes$id)
length(unique(codes$id))

codes <- codes %>% 
  mutate(theme_broad = case_when(
    str_detect(theme, "_color|_smell") ~ "aesthetic",
    str_detect(theme, "_size\\/shape|_texture") ~ "size_shape_texture",
    T ~ theme))
table(codes$theme_broad)
codes$abstract <- str_remove_all(codes$abstract, 'Ä|ò|ô|√')
saveRDS(codes, 'data/codes.RDS')

plants <- unique(codes[,c('id', 'id_edited', 'abstract')])
pps <- filter(plants, str_detect(id, '^PP'))

# Using these as examples for instructions
set.seed(16)
samp <- sample(1:nrow(pps), 5)
pps$abstract[samp]
unique(codes$theme_broad[codes$id == pps$id[samp[5]]])
samp
# Trialing:
pps$abstract[1] 
sort(unique(codes$theme_broad[codes$id == pps$id[1]]))
# Not bad, it gets climate/growth confused with growth rate/season
pps$abstract[2] 
sort(unique(codes$theme_broad[codes$id == pps$id[2]]))
# Does not get yield
pps$abstract[3] 
sort(unique(codes$theme_broad[codes$id == pps$id[3]]))
# Does not get size_shape because I think it is mistaking it for field
pps$abstract[4] 
sort(unique(codes$theme_broad[codes$id == pps$id[4]]))
# Got it right
pps$abstract[5] 
sort(unique(codes$theme_broad[codes$id == pps$id[5]]))
