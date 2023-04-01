library(tidyverse)

# Access files from Box: 
# Set working directory to Box
list.files("~/Box/lgu/data_clean")

# Read in licensing data 
licenses <- readRDS("~/Box/lgu/data_clean/df_final.RDS")

## Overview ----
# 3 components of a ggplot:
## 1. data, 2. aesthetic mapping, 3. geometry

# First try to image what it is you want to visualize: picture it in your head and work backwards. What kinds of data so the axes? What kinds of geometries work best for those data? Are there additional mappings (color, shape, etc) that you will want to add to the plot?

## Scatterplots (geom_point) ----
# Example: A scatter plot (points) that show the relationship between year founded and global sales
colnames(licenses)
# part 1: data
ggplot(licenses)
# plus part 2. mappings
ggplot(licenses, mapping = aes(x = yr_founded, y = global_sales_number))
# part 3: layering a geometry
ggplot(licenses, mapping = aes(x = yr_founded, y = global_sales_number)) +
  geom_point()

# Improvements:
## Add a title, rename axes
## Change or add unit for y axis
## Add color/make it look prettier
licenses %>% 
  mutate(sales_million = global_sales_number/1000000) %>% 
  ggplot(mapping = aes(x = yr_founded, y = sales_million)) +
  geom_point(color = "blue", size = 3) +
  labs(title = "Licensing companies: year and sales relationship",
       x = "Year", y = "Global sales ($M)") +
  theme_minimal()





ggplot(licenses, 
       aes(x = yr_founded, y = global_sales_number)) +
  geom_point()

# What if I wanted the dots to be blue? 
ggplot(licenses, 
       aes(x = yr_founded, y = global_sales_number)) +
  geom_point(color = "darkblue")

# What if I wanted company_region to be a color
ggplot(licenses, 
       aes(x = yr_founded, y = global_sales_number)) +
  geom_point(aes(color = company_region))

## barplots (geom_bar or geom_col) ----
# Example: A bae that show the counts of licenses by state

licenses %>% 
  select(licensees, yr_founded) %>% 
  unique() %>% 
  ggplot(...)

# only needs an x argument
ggplot(licenses, aes(x = uni_state)) +
  geom_bar()

# Another way? You can pipe data into plots, which can also help you order bars
licenses %>% 
  group_by(uni_state) %>% 
  count() %>% 
  arrange(-n) %>% 
  ggplot(aes(x = factor(uni_state, levels = .$uni_state), y = n)) +
  geom_col()

# Easier to read?
licenses %>% 
  group_by(uni_state) %>% 
  count() %>% 
  arrange(-n) %>% 
  ggplot(aes(x = factor(uni_state, levels = .$uni_state), y = n)) +
  geom_col() +
  coord_flip()

## boxplots (geom_boxplot) ----
# Boxplots are good at showing distributions
# Example: The range of company size for each 'fao' crop categorization, or crop_cat
ggplot(licenses, 
       aes(x = fao_class1, y = global_sales_number)) +
  geom_boxplot()

# Apply the same logics to put them in an order

ggplot(licenses, 
       aes(x = fao_class1, y = global_sales_number)) +
  geom_boxplot() +
  coord_flip()

# filter out the NAS, non_food, medicinal, sugar and spice crops
remove_these_crops <- c("sugar_crops", "spice_crops",
                        "non_food", "medicinal")
licenses %>% 
  filter(!is.na(fao_class1)) %>% 
  filter(!(fao_class1 %in% remove_these_crops)) %>% 
  mutate(fao_class1_f = factor(fao_class1, levels = 
                                 c("cereals", "oilseeds",
                                   "root_tuber", "grasses_fodder",
                                   "veg_melon", "fruits_nuts",
                                   "leguminous"))) %>% 
  ggplot(aes(x = fao_class1_f, y = global_sales_number)) +
  geom_boxplot() +
  coord_flip()

# facets?
# facet
licenses %>% 
  filter(!is.na(fao_class1)) %>% 
  filter(!(fao_class1 %in% remove_these_crops)) %>% 
  mutate(fao_class1_f = factor(fao_class1, levels = 
                                 c("cereals", "oilseeds",
                                   "root_tuber", "grasses_fodder",
                                   "veg_melon", "fruits_nuts",
                                   "leguminous"))) %>% 
  ggplot(aes(x = fao_class1_f, y = global_sales_number)) +
  geom_boxplot() +
  coord_flip() +
  facet_wrap(~uni_state)
