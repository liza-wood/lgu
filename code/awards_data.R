# Awards
library(stringr)
library(dplyr)
# Set working director to Box
setwd("~/Box/lgu")
awards <- read.csv("data_raw/university_awards/California_awards.csv",
                   skip = 2)
awards %>% 
  filter(Admin.Dept == "Plant Sciences") %>% 
  filter(str_detect(Project.Title, "breed|seed")) %>% 
  mutate(Total = as.numeric(str_remove_all(Total, "\\(|\\)|\\$|\\,"))) %>% 
  group_by(Sponsor.Type) %>% 
  summarize(Total = sum(Total, na.rm = T)) %>% 
  arrange(Total)
