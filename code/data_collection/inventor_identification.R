library(tidyverse)
setwd("~/Box/lgu")
df <- readRDS("data_clean/df_final.RDS")

# Already available in records
df_state <- df %>% filter(state == "Louisiana") %>% 
  select(state, crop_name_common, variety_name,
         inventor_first_name, inventor_last_name) 
colnames(df_state)[c(4,5)] <- c("inventor_first1", "inventor_last1")
LS <- df_state %>% unique()

df_state <- df %>% filter(state == "Connecticut") %>% 
  select(state, crop_name_common, variety_name,
         inventor_first_name, inventor_last_name) 
colnames(df_state)[c(4,5)] <- c("inventor_first1", "inventor_last1")
CT <- df_state %>% unique()

df_state <- df %>% filter(state == "Illinois") %>% 
  select(state, crop_name_common, variety_name, reference_id)
inventions <- read.csv("data_raw/inventor/recieved/Illinois.csv") %>% 
  select(reference_id, inventor_first1, inventor_last1)
IL <- left_join(df_state, inventions) %>% unique()

df_state <- df %>% filter(state == "Mississippi") %>% 
  select(state, crop_name_common, variety_name, reference_id)
inventions <- read.csv("data_raw/inventor/recieved/Mississippi.csv")
MS <- left_join(df_state, inventions) %>% unique()

df_state <- df %>% filter(state == "Montana") %>% 
  select(state, crop_name_common, variety_name, invention_name, reference_id)
inventions <- read.csv("data_raw/inventor/recieved/Montana.csv")
MT <- left_join(df_state, inventions, by = c("invention_name" = "variety_name")) %>% 
  select(-reference_id) %>% 
  unique()

df_state <- df %>% filter(state == "California") %>% 
  select(state, crop_name_common, variety_name) 
inventions <- read.csv("data_raw/inventor/recieved/California.csv")
CA <- left_join(df_state, inventions) %>% unique()

df_state <- df %>% filter(state == "Oregon") %>% 
  select(state, crop_name_common, variety_name)
inventions <- read.csv("data_raw/inventor/recieved/Oregon.csv") 
OR <- left_join(df_state, inventions) %>% unique()

write.csv(LS, "data_raw/inventor/cleaned/Louisiana.csv", row.names = F)
write.csv(CT, "data_raw/inventor/cleaned/Connecticut.csv", row.names = F)
write.csv(MS, "data_raw/inventor/cleaned/Mississippi.csv", row.names = F)
write.csv(IL, "data_raw/inventor/cleaned/Illinois.csv", row.names = F)
write.csv(MT, "data_raw/inventor/cleaned/Montana.csv", row.names = F)
write.csv(CA, "data_raw/inventor/cleaned/California.csv", row.names = F)
write.csv(OR, "data_raw/inventor/cleaned/Oregon.csv", row.names = F)
