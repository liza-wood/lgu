library(tidyverse)
library(pdftools)
library(stringr)
library(zoo)
pdf <- pdf_text("~/Box/lgu/data_raw/license/Washington.pdf")

# Break down by line and turn it into a dataframe
pdf <- pdf %>% 
  read_lines() %>% 
  as.data.frame(.) %>% 
  slice(-(1:2))  %>% 
  rename(text = ".")

# ID patterns for some of the columns
c1pattern = '^\\d{3,4}\\S*\\s?\\S*\\b|^\\d{3,4}\\s\\d{3,4}\\S+\\s?\\S*\\b|^AGR\\:\\S*'
c2pattern = '\\d{1,2}\\/\\d{1,2}\\/\\d{4}'
c3pattern = 'License|Non\\‐|Exclusive|Option'
c7pattern = '\\s{2,}Seed\\s{3,}|Vegetative\\s{3,}'
c8pattern = 'College of Agricultural\\, Human\\, and|College of Engineering and Architecture|Natural Resource Sciences|Crop and Soil Sciences|Horticulture|Plant Pathology|Research Extension Center'

# Go through and identify the rows based on a few of the patterns and ID them into their groups
for(i in 1:nrow(pdf)){
  pdf$rowstart[i] = ifelse(str_detect(pdf$text[i],
                                       c1pattern), T, F)
  pdf$id[i] = ifelse(str_detect(pdf$text[i],
                                 c1pattern), seq(1:nrow(pdf))[i],
                      NA)
}

# Use this function to fill in above with the value below
pdf <- na.locf(pdf, fromLast = T) %>% 
  group_by(id) %>% 
  mutate(numbering = row_number()) %>% 
  ungroup()

ids <- unique(pdf$id)
pdf$id_new = NA
for(j in 1:nrow(pdf)){
  for(i in 1:length(ids)){
    if(pdf$id[j] == ids[i]){
      pdf$id_new[j] = seq(1:nrow(pdf))[i]
    }
  }
}

pdf <- pdf %>% 
  mutate(id = paste(id_new, numbering, sep = "-")) 

# Separate into the columns for the first line, or "lower" part, of the row
## First do those that have identifiable patterns
lower <- pdf %>% 
  filter(rowstart == T) %>% 
  select(-rowstart) %>% 
  mutate(c1 = str_extract(text, c1pattern),
         c2 = str_extract(text, c2pattern),
         c3 = str_extract(text, c3pattern),
         c4 = "",
         c7 = str_extract(text, c7pattern),
         c8 = str_extract(text, c8pattern)) %>% 
  mutate(c3 = case_when(is.na(c3) ~ "", T ~ c3),
         c7 = case_when(is.na(c7) ~ "", T ~ c7),
         c8 = case_when(is.na(c8) ~ "", T ~ c8))

## Then the middle columns without easy patterns
c56 <- lower %>% 
  mutate(text = str_remove_all(text, paste(c1pattern, c2pattern, 
                                           c3pattern, c7pattern, 
                                           c8pattern, sep = "|"))) %>% 
  select(text) %>%
  mutate(c5 = str_extract(text, '(?<=^\\s{20,45}).{5,33}')) %>% 
  mutate(c5 = case_when(
    str_detect(text, '6, 8008‐10, 8011‐3, 8012‐5') ~ 
      '6, 8008‐10, 8011‐3, 8012‐5',
    str_detect(text, 'PA7221‐1') ~ 'PA7221‐1',
    str_detect(text, '7144‐3\\)') ~ '7144‐3)',
    str_detect(text, '2004NZ151\\)') ~ '2004NZ151)',
    str_detect(text, 'Crisp" TM') ~ 'Crisp" TM',
    str_detect(text, 'Selection WSU 1162\\)') ~ 'Selection WSU 1162)',
    str_detect(text, 'WSU‐910A "Peridot Cougar"') ~ 'WSU‐910A "Peridot Cougar"',
    T ~ c5))
  
c5pattern <- select(c56, c5) %>% 
  mutate(c5 = trimws(c5)) %>% 
  filter(!is.na(c5) & c5 != "") %>% 
  unique()

c5pattern$c5 <- str_replace_all(c5pattern$c5, "\\(", "\\\\\\(")
c5pattern$c5 <- str_replace_all(c5pattern$c5, "\\)", "\\\\\\)")
c5pattern$c5 <- str_replace_all(c5pattern$c5, '\\"', '\\\\\\"')

c5patternc <- paste(c5pattern$c5, collapse = "|") 

c56 <- c56 %>% 
  mutate(c6 = case_when(
    str_detect(trimws(text),c5patternc) ~ 
      str_replace(text, c5patternc, ""),
    T ~ text
  )) %>% 
  mutate(c5 = trimws(c5),
         c6 = trimws(c6))

## Combine
rplNA <- function(x){ifelse(is.na(x), "", x)}
lower <- cbind(lower, c56) %>% 
  select(id, numbering, id_new, c1, c2, c3, c4, c5, c6, c7, c8) 
lower[,4:11] <- sapply(lower[,4:11], rplNA)

# Upper parts
upper <- pdf %>% 
  filter(rowstart == F) %>% 
  select(-rowstart) %>% 
  mutate(c1 = str_extract(text, c1pattern),
         c2 = str_extract(text, c2pattern),
         c3 = str_extract(text, c3pattern),
         c7 = str_extract(text, c7pattern),
         c8 = str_extract(text, c8pattern)) %>% 
  mutate(c1 = case_when(is.na(c1) ~ "", T ~ c1),
         c2 = case_when(is.na(c2) ~ "", T ~ c2),
         c3 = case_when(is.na(c3) ~ "", T ~ c3),
         c7 = case_when(is.na(c7) ~ "", T ~ c7),
         c8 = case_when(is.na(c8) ~ "", T ~ c8)) %>% 
  mutate(c1 = trimws(c1),
         c2 = trimws(c2),
         c3 = trimws(c3),
         c7 = trimws(c7),
         c8 = trimws(c8))

## Then the middle columns without easy patterns
c4pattern = '\\b\\d{4}\\s|Plant Var\\‐\\d{2}\\/\\d{4}|\\d{4}\\‐\\S{2,6}\\‐OC|\\d{4}\\‐OC'
c4 <- upper %>% 
  mutate(text = str_remove_all(text, paste(c1pattern, c2pattern, 
                                           c3pattern, c7pattern, 
                                           c8pattern, sep = "|"))) %>% 
  select(id, text) %>% 
  mutate(text = trimws(text)) %>% 
  mutate(c4 = str_extract(text, c4pattern)) %>% 
  select(-text, -id) %>% 
  mutate(c4 = case_when(is.na(c4) ~ "", T ~ c4))

before <- paste(c("Fleming's Nurseries &", "Associates Pty\\. Ltd.",
            "\\(Graham's Factree Pty. Ltd.\\)", "Spooner Farms",
            "Tri‐State Seed Company, LLC", "Sakuma Brothers, Inc\\.",
            "USDA \\‐ Agricultural Research", 
            "Lassen Canyon Nursery, Inc\\.", "Sidhu & Sons Nursery USA",
            "Norcal Nursery, LLC", "Northwest Plant Company",
            "Scenic Hill Farm & Nursery", "Skagit Farmers Supply",
            "Skagit", "Graham's Factree PTY Ltd",
            "Nelson", "Nelson Seed", "Vineland Research and",
            "Fruit Improvement Co\\., Ltd\\."), 
            collapse = "|")
c6names <- c("Obstgenossenschaften Gen.", "of California Inc. (EMCO CAL)", "LLC", "Feed LLC", "Association", "Associates Pty. Ltd.", "Company", "(now part of PNW Coop)", "Cooperative", "River Division", "Venture")

c56 <- upper %>% 
  select(id, text) %>% 
  mutate(text = str_remove_all(text, paste(c4pattern, c1pattern, 
                                           c2pattern, c3pattern,
                                           c7pattern, c8pattern, 
                                           sep = "|"))) %>% 
  select(text) %>%
  # Remove leading
  mutate(text = str_remove_all(text, '^\\s*')) %>% 
  # Then take anything before two more spaces
  mutate(c5 = str_extract(text, '^.{3,33}(?=\\s{2,})|^.{3,33}$'),
         c5 = case_when(
           is.na(c5) & str_detect(text, before) ~
             str_extract(text, paste0('.*(?=', before, ')')),
           T ~ c5
         )) %>% 
  mutate(c5 = case_when(
    str_detect(text, "Wheat Variety 'Tekoa' \\(WA8189\\),") ~ 
      "Wheat Variety 'Tekoa' (WA8189),",
    str_detect(text, "Wheat Variety 'Ryan' \\(WA8214\\),") ~ 
      "Wheat Variety 'Ryan' (WA8214),",
    str_detect(text, "Wheat Variety 'Purl' \\(WA8234\\),") ~ 
      "Wheat Variety 'Purl' (WA8234),",
    str_detect(text, "Wheat Variety 'Puma' \\(WA8134\\),") ~ 
      "Wheat Variety 'Puma' (WA8134),",
    str_detect(text, "Wheat Variety 'Otto' \\(WA8092\\),") ~ 
      "Wheat Variety 'Otto' (WA8092),",
    str_detect(text, "Wheat Variety 'Melba' \\(WA8193\\),") ~ 
      "Wheat Variety 'Melba' (WA8193),",
    str_detect(text, "Wheat Variety 'Jasper' \\(WA8169\\),") ~ 
      "Wheat Variety 'Jasper' (WA8169),",
    str_detect(text, "Wheat Variety 'Glee' \\(WA8074\\),") ~ 
      "Wheat Variety 'Glee' (WA8074),",
    str_detect(text, "Wheat Variety 'Chet' \\(WA8165\\),") ~ 
      "Wheat Variety 'Chet' (WA8165),",
    str_detect(text, "Wheat Variety 'Alum' \\(WA8166\\),") ~ 
      "Wheat Variety 'Alum' (WA8166),",
    str_detect(text, "Waxy‐Pen, A Waxy Spring Wheat") ~ 
      "Waxy‐Pen, A Waxy Spring Wheat",
    str_detect(text, "Wheat Variety 'Resilience CL\\+'") ~ 
      "Wheat Variety 'Resilience CL+'",
    str_detect(text, 'WA8184, "Earl", semi‐dwarf hard') ~ 
      'WA8184, "Earl", semi‐dwarf hard',
    str_detect(text, "Two‐gene Clearfield spring club") ~ 
      "Two‐gene Clearfield spring club",
    str_detect(text, "PA7201‐1, PA63‐365, PA7005‐2,") ~ 
      "PA7201‐1, PA63‐365, PA7005‐2,",
    str_detect(text, "Fritz \\(Previous name\\: Richard\\) \\(2‐") ~ 
      "Fritz (Previous name: Richard) (2‐",
    str_detect(text, "Edamame \\(green soybean\\) Line") ~ 
      "Edamame (green soybean) Line",
    str_detect(text, "double bond of vegetable oil and") ~ 
      "double bond of vegetable oil and",
    str_detect(text, "Castella Soft White Club Wheat") ~ 
      "Castella Soft White Club Wheat",
    str_detect(text, "Cascade Delight \\(Raspberry WSU") ~ 
      "Cascade Delight (Raspberry WSU",
    str_detect(text, "Barley Variety 'Survivor' \\(07M‐") ~ 
      "Barley Variety 'Survivor' (07M‐",
    str_detect(text, "Barley Variety 'Muir' \\(07WA‐") ~ 
      "Barley Variety 'Muir' (07WA‐",
    str_detect(text, "Barley Variety 'Elise' \\(04WA‐") ~ 
      "Barley Variety 'Elise' (04WA‐",
    str_detect(text, 'Apple Cultivar \\(WA 38\\) "Cosmic') ~ 
      'Apple Cultivar (WA 38) "Cosmic',
    str_detect(text, "601\\.6\\), Two‐row Spring Barley") ~ 
      "601.6), Two‐row Spring Barley",
    str_detect(text, "113\\.22\\), Two‐row Spring Barley") ~ 
      "113.22), Two‐row Spring Barley",
    str_detect(text, "\\(4J071366‐1\\), Winter Club Wheat") ~ 
      "(4J071366‐1), Winter Club Wheat",
    str_detect(text, "A novel method converting the") ~ 
      "A novel method converting the",
    T ~ c5)) %>% 
  mutate(c5 = trimws(c5),
    c5 = case_when(
    c5 %in% c6names ~ "",
    T ~ c5
  ))

c5pattern <- select(c56, c5) %>% 
  mutate(c5 = trimws(c5)) %>% 
  filter(!is.na(c5) & c5 != "") %>% 
  unique() %>% 
  filter(c5 != "Wheat") # removing this because it messes with my pattern, add at end
c5pattern <- rbind(c5pattern, "Wheat")

c5patternc <- paste(c5pattern$c5, collapse = "|") 

c5patternc <- str_replace_all(c5patternc, "\\(", "\\\\\\(")
c5patternc <- str_replace_all(c5patternc, "\\)", "\\\\\\)")
c5patternc <- str_replace_all(c5patternc, '\\"', '\\\\\\"')
c5patternc <- str_replace_all(c5patternc, "\\+", "\\\\\\+")
c5patternc <- str_replace_all(c5patternc, "\\.", "\\\\\\.")

c56 <- c56 %>% 
  mutate(c6 = case_when(
    str_detect(trimws(text),c5patternc) ~ 
      str_replace_all(text, c5patternc, ""),
    T ~ text
  )) %>% 
  mutate(c5 = trimws(c5),
         c6 = trimws(c6))
look <- unique(c56)
## Combine
rplNA <- function(x){ifelse(is.na(x), "", x)}
upper <- cbind(upper, c4, c56) %>% 
  select(id, numbering, id_new, c1, c2, c3, c4, c5, c6, c7, c8) 
upper[,4:11] <- sapply(upper[,4:11], rplNA)

df <- full_join(lower, upper)

df <- df %>% 
  arrange(id_new, numbering) %>% 
  group_by(id_new) %>% 
  summarize(c1 = paste(unique(c1),collapse=' '),
            c2 = paste(unique(c2),collapse=' '),
            c3 = paste(unique(c3),collapse=' '),
            c4 = paste(unique(c4),collapse=' '),
            c5 = paste(unique(c5),collapse=' '),
            c6 = paste(unique(c6),collapse=' '),
            c7 = paste(unique(c7),collapse=' '),
            c8 = paste(unique(c8),collapse=' '))

# One issue
df <- df %>% 
  mutate(c6 = trimws(c6)) %>% 
  mutate(c6 = case_when(c6 == "Skagit" ~ "Skagit Horticulture", T ~ c6),
         c8 = case_when(c6 == "Skagit Horticulture" ~ 
                          "Natural Resource Sciences", T ~ c8)) %>% 
  mutate(c6 = case_when(str_detect(c6,"Columbia Grain International") ~ 
                          "Columbia Grain International",
                        str_detect(c6, "Nelson") ~ 
                          "Nelson Seed", 
                        str_detect(c6, "CPC Lower Valley Farms,LLC") ~
                          "CPC Lower Valley Farms,LLC",
                        str_detect(c6, "Crop Production Services") ~
                          "Crop Production Services",
                        str_detect(c6, "Homestead Family Grain, Co.") ~ 
                          "Homestead Family Grain, Co.", T ~ c6)) %>% 
  select(c5, c8, c6, c3, c2, c4) 

rpldash <- function(x){
  str_replace_all(x, '‐', '-')
}
df <- df %>% 
  mutate(across(c(c5,c6,c3,c4), rpldash)) %>% 
  mutate(across(c(c5,c6,c3,c4), trimws))

colnames(df) <- c("invention_name", "department", "licensee", 
                   "agreement_type", "effective_date",
                   "reference_id")
write.csv(df, "~/Box/lgu/data_raw/license/Washington.csv", row.names = F)

         