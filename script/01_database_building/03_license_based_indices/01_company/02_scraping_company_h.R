# List 1-25 doesn't have locations of corporate links
# Taking existing list and manually scraping from D&B using lists
# Could not find: Ashton Hi-Tech Seed Co., Inc. ; Bio Plant Research Ltd ;

# Different: Blue River Hybrids, L.L.C. is Blue River Hybrids, L.L
# Certis USA LLC is Certis U.S.A. L.L.C.
# Clarkson Grain Co Inc is Clarkson Grain Company or something

library(rvest)
library(httr)
library(xml2)
require(RSelenium)
require(wdman)
library(dplyr)
library(stringr)
library(data.table)
library(tidyr)
# Set working director to Box
setwd("~/Box/lgu")

existing <- read.csv("data_clean/company_db.csv") %>% filter(!is.na(name))
existing$group = "old"
existing_multi <- read.csv("data_clean/company_db_multi.csv") %>% filter(!is.na(name)) %>% 
  mutate(duplicate = duplicated(name)) %>% 
  filter(duplicate == F) %>% 
  select(-duplicate) %>% 
  mutate(group = "old")
company <- read.csv("data_clean/company_db_2.csv") %>% filter(!is.na(name)) %>% 
  mutate(group = "new")

hoovers <- rbind(existing, existing_multi, company)
hoovers$city <- str_extract_all(hoovers$address, "(?<=St |Rd |Ln |Ave |[A-Z]{1,2} )[A-Z][a-z]+\\s[A-Z][a-z]+|[A-Z][a-z]+(?=,\\s[A-Z][A-Z],\\s\\d+)")
# This is US and Canada
hoovers$country <- str_extract_all(hoovers$address, "United States|Canada|Australia|New Zealand|South Africa|France|Italy|Germany|United Kingdom|Denmark|Norway|Brazil|Chile|Bulgaria")

for(i in 1:nrow(hoovers)){
  if(lengths(hoovers$city[i]) > 1){
    hoovers$city[i] <- hoovers$city[i][[1]][1]
    hoovers$city[i] <- as.character(hoovers$city[i])
  } else if(lengths(hoovers$city[i]) == 0){
    hoovers$city[i] <- NA
  } 
  hoovers$city[i] <- unlist(hoovers$city[i])
}

lengths(hoovers$city[4])

for(i in 1:nrow(hoovers)){
  if(lengths(hoovers$country[i]) > 1){
    hoovers$country[i] <- hoovers$country[i][[1]][1]
    hoovers$country[i] <- as.character(hoovers$country[i])
    hoovers$country[i]  <- unlist(hoovers$country[i])
  } else if(lengths(hoovers$country[i]) == 0){
    hoovers$country[i] <- NA
  } 
  hoovers$country[i] <- unlist(hoovers$country[i])
}

hoovers <- select(hoovers, name, address, city, country, group)
hoovers <- as.data.frame(hoovers)
hoovers <- hoovers[order(hoovers$name),]
hoovers$id <- 1:nrow(hoovers)
data.table::fwrite(hoovers, "~/Desktop/companies.csv")
# Then I manually cleaned and loaded them into D&B to get their link, not all matched

aj <- read.csv("~/Desktop/dnb/companies_a_j_dnblinks.csv")
kr <- read.csv("~/Desktop/dnb/companies_k_r_dnblinks.csv")
sz <- read.csv("~/Desktop/dnb/companies_s_z_dnblinks.csv")

# First round are those that could be automatically ID'd in the db
hoovers1 <- rbind(aj, kr, sz)

# Second round are those that I went and got links for one by one
hoovers2 <- fread("~/Desktop/dnb/nomatch_found_manual.csv")

# I ran each through as hoovers and saved as db

rD <- rsDriver(browser = "firefox", port = 4445L)
# This should open a firefox browser
remDr <- rD[["client"]]
remDr$deleteAllCookies()
remDr$close()
rD$server$stop()
rm(rD, remDr)
gc()

rD <- rsDriver(browser = "firefox", port = 4445L)
# This should open a firefox browser
remDr <- rD[["client"]]
remDr$getAllCookies()

login <- "https://app.dnbhoovers.com/login"
remDr$navigate(login)

hoovers_db2 <- data.table()
for(i in 1:nrow(hoovers)){
  #id <- hoovers$id[i]
  official_name <- hoovers$name_link[i]
  url <- hoovers$link[i]
  remDr$navigate(url)
  if(i %% 3 == 0) {Sys.sleep(41)}
  else if(i %% 12 == 0) {Sys.sleep(65)}
  else if(i %% 2 == 0) {Sys.sleep(32)}
  else {Sys.sleep(49)}
  html <- remDr$getPageSource()[[1]]
  html <- read_html(html)
  name <- html %>% 
    xml_find_all('//*[contains(concat( " ", @class, " " ), concat( " ", "rpt-company-name", " " ))]//*[contains(concat( " ", @class, " " ), concat( " ", "ant-typography", " " ))]') %>% 
    html_text()
  type <- html %>% 
    xml_find_all('//*[contains(concat( " ", @class, " " ), concat( " ", "rpt-company-address", " " ))]//span[(((count(preceding-sibling::*) + 1) = 1) and parent::*)]') %>% 
    html_text()
  internal_deets <- html %>% 
    xml_find_all('//*[contains(concat( " ", @class, " " ), concat( " ", "paragraph-med", " " ))]') %>% 
    html_text()
  employees <- html %>% 
    xml_find_all('//*[contains(concat( " ", @class, " " ), concat( " ", "rpt-company-employees-this-site", " " ))]') %>% 
    html_text()
  website <- html %>% 
    xml_find_all('//*[contains(concat( " ", @class, " " ), concat( " ", "rpt-company-primaryurl", " " ))]//a') %>% 
    html_text()
  parent <- html %>% 
    xml_find_all('//*[contains(concat( " ", @class, " " ), concat( " ", "rpt-company-parent-company-name", " " ))]//a') %>% 
    html_text()
  corp_link <- html %>% 
    xml_find_all('//*[contains(concat( " ", @class, " " ), concat( " ", "rpt-company-corpfam-total", " " ))]//a') %>% 
    html_text()
  industry <- html %>% 
    xml_find_all('//*[contains(concat( " ", @class, " " ), concat( " ", "rpt-company-industry-desc", " " ))]') %>% 
    html_text()
  sales <- html %>% 
    xml_find_all('//*[contains(concat( " ", @class, " " ), concat( " ", "rpt-company-annual-sales", " " ))]') %>% 
    html_text()
  currency <- html %>% 
    xml_find_all('//*[contains(concat( " ", @class, " " ), concat( " ", "rpt-company-reporting-currency", " " ))]//span') %>% 
    html_text()
  assets <- html %>% 
    xml_find_all('//*[contains(concat( " ", @class, " " ), concat( " ", "rpt-company-total-assets", " " ))]') %>% 
    html_text()
  duns <- html %>% 
    xml_find_all('') %>% 
    html_text()
  address <- html %>% 
    xml_find_all('//*[contains(concat( " ", @class, " " ), concat( " ", "rpt-company-industry", " " ))]//*[contains(concat( " ", @class, " " ), concat( " ", "paragraph-med", " " ))]') %>% 
    html_text()
#  lat <- html %>% 
#    xml_find_all('//*[contains(concat( " ", @class, " " ), concat( " ", "paragraph-short", " " )) and #(((count(preceding-sibling::*) + 1) = 1) and parent::*)]') %>% 
#    html_text()
#  lon <- html %>% 
#    xml_find_all('//*+[contains(concat( " ", @class, " " ), concat( " ", "paragraph-short", " " ))]#//*[contains(concat( " ", @class, " " ), concat( " ", "paragraph-short", " " ))]') %>% 
#    html_text()
#  details <- html %>% 
#    xml_find_all('//p') %>% 
#    html_text()
  naics2017 <- html %>% 
    xml_find_all('//*[contains(concat( " ", @class, " " ), concat( " ", "ant-col-12", " " ))]//div[(((count(preceding-sibling::*) + 1) = 1) and parent::*)]//a') %>% 
    html_text()
#  ussic1987 <- html %>% 
#    xml_find_all('//*+[contains(concat( " ", @class, " " ), concat( " ", "ant-col-12", " " ))]//*[contains#(concat( " ", @class, " " ), concat( " ", "ant-col-12", " " ))]//div[(((count(preceding-sibling::*) + 1) = #4) and parent::*)]//a') %>% 
#    html_text()
  yr_founded <- html %>% 
    xml_find_all('//*[contains(concat( " ", @class, " " ), concat( " ", "rpt-company-year-founded", " " ))]') %>% 
    html_text()
  incorp_location <- html %>% 
    xml_find_all('//*[contains(concat( " ", @class, " " ), concat( " ", "rpt-company-soi", " " ))]') %>% 
    html_text()
  link <- url
  df <- data.table(list(name), list(type), list(employees), list(website),
                               list(parent), list(corp_link), list(industry), list(sales),
                               list(currency), list(assets), list(duns), list(address),
                               list(naics2017),  list(yr_founded), list(incorp_location),
                               list(link))
  hoovers_db2 <- rbind(hoovers_db2, df)
}

#fwrite(hoovers_db, "~/Desktop/Hoovers_db.csv")
#fwrite(hoovers_db2, "~/Desktop/Hoovers_db2.csv")


hoovers_db_combined <- rbind(hoovers_db, hoovers_db2)
hoovers1 <- select(hoovers1, name_link) %>% rename(official_name = name_link)
hoovers2 <- select(hoovers2, name_link) %>% rename(official_name = name_link)
hoovers_combined <- rbind(hoovers1, hoovers2)
hoovers_db_combined <- cbind(hoovers_db_combined, hoovers_combined)

colnames(hoovers_db_combined) <- c("name", "type", "employees", "website",
                                   "parent", "corp_link", "industry", "sales",
                                   "currency", "assets", "duns", "address",
                                   "naics2017", "yr_founded", "incorp_location",
                                   "link", "official_name")

#colnames(uled)
uled <- unnest(hoovers_db_combined, cols = "name", keep_empty = T) 
uled <- unnest(uled, cols = "type", keep_empty = T) 
uled <- unnest(uled, cols = "website", keep_empty = T) 
uled <- unnest(uled, cols = "parent", keep_empty = T) 
uled <- unnest(uled, cols = "corp_link", keep_empty = T) 
uled <- unnest(uled, cols = "industry", keep_empty = T) 
uled <- unnest(uled, cols = "currency", keep_empty = T) 
uled <- unnest(uled, cols = "assets", keep_empty = T) 
uled <- unnest(uled, cols = "duns", keep_empty = T) 
uled <- unnest(uled, cols = "address", keep_empty = T) 
uled <- unnest(uled, cols = "naics2017", keep_empty = T) 
uled <- unnest(uled, cols = "yr_founded", keep_empty = T) 
uled <- unnest(uled, cols = "incorp_location", keep_empty = T) 
uled <- unnest(uled, cols = "link", keep_empty = T) 
uled <- unnest(uled, cols = "employees", keep_empty = T)
uled <- unnest(uled, cols = "sales", keep_empty = T) 

uled <- unique(uled)

uled$employee_location <- str_extract(uled$employees, 
                                      "\\(here\\)|\\(Total\\)|\\(Global Ultimate Total\\)") %>% 
  str_remove_all(., "\\(|\\)")
uled$employees <- str_remove_all(uled$employees, 
                                 "\\(here\\)|\\(Total\\)|\\(Global Ultimate Total\\)")
uled$sales_location <- ifelse(str_detect(uled$sales, "\\(Global Ultimate Total\\)"),
                                   "Global Ultimate Total", "") 
uled$sales <- str_remove_all(uled$sales, "\\(Global Ultimate Total\\)")

uled <- uled %>% 
  mutate(employee_location = case_when(
    employee_location == "here" ~ "local_employee_number",
    employee_location == "Total" ~ "total_employee_number",
    employee_location == "Global Ultimate Total" ~ "global_employee_number",
    T ~ "no_employees"
  )) %>% 
  mutate(sales_location = case_when(
    sales_location == "" ~ "local_sales_number",
    sales_location == "Total" ~ "total_sales_number",
    sales_location == "Global Ultimate Total" ~ "global_sales_number",
    T ~ "no_sales"
  ))
uled <- unique(uled)

uledwide1 <- pivot_wider(uled, names_from = "employee_location", values_from = "employees") 
uledwide2 <- pivot_wider(uledwide1, names_from = "sales_location", values_from = "sales") 

uled <- uledwide2 %>% select(-no_sales, - no_employees) %>% unique()
fwrite(uled, "data_clean/company_db_h.csv")
