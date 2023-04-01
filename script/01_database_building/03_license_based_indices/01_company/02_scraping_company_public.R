library(rvest)
library(httr)
library(xml2)
require(RSelenium)
require(wdman)
library(dplyr)
library(stringr)
# Set working director to Box
setwd("~/Box/lgu")

existing <- read.csv("data_clean/company_db.csv")
existing$licensee_type <- "company"

# Read in company list
company <- read.csv("data_indices/company.csv")
company <- company %>% 
  filter(!(official_name %in% existing$official_name))
company$id <- paste0("c", 1:nrow(company))
company_nolink <- filter(company, is.na(dnb_link))
company_multilink <- filter(company, !is.na(dnb_link2))
company <- filter(company, !is.na(dnb_link), is.na(dnb_link2))

## Run this twice to get rid of cookies
#http://joshuamccrain.com/tutorials/web_scraping_R_selenium.html
rD <- rsDriver(browser = "firefox", port = 4567L)
# This should open a firefox browser
remDr <- rD[["client"]]
remDr$deleteAllCookies()
remDr$close()
rD$server$stop()
rm(rD, remDr)
gc()

rD <- rsDriver(browser = "firefox", port = 4567L)
# This should open a firefox browser
remDr <- rD[["client"]]
remDr$getAllCookies()

company_db <- data.frame()
for(i in 1:nrow(company)){
  id <- company$id[i]
  official_name <- company$official_name[i]
  url <- company$dnb_link[i]
  notes <- company$notes[i]
  remDr$navigate(url)
  Sys.sleep(65)
  #if(i %% 9 == 0) {Sys.sleep(57)}
  #else if(i %% 16 == 0) {Sys.sleep(34)}
  #else {Sys.sleep(12)}
  html <- remDr$getPageSource()[[1]]
  html <- read_html(html)
  name <- html %>% 
    xml_find_all('//*[(@class = "company-profile-header-title")]') %>% 
    html_text()
  name <- ifelse(length(name) == 0, NA, name)
  datapoints <- html %>% 
    xml_find_all('//span[(@class = "company_data_point")]') 
  dba <- datapoints %>% 
    xml_find_all('../*[(@name = "company_name")]') %>% # This keeps me right in the datapoints 
    xml_find_all('.//span') %>% 
    html_text()
  dba <- ifelse(length(dba) == 0, NA, dba)
  description <- datapoints %>% 
    xml_find_all('../*[(@name = "company_description")]') %>% # This keeps me right in the datapoints 
    xml_find_all('.//span') %>% 
    html_text()
  description <- ifelse(length(description) == 0, NA, description)
  industry <- datapoints %>% 
    xml_find_all('../*[(@name = "industry_links")]') %>% # This keeps me right in the datapoints 
    xml_find_all('.//span') %>% 
    html_text() %>% 
    str_remove_all("\\\n") %>% 
    str_remove_all("\\s{2,}")
  industry <- ifelse(length(industry) == 0, NA, industry)
  industry <- industry[1]
  details <- html %>% 
    xml_find_all('//*[(@id = "company_profile_snapshot")]') %>% 
    xml_find_all('.//*[(@class = "col-md-11")]')
  address <- details %>% 
    xml_find_all('.//*[(@name = "company_address")]') %>% 
    xml_find_all('.//span[not(contains(@class, "arrow"))]') %>% 
    html_text() %>% 
    str_remove_all("\\\n") %>% 
    str_remove_all("\\s{2,}")
  address <- ifelse(length(address) == 0, NA, address)
  website <- details %>% 
    xml_find_all('.//*[(@name = "company_website")]') %>% 
    html_nodes("a") %>% 
    html_attr("href") 
  website <- ifelse(length(website) == 0, NA, website)
  employees <- details %>% 
    xml_find_all('.//*[(@name = "employees_all_site")]') %>% 
    xml_find_all('.//span[not(contains(@class, "reliability-tag"))]') %>% 
    html_text() %>% 
    trimws()
  employees <- ifelse(length(employees) == 0, NA, employees)
  revenue <- details %>% 
    xml_find_all('.//*[(@name = "revenue_in_us_dollar")]') %>% 
    xml_find_all('.//span[not(contains(@class, "reliability-tag"))]') %>% 
    html_text() %>% 
    str_remove_all("\\\n") %>% 
    str_remove_all("\\s{2,}")
  revenue <- ifelse(length(revenue) == 0, NA, revenue)
  year_started <- details %>% 
    xml_find_all('.//*[(@name = "year_started")]') %>% 
    xml_find_all('.//span[not(contains(@class, "reliability-tag"))]') %>% 
    html_text() %>% 
    trimws()
  year_started <- ifelse(length(year_started) == 0, NA, year_started)
  year_inc <- details %>% 
    xml_find_all('.//*[(@name = "year_incorporated")]') %>% 
    xml_find_all('.//span[not(contains(@class, "reliability-tag"))]') %>% 
    html_text() %>% 
    trimws()
  year_inc <- ifelse(length(year_inc) == 0, NA, year_inc)
  esg_rank <- details %>% 
    xml_find_all('.//*[(@name = "esgRank")]') %>% 
    xml_find_all('.//span[not(contains(@class, "icon"))]') %>% 
    html_text() %>% 
    trimws()
  esg_rank <- ifelse(length(esg_rank) == 0, NA, esg_rank)
  df <- data.frame(id, official_name, name, dba, description, industry, address, website, 
             employees, revenue, year_started, year_inc, esg_rank, notes)
  company_db <- rbind(company_db, df)
}

#write.csv(company_db, "data_clean/company_db_3.csv", row.names = F)
#write.csv(company_db, "data_clean/company_db_2.csv", row.names = F)
#company_db <- read.csv("lgu/company_db.csv")
not_gathered <- filter(company_db, is.na(name)) %>% select(id)

# Make other for those with no link

company_nolink$name <- NA
company_nolink$dba <- NA
company_nolink$description <- NA
company_nolink$industry <- NA
company_nolink$address <- company_nolink$state # condition on location
company_nolink$website <- company_nolink$other_link
company_nolink$employees <- NA
company_nolink$revenue <- NA
company_nolink$year_started <- NA
company_nolink$year_inc <- NA
company_nolink$esg_rank <- NA
colnames(company_nolink)
company_nolink <- dplyr::select(company_nolink, id, official_name, name, dba, 
                         description, industry, address, website, 
                         employees, revenue, year_started, year_inc, esg_rank, 
                         notes)

company_db <- rbind(company_db, company_nolink)

write.csv(company_db, "data_clean/company_db_2.csv", row.names = F)

# Multi-link
multi_long <- company_multilink %>% 
  select(id, official_name, subsidiary_of, dnb_link, notes, dnb_link2:dnb_link66) %>% 
  tidyr::pivot_longer(cols = c(dnb_link, dnb_link2:dnb_link66), 
               values_to = "dnb_link", names_to = "link_number") %>% 
  filter(!is.na(dnb_link)) %>% 
  select(-link_number)

# For now doing only the main
multi_long <- company_multilink %>% 
  select(id, official_name, subsidiary_of, dnb_link, notes)

company_db_multi <- data.frame()
for(i in 1:nrow(multi_long)){
  id <- multi_long$id[i]
  official_name <- multi_long$official_name[i]
  url <- multi_long$dnb_link[i]
  notes <- multi_long$notes[i]
  remDr$navigate(url)
  if(i %% 3 == 0) {Sys.sleep(75)}
  else if(i %% 8 == 0) {Sys.sleep(62)}
  else {Sys.sleep(81)}
  html <- remDr$getPageSource()[[1]]
  html <- read_html(html)
  name <- html %>% 
    xml_find_all('//*[(@class = "company-profile-header-title")]') %>% 
    html_text()
  name <- ifelse(length(name) == 0, NA, name)
  datapoints <- html %>% 
    xml_find_all('//span[(@class = "company_data_point")]') 
  dba <- datapoints %>% 
    xml_find_all('../*[(@name = "company_name")]') %>% # This keeps me right in the datapoints 
    xml_find_all('.//span') %>% 
    html_text()
  dba <- ifelse(length(dba) == 0, NA, dba)
  description <- datapoints %>% 
    xml_find_all('../*[(@name = "company_description")]') %>% # This keeps me right in the datapoints 
    xml_find_all('.//span') %>% 
    html_text()
  description <- ifelse(length(description) == 0, NA, description)
  industry <- datapoints %>% 
    xml_find_all('../*[(@name = "industry_links")]') %>% # This keeps me right in the datapoints 
    xml_find_all('.//span') %>% 
    html_text() %>% 
    str_remove_all("\\\n") %>% 
    str_remove_all("\\s{2,}")
  industry <- ifelse(length(industry) == 0, NA, industry)
  industry <- industry[1]
  details <- html %>% 
    xml_find_all('//*[(@id = "company_profile_snapshot")]') %>% 
    xml_find_all('.//*[(@class = "col-md-11")]')
  address <- details %>% 
    xml_find_all('.//*[(@name = "company_address")]') %>% 
    xml_find_all('.//span[not(contains(@class, "arrow"))]') %>% 
    html_text() %>% 
    str_remove_all("\\\n") %>% 
    str_remove_all("\\s{2,}")
  address <- ifelse(length(address) == 0, NA, address)
  website <- details %>% 
    xml_find_all('.//*[(@name = "company_website")]') %>% 
    html_nodes("a") %>% 
    html_attr("href") 
  website <- ifelse(length(website) == 0, NA, website)
  employees <- details %>% 
    xml_find_all('.//*[(@name = "employees_all_site")]') %>% 
    xml_find_all('.//span[not(contains(@class, "reliability-tag"))]') %>% 
    html_text() %>% 
    trimws()
  employees <- ifelse(length(employees) == 0, NA, employees)
  revenue <- details %>% 
    xml_find_all('.//*[(@name = "revenue_in_us_dollar")]') %>% 
    xml_find_all('.//span[not(contains(@class, "reliability-tag"))]') %>% 
    html_text() %>% 
    str_remove_all("\\\n") %>% 
    str_remove_all("\\s{2,}")
  revenue <- ifelse(length(revenue) == 0, NA, revenue)
  year_started <- details %>% 
    xml_find_all('.//*[(@name = "year_started")]') %>% 
    xml_find_all('.//span[not(contains(@class, "reliability-tag"))]') %>% 
    html_text() %>% 
    trimws()
  year_started <- ifelse(length(year_started) == 0, NA, year_started)
  year_inc <- details %>% 
    xml_find_all('.//*[(@name = "year_incorporated")]') %>% 
    xml_find_all('.//span[not(contains(@class, "reliability-tag"))]') %>% 
    html_text() %>% 
    trimws()
  year_inc <- ifelse(length(year_inc) == 0, NA, year_inc)
  esg_rank <- details %>% 
    xml_find_all('.//*[(@name = "esgRank")]') %>% 
    xml_find_all('.//span[not(contains(@class, "icon"))]') %>% 
    html_text() %>% 
    trimws()
  esg_rank <- ifelse(length(esg_rank) == 0, NA, esg_rank)
  df <- data.frame(id, official_name, name, dba, description, 
                   industry, address, website, 
                   employees, revenue, year_started, year_inc, esg_rank, notes)
  company_db_multi <- rbind(company_db_multi, df)
}

# I cannot collapse and merge this with the other database until I convert some of the columns to other datatypes, so for now I will save it as its own thing
write.csv(company_db_multi, "data_clean/company_db_multi_2.csv", row.names = F)


