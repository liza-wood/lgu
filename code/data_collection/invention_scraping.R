library(rvest)
library(xml2)
library(httr)
library(pdftools)
library(tesseract)
library(patentsview)
df <- read.csv("data_clean/inventor_invention.csv")
table(df$ip_type)

## -- PATENTS ----
# Scraping patent database and lining up license patents to patent list
# patents in the licensing data
pp_license <- filter(df, ip_type == "USPTO") %>% 
  mutate(id = str_remove_all(pto_id, "\\,"))
sum(is.na(pp_license$id)) # should be zero
# patents in the database
pp_db <- read.csv("~/Box/lgu/data_clean/uspto_lgus.csv") %>% 
  mutate(id = str_remove_all(id, "(?<=PP)0{1,2}")) %>% 
  mutate(id = str_remove_all(id, "^0"))

# These are the licenses not in the database
mismatch1a <- anti_join(pp_license, pp_db, by = "id")
# These are in the database but not the licenses
mismatch2 <- anti_join(pp_db, pp_license, by = "id")
# But what if the database is wrong? Has an extra number?
mismatch2$id <- str_remove(mismatch2$id, "\\d$")
# The rest are either in 2020 or they didn't come up in my uspto filters for some reason...
mismatch1b <- anti_join(mismatch1a, mismatch2, by = "id")

pp_license$list_match <- ifelse(pp_license$id %in% pp_db$id, "direct match",
                         ifelse(pp_license$id %in% str_remove(pp_db$id, "\\d$"), "one less", F))
table(pp_license$list_match)
# This correcting the PP ids that match once you remove the last digit -- the patentr forammting of IDs clearly sucks
needs_corrected <- ifelse(str_remove(pp_db$id, "\\d$") %in% pp_license$id, pp_db$id, NA) 
needs_corrected <- unique(needs_corrected[!is.na(needs_corrected)])
pp_db <- pp_db %>% 
  mutate(id = case_when(
    id %in% needs_corrected ~ as.character(str_remove(id, "\\d$")),
    T ~ as.character(id)
  ))

## Now let's get the abstracts from the whole database
# This lists all the fields
#fields <- get_fields(endpoint = "patents")
pp_db$abstract <- ""
for (i in 1:nrow(pp_db)){
  output <- search_pv(
    query = qry_funs$eq(patent_number = pp_db$id[i]),
    fields = c("patent_number", "patent_abstract"))
  if(length(output$data$patents$patent_abstract) == 1){
    pp_db$abstract[i] <- output$data$patents$patent_abstract
  } else {next}
}

# Many still did not match
length(pp_db$abstract[pp_db$abstract == ""])
# many of these are utility patents. I removed the leading zero, but just like with the plant patents, there are some that have trailing numbers
no_abstract <- pp_db$id[pp_db$abstract == ""]
no_abs_df <- data.frame("id" = no_abstract,
                        "id_edited" = str_remove(no_abstract, "\\d$"))

no_abs_df$abstract <- ""
for (i in 1:nrow(no_abs_df)){
  output <- search_pv(
    query = qry_funs$eq(patent_number = no_abs_df$id_edited[i]),
    fields = c("patent_number", "patent_abstract"))
  if(length(output$data$patents$patent_abstract) == 1){
    no_abs_df$abstract[i] <- output$data$patents$patent_abstract
  } else {next}
}

still_no_abstract <- no_abs_df$id[no_abs_df$abstract == ""]

## COMPILE THE ABSTRACTS THAT I CAN GET ----
pp_db$abstract <- ifelse(pp_db$abstract == "", NA, pp_db$abstract)
pp_db <- full_join(pp_db, no_abs_df, by = "id") %>% 
  mutate(abstract = case_when(
    is.na(abstract.x) ~ abstract.y,
    T ~ abstract.x
  )) %>% select(-abstract.x, -abstract.y)

set.seed(300)
pp_random_selection <- sample.int(nrow(pp_db), round(.1*nrow(pp_db)), replace = F)
pp_random_selection <- pp_db[pp_random_selection,]

# This shit doesn't work anymore
#testurl <- pp$invention_url[2]
#html <- read_html(testurl)
#header <- html %>% 
#  xml_find_all('//hr+//font') %>% 
#  html_text()
#abstract <- html %>% 
#  xml_find_all('//center+//p') %>% 
#  html_text()
#html_text(html)

## -- PVPS ----
# Scraping PVP data
pvp_license <- filter(df, ip_type == "PVP") %>% 
  mutate(id = str_remove_all(pvp_id, "\\,"))
sum(is.na(pvp_license$id)) # should be zero
pvp_db <- read.csv("~/Box/lgu/data_clean/pvpo_lgu.csv") %>% 
  mutate(id = as.character(id))

# These are the licenses not in the database -- WHY??
mismatch1a <- anti_join(pvp_license, pvp_db, by = "id")

pvp_random_selection <- sample.int(nrow(pvp_db), round(.1*nrow(pvp_db)), replace = F)
pvp_random_selection <- pvp_db[random_selection,]

testurl <- pvp_random_selection$invention_url[55]
pdf <- tesseract::ocr(testurl)

pdf[5]

## -- GRIN ----