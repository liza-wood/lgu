library(rvest)
library(xml2)
library(httr)
library(pdftools)
library(tesseract)
library(patentsview)
library(tidyverse)
setwd("~/Box/lgu")
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

table(is.na(pp_db$id_edited))
pp_db$id_correct <- ifelse(is.na(pp_db$id_edited), pp_db$id, pp_db$id_edited)
# We gained one, what happened
which(duplicated(pp_db$id))
pp_db <- pp_db[!is.na(pp_db$id),]
write.csv(pp_db, "data_clean/pp_with_abstract.csv", 
          row.names = F, fileEncoding = "UTF-8")

set.seed(300)
pp_random_selection <- sample.int(nrow(pp_db), round(.1*nrow(pp_db)), replace = F)
pp_random_selection <- pp_db[pp_random_selection,]
colnames(pp_random_selection)
pp_random_selection <- pp_random_selection %>% 
  select(id, id_edited, uni_state, issue_year, Title, crop_cat, abstract)
write.csv(pp_random_selection, "data_clean/pp_random_selection.csv", 
          row.names = F, fileEncoding = "UTF-8")

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

set.seed(300)
pvp_random_selection <- sample.int(nrow(pvp_db), round(.1*nrow(pvp_db)), replace = F)
pvp_random_selection <- pvp_db[pvp_random_selection,]

pvp_random_selection$id <- ifelse(nchar(pvp_random_selection$id) == 7, 
                 paste0(0,0, pvp_random_selection$id),
                 ifelse(nchar(pvp_random_selection$id) == 8, 
                        paste0(0, pvp_random_selection$id),
                        pvp_random_selection$id))

              
pvp_random_selection$invention_url <- paste0("https://apps.ams.usda.gov/CMS//AdobeImages/",pvp_random_selection$id , ".pdf")


#dir.create('ocr_pdfs')
setwd('ocr_pdfs')

pvp_random_selection$text <- ""
for(i in 13:nrow(pvp_random_selection)){
  pdf <- tryCatch(tesseract::ocr(pvp_random_selection$invention_url[i]),
                  error = function(e) NULL)
  if(!is.null(pdf)){
    exb <- which(str_detect(tolower(pdf), "exhibit\\.?\\s?\\d?\\d?\\s?b"))
    if(length(exb) == 0){
      exb <- which(str_detect(tolower(pdf), 
                              "statement\\s?of\\s?distinctness|statement\\s?of\\s?distinetness"))
    }
    if(length(exb) > 1 & exb[1] %in% c(2,3)){ # it often appears on the title page
      exb <- exb[2]
    } else if(length(exb) > 1){
      exb <- exb[1]
    }
    
    start <- exb
    exc <- which(str_detect(tolower(pdf), "exhibit\\.?\\s?\\d?\\d?\\s?c"))
    desc <- which(str_detect(tolower(pdf), "objective description of variety"))
    end <- sort(unique(c(exc, desc)))
    if(length(end) > 1){
      endspot <- which(end > exb)[1]
    } else {endspot <- 1}
    end <- end[endspot]-1
    end <- ifelse(end == length(pdf) | end < start | is.na(end), exb+2, end)

    text <- paste(pdf[start:end], collapse = " ") 
    pvp_random_selection$text[i] <- str_remove_all(text, '\\\n')
    
  } else {next}
}

write.csv(pvp_random_selection, "data_clean/pvp_random_selection.csv", 
          row.names = F, fileEncoding = "UTF-8")

# OLD -- took these manually before I could get the text extraction:
# Manual taking descriptions for now
#pdf1 <- tesseract::ocr(pvp_random_selection$invention_url[11])
#pdf1b <- pdf1[6]
#cat(pdf1b)
#pdf2 <- tesseract::ocr(pvp_random_selection$invention_url[12])
#pdf2b <- pdf2[6]
#cat(pdf2b)
#pdf3 <- tesseract::ocr(pvp_random_selection$invention_url[13])
#pdf3b <- pdf3[6]
#cat(pdf3b)
#pdf4 <- tesseract::ocr(pvp_random_selection$invention_url[14])
#pdf4b <- pdf4[5]
#cat(pdf4b)
#pdf5 <- tesseract::ocr(pvp_random_selection$invention_url[15])
#pdf5b <- pdf5[8]
#cat(pdf5b)
#
#pdf6 <- tesseract::ocr(pvp_random_selection$invention_url[16])
#pdf6b <- pdf6[6]
#cat(pdf6b)
#pdf7 <- tesseract::ocr(pvp_random_selection$invention_url[17])
#pdf7b <- pdf7[11] 
#cat(pdf7b)
#pdf8 <- tesseract::ocr(pvp_random_selection$invention_url[18])
## doesn't exist
#pdf8b <- pdf8[5]
#cat(pdf8b)
#pdf9 <- tesseract::ocr(pvp_random_selection$invention_url[19])
## missing pages
#pdf9b <- pdf9[7]
#cat(pdf9b)
#pdf10 <- tesseract::ocr(pvp_random_selection$invention_url[20])
## doesn't exist
#pdf10b <- pdf10[7]
#cat(pdf10b)
#pdf8 <- tesseract::ocr(pvp_random_selection$invention_url[21])
## doesn't exist
#pdf8b <- pdf8[5]
#cat(pdf8b)
#pdf9 <- tesseract::ocr(pvp_random_selection$invention_url[22])
#pdf9b <- pdf9[4]
#cat(pdf9b)
#pdf10 <- tesseract::ocr(pvp_random_selection$invention_url[23])
## went with additional description
#pdf10b <- pdf10[c(11,12)]
#cat(pdf10b)
#pdf10 <- tesseract::ocr(pvp_random_selection$invention_url[24])
## went with additional description
#pdf10b <- pdf10[4]
#cat(pdf10b)

## -- GRIN ----