# Scraping dnb sites
library(rvest)
library(httr)
library(polite)
library(xml2)

company <- read.csv("data/company.csv")
company <- filter(company, !is.na(dnb_link))
company$ext <- str_extract(company$dnb_link, "(?<=https\\:\\/\\/www\\.dnb\\.com\\/).*")

# ---- First try just as I would normally ----
url <- company$dnb_link[5]
html <- read_html(url) # 403 error

# ---- Trying with polite ----
#https://community.rstudio.com/t/scrapping-400-pages-using-rvest-and-purr/31033/2 

url_base <- "https://www.dnb.com/"
session <- bow(url_base)
# user_agent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/100.0.4896.88 Safari/537.36"
session
#paste0(url_base, company$ext[1])
full_url <- nod(session, company$ext[1])
html <- polite::scrape(full_url)
div <- xml_find_all(html, "//div")


# ---- Trying to reset user agent ----
# https://stackoverflow.com/questions/66975138/change-user-agent-when-using-rvestread-html
url <- company$dnb_link[5]
my_session <- session(url)
my_session$response$request$options$useragent
html <- read_html(my_session)

user_agent_new <- user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/100.0.4896.88 Safari/537.36")
my_session <- session(url, user_agent_new)
my_session$response$request$options$useragent
html <- read_html(my_session)

# I can read it but it gives me no script
div <- xml_find_all(html, "//div")
html %>% 
  xml_find_all(div, '//*[(@id = "company_profile_snapshot")]')

