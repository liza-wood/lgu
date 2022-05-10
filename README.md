# Land Grant University Licensing Records  
## Trends in plant variety development and university technology transfer  

Workflow:  
`read_data` --> `scraping_rselenium` --> `federal_money` --> `data_explore`

* `read_data`: Reads in data from Google Drive and creates a single data frame with the license data, and a second data frame with each of the D&B companies  
* `scraping_selenium`: Powers up a server to read html from each D&B site. This is necessary because there is a 403 error with more basic scraping methods (e.g. rvest). This script scrapes the html and extracts the necessary html details, appended to the licensing data frame from `read_data`   
* `federal_modey` brings in data downloaded from the (USDA NIFA portal)[https://portal.nifa.usda.gov/lmd4/recent_awards]  
* `data_explore`: Beginning look at univariate and multivariate descriptions of the data  

All data stored privately on Box.
