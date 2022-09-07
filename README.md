# Land Grant University Licensing Records  
## Trends in plant variety development and university technology transfer  

Workflow:  
`read_data` --> `federal_money` -->  `data_merging` --> `data_explore` --> `models`

* `read_data`: Reads in data from Google Drive and creates a single data frame with the license data, and a second data frame with each of the D&B companies  
* `federal_modey` brings in data downloaded from the (USDA NIFA portal)[https://portal.nifa.usda.gov/lmd4/recent_awards]  
* `data_explore`: Beginning look at univariate and multivariate descriptions of the data  

All data stored privately on Box. Data sources include:
* Federal money is downloaded from USDA NIFA's recent awards database: https://portal.nifa.usda.gov/lmd4/recent_awards  
* PVP database is downloaded from USDA PVPO as an Excel spreadsheet: https://www.ams.usda.gov/services/plant-variety-protection/application-status  
* OSSI database was scraped from the OSSI webpage, where the Wayback Machine was used to look at archive and make sure nothing was missing: https://osseeds.org/  
* Licensing data, read in from Google Drive, was requested from all 50 LGUs between 2021-2022  
* Awards data was requested from all 50 LGUs in 2022  
