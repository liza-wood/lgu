## Land Grant University Licensing Records  
### Trends in plant variety development and university technology transfer  

**File organization**

There are three folders in the `code/` folder:

* `data_collection/` includes a number of scripts that help with data downloading/collection and cleaning. Each file begins with a verb: downloading, scraping, or cleaning. 
  * `company/` data related to the companies that LGUs license with
  * `invention/` data related to the specific inventions listed on LGU licensing agreements
  * `ip/` data related to plant variety protections, patents, from the GRIN repository, and others
  * `license/` data related to the licensing agreements between LGUs and the licensees of plant material
  * `money/` data related to funding, include funding available through USDA websites and awards data requested from universities
* `analysis/` includes a sequence of scripts that work through merging all of the files from data collection, cleaning the merged data, and then analyzing it.


All data stored privately on Box. Data sources include:  

* Money: Federal money is downloaded from [USDA NIFA's recent awards database](https://portal.nifa.usda.gov/lmd4/recent_awards) and LGU awards were requested from the 25 LGUs that provided licensing data in 2022  
* IP: PVP database is downloaded from [USDA PVPO as an Excel spreadsheet](https://www.ams.usda.gov/services/plant-variety-protection/application-status); US Patent data was downloaded and filtered using the [patentR](https://cran.r-project.org/web/packages/patentr/patentr.pdf) and [patentsview](https://cran.r-project.org/web/packages/patentsview/patentsview.pdf) packages that wrap around the [Patents View API](https://patentsview.org/apis/api-endpoints); OSSI database was scraped from the [OSSI webpage](https://osseeds.org/), where the Wayback Machine was used to look at archive and make sure nothing was missing  
* License: Licensing data, read in from Google Drive, was requested from all 50 LGUs between 2021-2022  

**Contact**  

Liza Wood  
belwood[at]ucdavis.edu
