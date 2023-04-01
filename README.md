## Land Grant University Licensing Records

### Trends in plant variety development and university technology transfer

**File organization**

There are three folders in the `script/` folder:

1.  `database_building/` includes scripts that build or impute indices based on initial input data: 1) LGU plant variety licenses 1980-2020 and 2) LGU Agricultural College research awards 2000-2020.

-   `requested_data/` pulls down the input data that has been cleaned/organized by student researchers (green)
-   `external_indices/` downloads/scrapes data from publicly available databases (`federal_funding` and `ip`) to create indices of federal funding for awards and for university-relevant PVP, PP, [ONE DAY ALL GRIN?] (yellow)
-   `license_based_indices/` isolates data from the requested data to build two indices, one with `company` data, with attributes from the D&B database, and one with `innovations` data, where licensed innovations are matched to the IP external indices. Also inside `innovations` is the reading in and developing of a money to classify attributes of the innovations. (blue and grey)

2.  `merging_cleaning/` brings together licensing and company data, innovation and award data, and innovation and inventor data (orange and light green)

3.  `analysis/` summarizes the merges so far

![](img/lgu_workflow.pdf)

All data stored privately on Box. Data sources include:

-   Money: Federal money is downloaded from [USDA NIFA's recent awards database](https://portal.nifa.usda.gov/lmd4/recent_awards) and LGU awards were requested from the 25 LGUs that provided licensing data in 2022\
-   IP: PVP database is downloaded from [USDA PVPO as an Excel spreadsheet](https://www.ams.usda.gov/services/plant-variety-protection/application-status); US Patent data was downloaded and filtered using the [patentR](https://cran.r-project.org/web/packages/patentr/patentr.pdf) and [patentsview](https://cran.r-project.org/web/packages/patentsview/patentsview.pdf) packages that wrap around the [Patents View API](https://patentsview.org/apis/api-endpoints); OSSI database was scraped from the [OSSI webpage](https://osseeds.org/), where the Wayback Machine was used to look at archive and make sure nothing was missing\
-   License: Licensing data, read in from Google Drive, was requested from all 50 LGUs between 2021-2022\
-   Awards: Awards data, read in from Google Drive, was requested from all 50 LGUs between 2021-2022

**Contact**

Liza Wood\
belwood[at]ucdavis.edu
