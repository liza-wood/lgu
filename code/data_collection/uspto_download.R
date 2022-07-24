library(tidyverse)
library(lubridate)
library(stringr)
# https://cran.r-project.org/web/packages/patentr/vignettes/intro.html
# https://github.com/JYProjs/patentr

getOption('timeout')
options(timeout=600)

plant_words <- paste(c("\\b[Pp]lant", "[Gg]ermplasm", "[Vv]ariet*", 
                       "[Cc]ultivar", "[Ss]cion", "[Rr]ootstock"), collapse = "|")

uni_plant_patents <- data.frame()
for(i in 1974:2020){
  for(j in 1:52){
    get_bulk_patent_data(
      year = i,         
      week = j,                   
      output_file = "data/temp_patent_output.csv" # output file in which patent data is stored
    )
    patent_data <- read.csv("data/temp_patent_output.csv", 
                            col.names = c("WKU", "Title", "App_Date",
                                          "Issue_Date", "Inventor",
                                          "Assignee", "ICL_Class",
                                          "References", "Claims")) %>%
      mutate(App_Date = as_date(App_Date),
             Issue_Date=as_date(Issue_Date))
    
    df <- patent_data %>% 
      filter(str_detect(Assignee, "[Uu]niversity|[Vv]irginia Tech")) %>% 
      filter(str_detect(Title, plant_words))
    
    uni_plant_patents <- rbind(uni_plant_patents, df)
  }
}

write.csv(uni_plant_patents, "~/Desktop/uspto1990_2010.csv", row.names = F)
