fls <- list.files('~/Box/lgu/data_raw/university_awards/Oregon/', full.names = T)
df <- sapply(fls, function(x) xlsx::read.xlsx(x, sheetIndex = 1))
df <- unique(data.table::rbindlist(df,fill=T))

yrs <- lubridate::year(df$Project.Start)
table(yrs)

pis <- df$Primary.PI
unique_pis <- unique(pis)
table(pis)

grants <- unique(df$Grant.Title)
