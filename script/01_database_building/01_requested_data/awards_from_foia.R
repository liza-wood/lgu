# Awards
library(stringr)
library(dplyr)
# Set working director to Box
setwd("~/Box/lgu")

# CALIFORNIA ----
ca <- read.csv("data_raw/university_awards/California_awards.csv",
                   skip = 2)
splt <- str_split(ca$PI.Name, ',')
names <- data.frame(do.call('rbind', splt))
colnames(names) <- c('inventor_last', 'inventor_first')
ca$Project.Start <- mdy(ca$Project.Start)
ca$Project.End <- mdy(ca$Project.End)
ca <- cbind(ca, names)

# LOUISIANA ----
la <- readxl::read_xlsx ("data_raw/university_awards/Louisiana_awards.xlsx")
splt <- str_split(la$Investigator, ',')
names <- data.frame(do.call('rbind', splt))
colnames(names) <- c('inventor_last', 'inventor_first')
la <- cbind(la, names)

# MINNESOTA ----
mn <- readxl::read_xlsx("data_raw/university_awards/Minnesota_awards.xlsx")
colnames(mn) <- mn[1,]
mn <- mn[-1,]
splt <- str_split(mn$`Award PI`, ',')
names <- data.frame(do.call('rbind', splt))
colnames(names) <- c('inventor_last', 'inventor_first')
# Had to look up this ridiculous excel conversion:
# https://gizmokid2005.com/2013/05/convert-excel-5-digit-serial-date-numbers-to-date
# It has to do with Julian dates but havent gotten a function to work
mdy("01-01-1900")+(as.numeric(mn$`Award Start Date`[1])-2)
mn$`Award Start Date` <- mdy("01-01-1900")+as.numeric(mn$`Award Start Date`)-2
mn$`Award Start Date`[2]
mn$`Award End Date` <- mdy("01-01-1900")+as.numeric(mn$`Award End Date`)-2
mn <- cbind(mn, names)

# IDAHO ----
# these are on multiple sheets so I need to go through each
yrs <- 2002:2020
id <- data.frame()
for(i in 1:19){
  sheet <- readxl::read_xlsx ("data_raw/university_awards/Idaho_awards_editable.xlsx", sheet = i)
  sheet$year = yrs[i]
  colnames(sheet) <- c("Granting Agency", "Amount", "Start Date", "End Date",
  "Project Title", "PI", "Department", "Cost Share", "FY")
  id <- rbind(sheet, id)
}
# Need to split name
splt <- ifelse(str_detect(id$PI, ','), str_split(id$PI, ','),
               str_split(id$PI, ' ', n = 2))
# Some are only length 1
rm1 <- which(lengths(splt) == 1)
rm3 <- which(lengths(splt) == 3)
splt[[rm1[3]]]
# Remove those with 1 names because they are not names
splt <- splt[-rm1]
# Then for the length 3 one...
splt[[rm3]]
splt[[rm3]] <- splt[[rm3]][-3]
names <- data.frame(do.call('rbind', splt))
id <- id[-rm1,]
colnames(names) <- c('inventor_last', 'inventor_first')
id <- cbind(id, names)

head(ca)
colnames(ca) <- c("AwardID", "ProjectNo", "PIFullName", "Dept", "College", "Sponsor",
                  "SponsorType", "PrimeSponsor", "PrimeSponsorType", 
                  "ProjectTitle", "StartDate", "EndDate", "AwardAmt", "ProcessedDate",
                  'inventor_last', 'inventor_first')
head(la$`Investigator Dept`)
colnames(la) <- c("AwardID", "ProjectNo", "BudgetDate", "FY", "Sponsor", "AwardAmt",
                  "StartDate", "EndDate", "ProjectTitle", "PIFullName", "PIType",
                  "Dept", "CostShareAmt", 'inventor_last', 'inventor_first')
colnames(mn)
head(mn$`Award Status`)
colnames(mn) <- c("AwardType", "AwardPurpose", "AwardID", "ProjectTitle",
                  "PIFullName", "DeptID", "Dept", "Sponsor", "SponsorID", 
                  "PrimeSponsor", "PrimeSponsorID", "GenerateDate", 
                  "StartDate", "EndDate", "AwardAmt", "AwardStatus", "ContractStatus",
                  'inventor_last', 'inventor_first')
colnames(id)
head(id$`Cost Share`)
colnames(id) <- c("Sponsor", "AwardAmt", "StartDate", "EndDate", "ProjectTitle",
                  "PIFullName",  "Dept", "CostShareAmt","FY", 
                  'inventor_last', 'inventor_first')

cols_int <- c("Sponsor", "AwardAmt", "StartDate", "EndDate", "ProjectTitle",
              "PIFullName",  "Dept", 'inventor_last', 'inventor_first')

ca <- select(ca, all_of(cols_int)) %>% mutate(uni_state = "California")
la <- select(la, all_of(cols_int)) %>% mutate(uni_state = "Louisiana")
mn <- select(mn, all_of(cols_int)) %>% mutate(uni_state = "Minnesota")
id <- select(id, all_of(cols_int)) %>% mutate(uni_state = "Idaho")

head(ca$StartDate)
head(la$StartDate)
head(mn$StartDate)
head(id$StartDate)

awards <- rbind(ca, la) %>% rbind(mn) %>% rbind(id)
awards$inventor_first <- trimws(awards$inventor_first)
awards$inventor_first <- str_remove_all(awards$inventor_first, "\\.")
awards$inventor_first_1 <- str_extract(awards$inventor_first, "^\\w{1}")
awards$inventor_last <- trimws(awards$inventor_last)
awards$inventor_last <- str_remove_all(awards$inventor_last, "\\,")
awards <- awards %>% 
  mutate(start_year = year(ymd(StartDate)),
         end_year = year(ymd(EndDate)),
         amt = as.numeric(str_remove_all(AwardAmt, '\\$|\\s+|\\,|\\(|\\)')))

write.csv(awards,'data_clean/awards_selected_states.csv', row.names = F)

write.csv(ca,'data_clean/awards_ca.csv', row.names = F)
write.csv(la,'data_clean/awards_la.csv', row.names = F)
write.csv(mn,'data_clean/awards_mn.csv', row.names = F)
write.csv(id,'data_clean/awards_id.csv', row.names = F)

# OLD APPROACH, CA -----
breeding <- awards %>% 
  filter(Admin.Dept == "Plant Sciences") %>% 
  filter(str_detect(Project.Title, "breed|seed|germplasm|variety"))

breeding %>% 
  mutate(Total = as.numeric(str_remove_all(Total, "\\(|\\)|\\$|\\,"))) %>% 
  group_by(Sponsor.Type) %>% 
  summarize(Total = sum(Total, na.rm = T)) %>% 
  arrange(Total)

companies <- awards %>% 
  filter(Admin.Dept == "Plant Sciences") %>% 
  filter(Sponsor.Type == "BUSINESS") %>% 
  select(Sponsor) %>% 
  unique()

# Crop master list from licenses
crops <- read.csv("crop.csv")
morecrops <- "walnut|rosaceae|armillaria|strawberry|pistachio|grape|almond|legume|forage|cassave|pine|garbanzos|juglans|pima"
# juglans is walnut
# pima is bunchgrass
crop <- paste(crops$crop_name_common, collapse = "|")
family <- paste(crops$crop_name_scientific..genus.species., collapse = "|")
crops <- tolower(paste(crop, family, morecrops, sep = "|"))

breeding <- breeding %>% 
  mutate(crop = str_extract(tolower(Project.Title), crops))
