library(ggplot2)
library(lubridate)
library(dplyr)
# Set working director to Box
setwd("~/Box/lgu")

funding <- read.csv("data_raw/federal_funding//breeding_awards_combined.csv")
funding_by_yr_state <- funding %>% 
  group_by(state, year) %>% 
  summarize(funding_amt = sum(Award.Dollars)) %>% 
  rename(funding_yr = year) %>% 
  ungroup()

fund_df <- funding %>% 
  mutate(funding_yr_grpd = case_when(
    year %in% c(2002:2005) ~ "2002-2005",
    year %in% c(2006:2009) ~ "2006-2009",
    year %in% c(2010:2013) ~ "2010-2013",
    year %in% c(2014:2017) ~ "2014-2017",
    year %in% c(2018:2022) ~ "2018-2022")) %>% 
  group_by(state, funding_yr_grpd) %>% 
  mutate(funding_amt_grpd = sum(Award.Dollars)) %>% 
  select(state, funding_yr_grpd, funding_amt_grpd) %>% 
  unique()


df <- read.csv("data_clean/merged_df.csv") %>% 
  mutate(license_yr = year(effective_date)) %>% 
  filter(license_yr > 1999 & license_yr < 2021) %>% 
  mutate(license_yr_grpd = case_when(
    license_yr %in% (2002:2005) ~ "2002-2005",
    license_yr %in% (2006:2009) ~ "2006-2009",
    license_yr %in% (2010:2013) ~ "2010-2013",
    license_yr %in% (2014:2017) ~ "2014-2017",
    license_yr %in% (2018:2022) ~ "2018-2022")) %>% 
  mutate(funding_yr_grpd = case_when(
    (license_yr - 5) %in% (2002:2005) ~ "2002-2005",
    (license_yr - 5) %in% (2006:2009) ~ "2006-2009",
    (license_yr - 5) %in% (2010:2013) ~ "2010-2013",
    (license_yr - 5) %in% (2014:2017) ~ "2014-2017",
    (license_yr - 5) %in% (2018:2022) ~ "2018-2022"))# %>% 
  #left_join(fund_df)

intersect(colnames(df), colnames(fund_df))
df <- merge(df, fund_df, all.x = T)
# Making this unique to remove some of the repeated geographic exclusive licenses (this was a thing at lease in CA)
df <- unique(df)

organic <- str_detect(unique(df$description), "[Oo]rganic")
table(organic)

df$agreement_bi <- ifelse(df$agreement_type == "Exclusive license", 1,
                          ifelse(df$agreement_type == "Non-exclusive license", 0, NA))
df$funding_amt_grpd <- ifelse(!is.na(df$funding_yr_grpd) & is.na(df$funding_amt_grpd), 
                              0 , df$funding_amt_grpd)
df$funding_amt_grpd_log <- log(df$funding_amt_grpd)
df$funding_amt_grpd_log <- ifelse(df$funding_amt_grpd_log == "-Inf", 0, df$funding_amt_grpd_log)
df$rev <- ifelse(df$licensee_type == "other", 0, df$rev)
df$rev_log <- log(df$rev)
df$rev_log <- ifelse(df$rev_log == "-Inf", 0, df$rev_log)
df$pres <- ifelse(df$license_yr == 2000, "Clinton",
           ifelse(df$license_yr %in% 2001:2009, "Bush",
           ifelse(df$license_yr %in% 2010:2016, "Obama",
           ifelse(df$license_yr %in% 2017:2021, "Trump", "Other"))))
df$inregion <- ifelse(df$spatial_match == "Within region", 1,
               ifelse(df$spatial_match == "Outside region", 0, NA))


df$company_size <- factor(df$company_size, levels = c("Large ($100M+)",
                                                      "Medium ($10M - $100M)",
                                                      "Small ($328K - $10M)",
                                                      "Very small (<$350K)",
                                                      "Non-company", 
                                                      "Unknown"))

saveRDS(df, "data_clean/df_final.RDS")
df <- select(df, -c(licensee, crop_name_scientific, variety_name, invention_name, 
                    department, inventor_last_name, inventor_last_name,
                    effective_date, end_date, reference_id, id, dba, description, 
                    industry, address, website, employees, revenue, year_started,
                    year_inc, esg_rank, type, uni_region, company_region))

# ---- Univariate ----

table(df$crop_name_common)
table(df$crop)
table(df$crop_intermed)
table(df$crop_fewer)
summary(df$rev/10000)
unique(df$license_yr)
df$yr_bi <- ifelse(df$license_yr %in% 2001:2010, "2001-2010", "2011-2020")
table(df$agreement_type)
618/(618+709) # Exclusive
table(df$agreement_type, df$yr_bi)
279/(279+234) # exclusive 2001-2010
339/(339+475) # exclusive 2011-2020
table(df$spatial_match)
prop.table(table(df$spatial_match))
381/(381+1434) # outside
prop.table(table(df$spatial_match, df$yr_bi), margin = 2)
158/(158+451) # outside 2001-2010
223/(223+983) # outside 2011-2020

prop.table(table(df$company_size[df$company_size != "Unknown" & df$company_size != "Non-company"]))
prop.table(table(df$company_size, df$spatial_match), margin = 2)

df %>% 
  select(name, company_size) %>% 
  unique() %>% 
  count(company_size) %>% 
  mutate(sum = sum(n)) %>% 
  mutate(percent = 100*(n/sum)) %>% 
  filter(!is.na(company_size), !company_size %in% c("Unknown")) %>% 
  ggplot(aes(x = company_size, y = n)) +
  geom_bar(stat = "identity")

# --- Exploring the data: Looking at trends and patterns ----

## TIME across the x axis ----
# Are licenses generally going up or down?

df %>% 
  group_by(yr_bi) %>% 
  count()

time_count_plot <- df %>% 
  group_by(license_yr) %>% 
  count() %>% 
  ggplot(aes(x = license_yr, y = n,)) +
  geom_bar(stat = "identity") +
  stat_identity(geom = "text", colour = "white", size = 2,
                aes(label = n),
                position=position_stack(vjust=0.5)) +
  labs(x = "Year", y = "Count", title = "Licenses over time") +
  theme_linedraw() +
  theme(plot.title = element_text(hjust = 0.5, size = 10)) ; time_count_plot 

df$crop_main <- ifelse(df$crop_fewer == "Field crops", "Field crops",
                       ifelse(df$crop_fewer == "Perennial (fruit/nut)", 
                              "Perennial (fruit/nut)", "Other"))

yr_count <- df %>% 
  group_by(license_yr) %>% 
  count() %>% 
  rename(yr_n = n)
colors9 <- RColorBrewer::brewer.pal(name = "Spectral", n = 9)

crop_plot <- df %>% 
  group_by(license_yr, crop_main) %>% 
  count() %>% 
  left_join(yr_count) %>% 
  mutate(prop = round(100*n/sum(yr_n), 0)) %>% 
  ungroup() %>% 
  mutate(prop = case_when(
    prop == 29 & license_yr == 2009 ~ 28,
    prop == 39 & license_yr == 2014 ~ 38,
    prop == 55 & license_yr == 2015 ~ 56,
    T ~ prop)) %>% 
  ggplot(aes(x = license_yr, y = prop, fill = crop_main)) +
  geom_bar(stat = "identity") +
  #stat_identity(geom = "text", colour = "white", size = 3.5,
  #           aes(label = prop),
  #           position=position_stack(vjust=0.5)) +
  labs(x = "Year", y = "Percent", fill = "Crop",
       title = "Percent of license by crop over time") +
  scale_fill_manual(values = colors9[c(1,3,8)])+
  theme_linedraw() +
  theme(plot.title = element_text(hjust = 0.1, size = 10)) ; crop_plot

table(df$crop_fewer)
df$crop_main2 <- ifelse(df$crop_fewer == "Field crops", "Field crops",
                 ifelse(df$crop_fewer == "Perennial (fruit/nut)", 
                        "Perennial (fruit/nut)",
                 ifelse(df$crop_fewer == "Berries", "Berries",
                 ifelse(df$crop_fewer == "Roots (starch)", "Roots",
                 ifelse(df$crop_fewer == "Grain", "Grain", "Other")))))
crop_plot2 <- df %>% 
  group_by(license_yr, crop_main2) %>% 
  count() %>% 
  left_join(yr_count) %>% 
  mutate(prop = round(100*n/sum(yr_n), 0)) %>% 
  ungroup() %>% 
  mutate(prop = case_when(
    prop == 29 & license_yr == 2009 ~ 28,
    prop == 39 & license_yr == 2014 ~ 38,
    prop == 55 & license_yr == 2015 ~ 56,
    T ~ prop)) %>% 
  ggplot(aes(x = license_yr, y = prop, fill = crop_main2)) +
  geom_bar(stat = "identity") +
  #stat_identity(geom = "text", colour = "white", size = 3.5,
  #           aes(label = prop),
  #           position=position_stack(vjust=0.5)) +
  labs(x = "Year", y = "Percent", fill = "Crop",
       title = "Percent of license by crop over time") +
  scale_fill_manual(values = colors9[c(1,2,3,7,8)])+
  theme_linedraw() +
  theme(plot.title = element_text(hjust = 0.1, size = 10)) ; crop_plot2

yr_count3 <- df %>% 
  filter(!(company_size %in% c("Unknown", "Non-company"))) %>% 
  filter(!is.na(company_size)) %>% 
  group_by(license_yr) %>% 
  count() %>% 
  rename(yr_n = n)

company_plot <- df %>% 
  filter(!(company_size %in% c("Unknown", "Non-company"))) %>% 
  filter(!is.na(company_size)) %>% 
  group_by(license_yr, company_size) %>% 
  count() %>% 
  left_join(yr_count3) %>% 
  mutate(prop = round(100*n/sum(yr_n), 0)) %>% 
  ungroup() %>% 
  mutate(prop = case_when(
    prop == 48 & license_yr %in% c(2000, 2004) ~ 49,
    prop == 37 & license_yr == 2005 ~ 36,
    prop == 33 & license_yr == 2010 ~ 32,
    prop == 35 & license_yr == 2012 ~ 34,
    prop == 12 & license_yr == 2013 ~ 13,
    prop == 18 & license_yr == 2015 ~ 17,
    prop == 21 & license_yr == 2018 ~ 22,
    T ~ prop)) %>% 
  ggplot(aes(x = license_yr, y = prop, fill = company_size)) +
  geom_bar(stat = "identity") +
  #stat_identity(geom = "text", colour = "white", size = 3.5,
  #              aes(label = prop),
  #              position=position_stack(vjust=0.5)) +
  labs(x = "Year", y = "Percent", fill = "Company size",
       title = "Percent of license by company size over time") +
  scale_fill_manual(values = colors9[c(1,3,7,9)])+
  theme_linedraw() +
  theme(plot.title = element_text(hjust = 0.1, size = 10)) ; company_plot

licensee_plot <- df %>% 
  mutate(spatial_match = case_when(
    spatial_match == "Outside region" ~ "Extra-regional",
    spatial_match == "Within region" ~ "Regional",
    T ~ spatial_match
  )) %>% 
  group_by(license_yr, spatial_match) %>% 
  count() %>% 
  left_join(yr_count) %>% 
  mutate(prop = round(100*n/sum(yr_n), 0)) %>% 
  ggplot(aes(x = license_yr, y = prop, fill = spatial_match)) +
  geom_bar(stat = "identity") +
  stat_identity(geom = "text", colour = "white", size = 3.5,
                aes(label = prop),
                position=position_stack(vjust=0.5)) +
  labs(x = "Year", y = "Percent", fill = "Licensee location",
       title = "Percent of license by licensee over time") +
  scale_fill_manual(values = colors9[c(2,9)])+
  theme_linedraw() +
  theme(plot.title = element_text(hjust = 0.5, size = 10)) ; licensee_plot

yr_count2 <- df %>% 
  filter(agreement_type %in% c("Non-exclusive license", "Exclusive license")) %>% 
  group_by(license_yr) %>% 
  count() %>% 
  rename(yr_n = n)
agreement_plot <- df %>% 
  filter(agreement_type %in% c("Non-exclusive license", "Exclusive license")) %>% 
  group_by(license_yr, agreement_type) %>% 
  count() %>% 
  left_join(yr_count2) %>% 
  mutate(prop = round(100*n/sum(yr_n), 0)) %>% 
  ggplot(aes(x = license_yr, y = prop, fill = agreement_type)) +
  geom_bar(stat = "identity") +
  #stat_identity(geom = "text", colour = "white", size = 3.5,
  #              aes(label = prop),
  #              position=position_stack(vjust=0.5)) +
  labs(x = "Year", y = "Percent", fill = "Agreement restriction",
       title = "Percent of license by agreement type over time") +
  scale_fill_manual(values = colors9[c(1,8)])+
  theme_linedraw() +
  theme(plot.title = element_text(hjust = 0.1, size = 10)) ; agreement_plot


library(cowplot)
plot_grid(time_count_plot, agreement_plot, company_plot, crop_plot, labels = c('A', 'B', "C", "D"), label_size = 12, nrow = 2)
ggsave("~/Desktop/lgu_plots.png", width = 8, height = 4)

ggplot(df, aes(x = license_yr)) +
  geom_bar() +
  #geom_text(tab, aes(x = x, label = n)) + 
  labs(x = "Year", y = "Count", title = "Licenses over time by state") +
  theme_linedraw() +
  facet_wrap(~state, nrow = 3)


# By crop type?
table(df$crop, df$license_yr)

df %>% 
  count(license_yr, crop_fewer) %>% 
  group_by(license_yr) %>% 
  mutate(sum = sum(n)) %>% 
  mutate(percent = 100*(n/sum)) %>% 
  ggplot(aes(x = license_yr, y = percent, fill = crop_fewer)) +
  geom_bar(stat = "identity") +
  labs(x = "Year", y = "Percent", 
       fill = "Crop type",
       title = "Licenses over time by crop") +
  #scale_fill_viridis_d() +
  scale_fill_brewer(palette = "Spectral") +
  theme_linedraw()

df %>% 
  count(license_yr, crop_intermed) %>% 
  group_by(license_yr) %>% 
  mutate(sum = sum(n)) %>% 
  mutate(percent = 100*(n/sum)) %>% 
  ggplot(aes(x = license_yr, y = percent)) +
  geom_bar(stat = "identity") +
  geom_smooth() +
  labs(x = "Year", y = "Percent", 
       fill = "Crop type",
       title = "Licenses over time by crop") +
  #scale_fill_viridis_d() +
  theme_linedraw() +
  facet_wrap(~crop_intermed, scales = "free_y")

df %>% 
  count(state, license_yr, crop_fewer) %>% 
  group_by(state, license_yr) %>% 
  mutate(sum = sum(n)) %>% 
  mutate(percent = 100*(n/sum)) %>% 
  ggplot(aes(x = license_yr, y = percent, fill = crop_fewer)) +
  geom_bar(stat = "identity") +
  labs(x = "Year", y = "Percent", 
       fill = "Crop type",
       title = "Licenses over time by crop and state") +
  #scale_fill_viridis_d() +
  scale_fill_brewer(palette = "Spectral") +
  theme_linedraw() +
  facet_wrap(~state, nrow = 3)

# By exclusivity type?
table(df$agreement_type, df$license_yr) 

df %>% 
  count(license_yr, agreement_type) %>% 
  group_by(license_yr) %>% 
  mutate(sum = sum(n)) %>% 
  mutate(percent = 100*(n/sum)) %>% 
  ggplot(aes(x = license_yr, y = percent, fill = agreement_type)) + 
  geom_bar(stat = "identity") +
  labs(x = "Year", y = "Percent", 
       fill = "Agreement type",
       title = "Licenses over time by license agreement") +
  scale_fill_brewer(palette = "Spectral") +
  theme_linedraw()

df %>% 
  count(state, license_yr, agreement_type) %>% 
  group_by(state, license_yr) %>% 
  mutate(sum = sum(n)) %>% 
  mutate(percent = 100*(n/sum)) %>% 
  ggplot(aes(x = license_yr, y = percent, fill = agreement_type)) + 
  geom_bar(stat = "identity") +
  facet_wrap(~state, nrow = 3) +
  labs(x = "Year", y = "Percent", 
       fill = "Agreement type",
       title = "Licenses over time by license agreement and state") +
  scale_fill_brewer(palette = "Spectral") +
  theme_linedraw()

# By companies versus non companies?
df %>% 
  filter(!is.na(licensee_type)) %>% 
  count(license_yr, licensee_type) %>% 
  group_by(license_yr) %>% 
  mutate(sum = sum(n)) %>% 
  mutate(percent = 100*(n/sum)) %>% 
  ggplot(aes(x = license_yr, y = percent, fill = licensee_type)) + 
  geom_bar(stat = "identity") +
  labs(x = "Year", y = "Percent", 
       fill = "Licensee",
       title = "Licenses over time by licensee") +
  scale_fill_viridis_d() +
  theme_linedraw()

df %>% 
  filter(!is.na(licensee_type)) %>% 
  count(state, license_yr, licensee_type) %>% 
  group_by(state, license_yr) %>% 
  mutate(sum = sum(n)) %>% 
  mutate(percent = 100*(n/sum)) %>% 
  ggplot(aes(x = license_yr, y = percent, fill = licensee_type)) + 
  geom_bar(stat = "identity") +
  facet_wrap(~state, nrow = 3) +
  labs(x = "Year", y = "Percent", 
       fill = "Licensee",
       title = "Licenses over time by licensee and state") +
  scale_fill_viridis_d() +
  theme_linedraw()

# By company size?
df %>% 
  filter(company_size != "Unknown") %>% 
  count(license_yr, company_size) %>% 
  group_by(license_yr) %>% 
  mutate(sum = sum(n)) %>% 
  mutate(percent = 100*(n/sum)) %>% 
  ggplot(aes(x = license_yr, y = percent, fill = company_size)) + 
  geom_bar(stat = "identity") +
  labs(x = "Year", y = "Percent", 
       fill = "Company size",
       title = "Licenses over time by company size") +
  scale_fill_brewer(palette = "Spectral") +
  theme_linedraw()

df %>% 
  filter(company_size != "Unknown") %>% 
  count(state, license_yr, company_size) %>% 
  group_by(state, license_yr) %>% 
  mutate(sum = sum(n)) %>% 
  mutate(percent = 100*(n/sum)) %>% 
  ggplot(aes(x = license_yr, y = percent, fill = company_size)) + 
  geom_bar(stat = "identity") +
  facet_wrap(~state, nrow = 3) +
  labs(x = "Year", y = "Percent", 
       fill = "Company size",
       title = "Licenses over time by company size and state") +
  scale_fill_brewer(palette = "Spectral") +
  theme_linedraw()

df %>% 
  filter(company_size != "Unknown") %>% 
  ggplot(aes(x = factor(license_yr), y = rev_log)) + 
  geom_boxplot() 

# Within and outside of region
colors <- RColorBrewer::brewer.pal(name = "Spectral", n = 9)[c(3,8)]
df %>% 
  filter(spatial_match != "Unknown") %>% 
  mutate(spatial_match = case_when(
    domestic == F ~ "International",
    T ~ spatial_match)) %>% 
  count(license_yr, spatial_match) %>% 
  group_by(license_yr) %>% 
  mutate(sum = sum(n)) %>% 
  mutate(percent = 100*(n/sum)) %>% 
  ggplot(aes(x = license_yr, y = percent, fill = spatial_match)) + 
  geom_bar(stat = "identity") +
  labs(x = "Year", y = "Percent", 
       fill = "Licensee location",
       title = "Licenses over time by licensee geographic scope") +
  scale_fill_manual(values = colors) +
  theme_linedraw()


df %>% 
  filter(spatial_match != "Unknown") %>% 
  mutate(spatial_match = case_when(
    domestic == F ~ "International",
    T ~ spatial_match)) %>% 
  count(state, license_yr, spatial_match) %>% 
  group_by(state, license_yr) %>% 
  mutate(sum = sum(n)) %>% 
  mutate(percent = 100*(n/sum)) %>% 
  ggplot(aes(x = license_yr, y = percent, fill = spatial_match)) + 
  geom_bar(stat = "identity") + 
  facet_wrap(~ state) +
  labs(x = "Year", y = "Percent", 
       fill = "Licensee location",
       title = "Licenses over time by licensee geographic scope and state") +
  scale_fill_manual(values = colors) +
  theme_linedraw()

## CROP across the x axis ----
# What crops are being developed and licensed
ggplot(df, aes(x = crop_fewer)) +
  geom_bar() +
  stat_count(geom = "text", colour = "white", size = 3.5,
             aes(label = ..count..),
             position=position_stack(vjust=0.5)) +
  labs(x = "Crop", y = "Count", 
       title = "License totals by crop") +
  theme_linedraw() +
  theme(axis.text.x= element_text(angle = 45, hjust = 1)) 

crop_by_state <- data.frame(table(df$crop_fewer, df$state))

ggplot(df, aes(x = crop_fewer)) +
  geom_bar() +
  labs(x = "Crop", y = "Count", title = "License totals by crop and state") +
  theme_linedraw() +
  facet_wrap(~state, nrow = 3) +
  theme(axis.text.x= element_text(angle = 45, hjust = 1))

# By exclusivity type?
crop_by_agreement <- data.frame(table(df$crop_fewer, df$agreement_type))

df %>% 
  count(crop_fewer, agreement_type) %>% 
  group_by(crop_fewer) %>% 
  mutate(sum = sum(n)) %>% 
  mutate(percent = 100*(n/sum)) %>% 
  ggplot(aes(x = crop_fewer, y = percent, fill = agreement_type)) + 
  geom_bar(stat = "identity") +
  labs(x = "Crop", y = "Percent", 
       fill = "Agreement type",
       title = "Crop licenses by agreement type") +
  scale_fill_brewer(palette = "Spectral") +
  theme_linedraw() +
  theme(axis.text.x= element_text(angle = 45, hjust = 1))

df %>% 
  count(state, crop, agreement_type) %>% 
  group_by(state, crop) %>% 
  mutate(sum = sum(n)) %>% 
  mutate(percent = 100*(n/sum)) %>% 
  ggplot(aes(x = crop, y = percent, fill = agreement_type)) + 
  geom_bar(stat = "identity") +
  labs(x = "Crop", y = "Percent", 
       fill = "Agreement type",
       title = "Crop licenses by agreement type and state") +
  scale_fill_viridis_d() +
  theme_linedraw() +
  theme(axis.text.x= element_text(angle = 45, hjust = 1)) +
  facet_wrap(~state, nrow = 3)

# By companies versus non companies?

crop_by_licenseetype <- data.frame(table(df$crop_fewer, df$licensee_type))

df %>% 
  filter(!is.na(licensee_type)) %>% 
  count(crop_fewer, licensee_type) %>% 
  group_by(crop_fewer) %>% 
  mutate(sum = sum(n)) %>% 
  mutate(percent = 100*(n/sum)) %>% 
  ggplot(aes(x = crop_fewer, y = percent, fill = licensee_type)) + 
  geom_bar(stat = "identity") +
  labs(x = "Crop", y = "Percent", 
       fill = "Licensee",
       title = "Crop licenses by licensee") +
  scale_fill_manual(values = colors) +
  theme_linedraw() +
  theme(axis.text.x= element_text(angle = 45, hjust = 1))

df %>% 
  filter(!is.na(licensee_type)) %>% 
  count(state, crop_fewer, licensee_type) %>% 
  group_by(state, crop_fewer) %>% 
  mutate(sum = sum(n)) %>% 
  mutate(percent = 100*(n/sum)) %>% 
  ggplot(aes(x = crop_fewer, y = percent, fill = licensee_type)) + 
  geom_bar(stat = "identity") +
  labs(x = "Crop", y = "Percent", 
       fill = "Licensee",
       title = "Crop licenses by licensee and state") +
  scale_fill_manual(values = colors) +
  theme_linedraw() +
  theme(axis.text.x= element_text(angle = 45, hjust = 1)) +
  facet_wrap(~state, nrow = 3)

# By company size?

crop_by_companysize <- data.frame(table(df$crop, df$company_size))

df %>% 
  filter(company_size != "Unknown") %>% 
  count(crop_fewer, company_size) %>% 
  group_by(crop_fewer) %>% 
  mutate(sum = sum(n)) %>% 
  mutate(percent = 100*(n/sum)) %>% 
  ggplot(aes(x = crop_fewer, y = percent, fill = company_size)) + 
  geom_bar(stat = "identity") +
  labs(x = "Year", y = "Percent", 
       fill = "Company size",
       title = "Crop licenses by company size") +
  scale_fill_brewer(palette =  "Spectral") +
  theme_linedraw() +
  theme(axis.text.x= element_text(angle = 45, hjust = 1))

df %>% 
  filter(company_size != "Unknown") %>% 
  count(state, crop_fewer, company_size) %>% 
  group_by(state, crop_fewer) %>% 
  mutate(sum = sum(n)) %>% 
  mutate(percent = 100*(n/sum)) %>% 
  ggplot(aes(x = crop_fewer, y = percent, fill = company_size)) + 
  geom_bar(stat = "identity") +
  labs(x = "Year", y = "Percent", 
       fill = "Company size",
       title = "Crop licenses by company size and state") +
  scale_fill_brewer(palette =  "Spectral") +
  theme_linedraw() +
  theme(axis.text.x= element_text(angle = 45, hjust = 1)) +
  facet_wrap(~state, nrow = 3)

# Within and outside of region
crop_by_companylocation <- data.frame(table(df$crop_fewer, df$inregion))

df %>% 
  filter(spatial_match != "Unknown") %>% 
  mutate(spatial_match = case_when(
    domestic == F ~ "International",
    T ~ spatial_match)) %>% 
  count(crop_fewer, spatial_match) %>% 
  group_by(crop_fewer) %>% 
  mutate(sum = sum(n)) %>% 
  mutate(percent = 100*(n/sum)) %>% 
  ggplot(aes(x = crop_fewer, y = percent, fill = spatial_match)) + 
  geom_bar(stat = "identity") +
  labs(x = "Year", y = "Percent", 
       fill = "Licensee location",
       title = "Crop licenses by licensee geographic scope") +
  scale_fill_manual(values = colors) +
  theme_linedraw() +
  theme(axis.text.x= element_text(angle = 45, hjust = 1))

df %>% 
  filter(spatial_match != "Unknown") %>% 
  mutate(spatial_match = case_when(
    domestic == F ~ "International",
    T ~ spatial_match)) %>% 
  count(state, crop, spatial_match) %>% 
  group_by(state, crop) %>% 
  mutate(sum = sum(n)) %>% 
  mutate(percent = 100*(n/sum)) %>% 
  ggplot(aes(x = crop, y = percent, fill = spatial_match)) + 
  geom_bar(stat = "identity") +
  labs(x = "Year", y = "Percent", 
       fill = "Licensee location",
       title = "Crop licenses by licensee geographic scope and state") +
  scale_fill_viridis_d() +
  theme_linedraw() +
  theme(axis.text.x= element_text(angle = 45, hjust = 1)) +
  facet_wrap(~state, nrow = 3)


# ---- Combining with USDA funding data ----

# Trends in federal money contribution by state and licensing?

funding %>% 
  ggplot(aes(x = year, y = Award.Dollars/1000000)) +
  geom_bar(stat = "identity") +
  labs(x = "Year", y = "Public award dollars (1M)", 
       title = "USDA plant breeding funding (All 38 funded states)") +
  theme_linedraw() 

fund_df %>% 
  ggplot(aes(x = funding_yr_grpd, y = funding_amt_grpd/1000000)) +
  geom_bar(stat = "identity") +
  labs(x = "Year", y = "Public award dollars (1M)", 
       title = "USDA plant breeding funding by state") +
  theme_linedraw() +
  facet_wrap(~state, nrow = 5) +
  theme(axis.text.x= element_text(angle = 45, hjust = 1))

# This is different because this is only for the schools we have
funding %>% 
  filter(state %in% df$state) %>% 
  group_by(state) %>% 
  summarize(sum(Award.Dollars))

funding %>% 
  filter(state %in% df$state) %>% 
  ggplot(aes(x = year, y = Award.Dollars/1000000)) +
  geom_bar(stat = "identity") +
  labs(x = "Year", y = "Public award dollars (1M)", 
       title = "USDA plant breeding funding (14 states)") +
  theme_linedraw() 

df %>% 
  filter(!is.na(funding_yr_grpd)) %>% 
  select(state, funding_yr_grpd, funding_amt_grpd) %>% 
  unique() %>% 
  ggplot(aes(x = funding_yr_grpd, y = funding_amt_grpd/1000000)) +
  geom_bar(stat = "identity") +
  labs(x = "Year", y = "Public award dollars (1M)", 
       title = "USDA Plant Breeding Funding for states in our sample") +
  theme_linedraw() +
  facet_wrap(~state, nrow = 3) +
  theme(axis.text.x= element_text(angle = 45, hjust = 1))


# Exclusivity ~ crop_cat + company_revenue + (1 | grp_yr_license) + grp_amt
library(lme4)
factor(df$crop, levels = c("Field crops", ))
lm(rev_log ~ crop, df)

ex <- df %>% select(agreement_bi, funding_amt_grpd, rev) %>% 
  filter(agreement_bi == 1)
nex <- df %>% select(agreement_bi, funding_amt_grpd, rev) %>% 
  filter(agreement_bi == 0)

t.test(ex$funding_amt_grpd, nex$funding_amt_grpd)
# the mean of public funding is significantly higher for non-exclusive licenses than for exclusive licenses
t.test(ex$rev, nex$rev)
# the mean of licensee revenue is not significantly higher, though higher, for non-exclusive licenses than for exclusive licenses
df$agreement_bi_f <- factor(df$agreement_bi)
df$agreement_bi_f <- recode_factor(df$agreement_bi_f, '0' = "Non-exclusive", '1' = "Exclusive")

df %>% 
  filter(!is.na(agreement_bi_f)) %>% 
  ggplot(aes(x = agreement_bi_f, 
             y = funding_amt_grpd/1000000, 
             color = factor(agreement_bi_f))) +
  geom_boxplot() +
  scale_color_viridis_d() +
  theme_linedraw() +
  labs(x = "", y = "Public award dollars (1M)", color = "Agreement type")

df %>% 
  filter(!is.na(agreement_bi_f)) %>% 
  ggplot(aes(x = agreement_bi_f, 
             y = rev/1000000, 
             color = factor(agreement_bi_f))) +
  geom_boxplot() +
  scale_color_viridis_d() +
  theme_linedraw() +
  labs(x = "", y = "Licensee annual revenue (1M)", color = "Agreement type") +
  ylim(0,200)

inr <- df %>% select(inregion, funding_amt_grpd, rev) %>% 
  filter(inregion == 1)
outr <- df %>% select(inregion, funding_amt_grpd, rev) %>% 
  filter(inregion == 0)

t.test(inr$funding_amt_grpd, outr$funding_amt_grpd)
# the mean of public funding is significantly higher for out of region licenses than for in region licenses
t.test(inr$rev, outr$rev)
# the mean of licensee revenue is not significantly higher, though higher, for non-exclusive licenses than for exclusive licenses
df$inregion_f <- factor(df$inregion)
df$inregion_f <- recode_factor(df$inregion_f, '0' = "Outside region", '1' = "Within region")

df %>% 
  filter(!is.na(inregion_f)) %>% 
  ggplot(aes(x = inregion_f, 
             y = funding_amt_grpd/1000000, 
             color = factor(inregion_f))) +
  geom_boxplot() +
  scale_color_viridis_d() +
  theme_linedraw() +
  labs(x = "", y = "Licensee annual revenue (1M)", color = "Region of licensee")

df %>% 
  filter(!is.na(inregion_f)) %>% 
  ggplot(aes(x = inregion_f, 
             y = rev/1000000, 
             color = factor(inregion_f))) +
  geom_boxplot() +
  scale_color_viridis_d() +
  theme_linedraw() +
  labs(x = "", y = "Public award dollars (1M)", color = "Region of licensee")

inc <- df %>% select(domestic, funding_amt_grpd, rev) %>% 
  filter(domestic == T)
outc <- df %>% select(domestic, funding_amt_grpd, rev) %>% 
  filter(domestic == F)

t.test(inc$funding_amt_grpd, outc$funding_amt_grpd)
# the mean of public funding is significantly higher for out of country licenses than for out of country licenses
t.test(inc$rev, outc$rev)
# the mean of licensee revenue is not significantly higher, though higher, for non-exclusive licenses than for exclusive licenses


df %>% 
  filter(!is.na(inregion_f)) %>% 
  ggplot(aes(x = factor(license_yr), 
             y = rev/1000000)) +
  geom_boxplot() +
  scale_color_viridis_d() +
  theme_linedraw() +
  labs(x = "", y = "Licensee annual revenue (1M)") +
  ylim(0,2000)

summary(lm(license_yr ~ rev + funding_amt_grpd, data = df))

nlicenses <- df %>% 
  group_by(funding_yr_grpd, state) %>% 
  count()

bystate <- df %>% 
  group_by(state, funding_yr_grpd, funding_amt_grpd_log) %>% 
  summarize(avg_rev = mean(rev, na.rm = T)) %>% 
  left_join(nlicenses)

summary(lm(n ~ funding_amt_grpd_log + avg_rev, data = bystate))

ggplot(bystate, aes(x = funding_amt_grpd_log, y = n)) +
  geom_point() +
  geom_smooth(method = 'lm')

df.full <- df %>% 
  select(crop, funding_amt_grpd_log, license_yr_grpd, state) %>% 
  filter(complete.cases(.) == T)
multi <- nnet::multinom(factor(crop) ~ rev_log + funding_amt_grpd_log, data = df, Hess = T)
summary(multi)


df.full <- df %>% 
  select(agreement_bi, crop, rev_log, funding_amt_grpd_log, license_yr_grpd, state) %>% 
  filter(complete.cases(.) == T)

glm_agree <- glm(agreement_bi ~ crop + rev_log + funding_amt_grpd_log, 
                 family = binomial,  data = df.full)
summary(glm_agree)

glmer_agree <- glmer(agreement_bi ~ crop + rev_log + funding_amt_grpd_log + 
              (1 | state), #  (1|license_yr_grpd) + 
             family = binomial,  data = df.full)
summary(glmer_agree)

df.full <- df %>% 
  select(inregion, crop, rev_log, funding_amt_grpd_log, license_yr_grpd, state) %>% 
  filter(complete.cases(.) == T)

glm_inregion <- glm(inregion ~ crop + rev_log + funding_amt_grpd_log, 
                 family = binomial,  data = df.full)
summary(glm_inregion)

glmer_inregion <- glmer(inregion ~ crop + rev_log + funding_amt_grpd_log + 
                (1|state), # (1|license_yr_grpd)
             family = binomial,  data = df.full)
summary(glmer_inregion)

