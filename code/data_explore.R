library(ggplot2)
library(lubridate)
library(dplyr)
# Set working director to Box
setwd("~/Box/")

funding <- read.csv("lgu/breeding_awards_combined.csv")
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


df <- read.csv("lgu/merged_df.csv") %>% 
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


df$company_size <- factor(df$company_size, levels = c("Large (100M+)",
                                                      "Medium ($10M - $100M)",
                                                      "Small ($328K - $10M)",
                                                      "Very small (<$350K)",
                                                      "Non-company", 
                                                      "Unknown"))

df <- select(df, -c(licensee, crop_name_scientific, variety_name, invention_name, 
                    department, inventor_last_name, inventor_last_name,
                    effective_date, end_date, reference_id, id, dba, description, 
                    industry, address, website, employees, revenue, year_started,
                    year_inc, esg_rank, type, uni_region, company_region))

# ---- Univariate ----

table(df$crop)
summary(df$rev/10000)
table(df$agreement_type)
table(df$spatial_match)

df %>% 
  count(company_size) %>% 
  mutate(sum = sum(n)) %>% 
  mutate(percent = 100*(n/sum)) %>% 
  filter(!is.na(company_size), !company_size %in% c("Unknown")) %>% 
  ggplot(aes(x = company_size, y = n)) +
  geom_bar(stat = "identity")

# --- Exploring the data: Looking at trends and patterns ----

## TIME across the x axis ----
# Are licenses generally going up or down?
tab <- as.data.frame(table(df$license_yr))
colnames(tab) <- c("x", "n")

ggplot(df, aes(x = license_yr)) +
  geom_bar() +
  #geom_text(tab, aes(x = x, label = n)) + 
  stat_count(geom = "text", colour = "white", size = 3.5,
             aes(label = ..count..),
             position=position_stack(vjust=0.5)) +
  labs(x = "Year", y = "Count", title = "Licenses over time") +
  theme_linedraw()

ggplot(df, aes(x = license_yr)) +
  geom_bar() +
  #geom_text(tab, aes(x = x, label = n)) + 
  labs(x = "Year", y = "Count", title = "Licenses over time by state") +
  theme_linedraw() +
  facet_wrap(~state, nrow = 3)


# By crop type?
table(df$crop, df$license_yr)

df %>% 
  count(license_yr, crop) %>% 
  group_by(license_yr) %>% 
  mutate(sum = sum(n)) %>% 
  mutate(percent = 100*(n/sum)) %>% 
  ggplot(aes(x = license_yr, y = percent, fill = crop)) +
  geom_bar(stat = "identity") +
  labs(x = "Year", y = "Percent", 
       fill = "Crop type",
       title = "Licenses over time by crop") +
  scale_fill_viridis_d() +
  theme_linedraw()

df %>% 
  count(license_yr, crop) %>% 
  group_by(license_yr) %>% 
  mutate(sum = sum(n)) %>% 
  mutate(percent = 100*(n/sum)) %>% 
  ggplot(aes(x = license_yr, y = percent)) +
  geom_bar(stat = "identity") +
  labs(x = "Year", y = "Percent", 
       fill = "Crop type",
       title = "Licenses over time by crop") +
  scale_fill_viridis_d() +
  theme_linedraw() +
  facet_wrap(~crop)

df %>% 
  count(state, license_yr, crop) %>% 
  group_by(state, license_yr) %>% 
  mutate(sum = sum(n)) %>% 
  mutate(percent = 100*(n/sum)) %>% 
  ggplot(aes(x = license_yr, y = percent, fill = crop)) +
  geom_bar(stat = "identity") +
  labs(x = "Year", y = "Percent", 
       fill = "Crop type",
       title = "Licenses over time by crop and state") +
  scale_fill_viridis_d() +
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
  scale_fill_viridis_d() +
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
  scale_fill_viridis_d() +
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
  scale_fill_viridis_d() +
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
  scale_fill_viridis_d() +
  theme_linedraw()

df %>% 
  filter(company_size != "Unknown") %>% 
  ggplot(aes(x = license_yr, y = rev_log)) + 
  geom_point() +
  geom_smooth()

# Within and outside of region
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
  scale_fill_viridis_d() +
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
  scale_fill_viridis_d() +
  theme_linedraw()

## CROP across the x axis ----
# What crops are being developed and licensed
ggplot(df, aes(x = crop)) +
  geom_bar() +
  stat_count(geom = "text", colour = "white", size = 3.5,
             aes(label = ..count..),
             position=position_stack(vjust=0.5)) +
  labs(x = "Crop", y = "Count", 
       title = "License totals by crop") +
  scale_fill_viridis_d() +
  theme_linedraw() +
  theme(axis.text.x= element_text(angle = 45, hjust = 1)) 

crop_by_state <- data.frame(table(df$crop, df$state))

ggplot(df, aes(x = crop)) +
  geom_bar() +
  labs(x = "Crop", y = "Count", title = "License totals by crop and state") +
  theme_linedraw() +
  facet_wrap(~state, nrow = 3) +
  theme(axis.text.x= element_text(angle = 45, hjust = 1))

# By exclusivity type?
crop_by_agreement <- data.frame(table(df$crop, df$agreement_type))

df %>% 
  count(crop, agreement_type) %>% 
  group_by(crop) %>% 
  mutate(sum = sum(n)) %>% 
  mutate(percent = 100*(n/sum)) %>% 
  ggplot(aes(x = crop, y = percent, fill = agreement_type)) + 
  geom_bar(stat = "identity") +
  labs(x = "Crop", y = "Percent", 
       fill = "Agreement type",
       title = "Crop licenses by agreement type") +
  scale_fill_viridis_d() +
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

crop_by_licenseetype <- data.frame(table(df$crop, df$licensee_type))

df %>% 
  filter(!is.na(licensee_type)) %>% 
  count(crop, licensee_type) %>% 
  group_by(crop) %>% 
  mutate(sum = sum(n)) %>% 
  mutate(percent = 100*(n/sum)) %>% 
  ggplot(aes(x = crop, y = percent, fill = licensee_type)) + 
  geom_bar(stat = "identity") +
  labs(x = "Crop", y = "Percent", 
       fill = "Licensee",
       title = "Crop licenses by licensee") +
  scale_fill_viridis_d() +
  theme_linedraw() +
  theme(axis.text.x= element_text(angle = 45, hjust = 1))

df %>% 
  filter(!is.na(licensee_type)) %>% 
  count(state, crop, licensee_type) %>% 
  group_by(state, crop) %>% 
  mutate(sum = sum(n)) %>% 
  mutate(percent = 100*(n/sum)) %>% 
  ggplot(aes(x = crop, y = percent, fill = licensee_type)) + 
  geom_bar(stat = "identity") +
  labs(x = "Crop", y = "Percent", 
       fill = "Licensee",
       title = "Crop licenses by licensee and state") +
  scale_fill_viridis_d() +
  theme_linedraw() +
  theme(axis.text.x= element_text(angle = 45, hjust = 1)) +
  facet_wrap(~state, nrow = 3)

# By company size?

crop_by_companysize <- data.frame(table(df$crop, df$company_size))

df %>% 
  filter(company_size != "Unknown") %>% 
  count(crop, company_size) %>% 
  group_by(crop) %>% 
  mutate(sum = sum(n)) %>% 
  mutate(percent = 100*(n/sum)) %>% 
  ggplot(aes(x = crop, y = percent, fill = company_size)) + 
  geom_bar(stat = "identity") +
  labs(x = "Year", y = "Percent", 
       fill = "Company size",
       title = "Crop licenses by company size") +
  scale_fill_viridis_d() +
  theme_linedraw() +
  theme(axis.text.x= element_text(angle = 45, hjust = 1))

df %>% 
  filter(company_size != "Unknown") %>% 
  count(state, crop, company_size) %>% 
  group_by(state, crop) %>% 
  mutate(sum = sum(n)) %>% 
  mutate(percent = 100*(n/sum)) %>% 
  ggplot(aes(x = crop, y = percent, fill = company_size)) + 
  geom_bar(stat = "identity") +
  labs(x = "Year", y = "Percent", 
       fill = "Company size",
       title = "Crop licenses by company size and state") +
  scale_fill_viridis_d() +
  theme_linedraw() +
  theme(axis.text.x= element_text(angle = 45, hjust = 1)) +
  facet_wrap(~state, nrow = 3)

# Within and outside of region
crop_by_companylocation <- data.frame(table(df$crop, df$inregion))

df %>% 
  filter(spatial_match != "Unknown") %>% 
  mutate(spatial_match = case_when(
    domestic == F ~ "International",
    T ~ spatial_match)) %>% 
  count(crop, spatial_match) %>% 
  group_by(crop) %>% 
  mutate(sum = sum(n)) %>% 
  mutate(percent = 100*(n/sum)) %>% 
  ggplot(aes(x = crop, y = percent, fill = spatial_match)) + 
  geom_bar(stat = "identity") +
  labs(x = "Year", y = "Percent", 
       fill = "Licensee location",
       title = "Crop licenses by licensee geographic scope") +
  scale_fill_viridis_d() +
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
  scale_fill_viridis_d() +
  theme_linedraw() 

fund_df %>% 
  ggplot(aes(x = funding_yr_grpd, y = funding_amt_grpd/1000000)) +
  geom_bar(stat = "identity") +
  labs(x = "Year", y = "Public award dollars (1M)", 
       title = "USDA plant breeding funding by state") +
  scale_fill_viridis_d() +
  theme_linedraw() +
  facet_wrap(~state, nrow = 5) +
  theme(axis.text.x= element_text(angle = 45, hjust = 1))

# This is different because this is only for the schools we have
df %>% 
  filter(!is.na(funding_yr_grpd)) %>% 
  select(state, funding_yr_grpd, funding_amt_grpd) %>% 
  unique() %>% 
  ggplot(aes(x = funding_yr_grpd, y = funding_amt_grpd/1000000)) +
  geom_bar(stat = "identity") +
  labs(x = "Year", y = "Public award dollars (1M)", 
       title = "USDA Plant Breeding Funding for 11 states in our sample") +
  scale_fill_viridis_d() +
  theme_linedraw() +
  facet_wrap(~state, nrow = 3) +
  theme(axis.text.x= element_text(angle = 45, hjust = 1))


# Exclusivity ~ crop_cat + company_revenue + (1 | grp_yr_license) + grp_amt
library(lme4)


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

