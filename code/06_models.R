library(dplyr)
library(nnet)
library(lme4)
library(brms)
setwd("~/Box/")
df <- readRDS("lgu/df_final.RDS")

# Q1: Does the amount of public funding affect the number of plant varieties that a university licenses?
## First we need to shape the data so that we can get our counts
rev <- df %>% 
  group_by(state, funding_yr_grpd) %>%
  summarize(mean_rev = mean(rev_log, na.rm = T)) %>% 
  filter(!is.na(mean_rev), !is.na(funding_yr_grpd)) %>% 
  unique() 
counts <- df %>% 
  group_by(state, funding_yr_grpd, funding_amt_grpd_log) %>% 
  count() %>% 
  filter(!is.na(funding_yr_grpd)) %>% 
  left_join(rev)
## Then we can model the effects of funding and state on the number of licenses:
poisson <- glm(n ~ funding_amt_grpd_log + mean_rev, data = counts, family = "poisson")
summary(poisson)
poisson1 <- glm(n ~ funding_amt_grpd_log + mean_rev + state, data = counts, family = "poisson")
summary(poisson1)
# We see a NEGATIVE SIGNIFICANT EFFECT between public funding and plant variety licenses -- so public money actually results in less licenses; while we see a small significant effect of revenue of the companies they license to -- basically that the more licenses you are producing the larger value companies you are licensing too. Further, the big the variation is all really about the state, where almost all states are licensing significantly less than California, except for Kansas, which licenses about the same, and Michigan, which licenses more

## Do we see the same thing when we fix the effect of state -- yes
poisson2 <- glmer(n ~ funding_amt_grpd_log + mean_rev + (1|state), data = counts, family = "poisson")
summary(poisson2)


# Q1: Does the amount of public funding affect the likelihood of having an exclusive license? (versus non-exclusive)

## First get our complete data 
df.full <- df %>% 
  select(agreement_bi, crop_fewer, funding_amt_grpd_log, 
         license_yr_grpd, state) %>% 
  filter(complete.cases(.) == T)

# Setting "field crops" as the baseline to which to compare all crops
df.full$crop_fewer <- factor(df.full$crop_fewer)
df.full$crop_fewer <- relevel(df.full$crop_fewer, "Field crops")

# Can we predict exclusive agreements based on crop type, revenue of licensee, and public funding amount?
glm_agree <- glm(agreement_bi ~ crop_fewer + funding_amt_grpd_log, 
                 family = binomial,  data = df.full)
summary(glm_agree)
# At first these results suggest that the more public funding you get, the more likely you are to have exclusive licenses, and the higher the rev
# Let's add in state as a fixed effect
glmer_agree <- glmer(agreement_bi ~ crop_fewer +  funding_amt_grpd_log + 
                       (1 | state), #  (1|license_yr_grpd) + 
                     family = binomial,  data = df.full)
summary(glmer_agree)
# Now it is now significant

fixef(glmer_agree) # Average intercept across all variables -- just the coefficients
table(df.full$state)
coef(glmer_agree)$state$`(Intercept)` # Random effects plus fixed effects; # extract the coefficient but different for each state-- this gives us intercepts bc we have no x and therefore no slope. 
unique(df.full$state)
ranef(glmer_agree)$state$`(Intercept)` # This will tell us how far the state is away from the overall mean
state_variation <- data.frame(dist.from.mean = ranef(glmer_agree)$state$`(Intercept)`,
           state = unique(df.full$state))

df.full <- df %>% 
  select(inregion, crop_fewer, funding_amt_grpd_log, 
         license_yr_grpd, state) %>% 
  filter(complete.cases(.) == T)

glm_inregion <- glm(inregion ~ crop_fewer + funding_amt_grpd_log, 
                    family = binomial,  data = df.full)
summary(glm_inregion)

glmer_inregion <- glmer(inregion ~ crop_fewer + funding_amt_grpd_log + 
                          (1|state), # (1|license_yr_grpd)
                        family = binomial,  data = df.full)
summary(glmer_inregion)


fixef(glmer_inregion) # Average intercept across all
coef(glmer_inregion)$state$`(Intercept)` # Random effects plus fixed effects; # extract the coefficient but different for each state-- this gives us intercepts bc we have no x and therefore no slope. 
unique(df.full$state)
ranef(glmer_inregion)$state$`(Intercept)` # This will tell us how far the judge is away from the overall mean
# judge 4 and 5 considerable lower, judge 3 considerable higher



# Q2: Does the amount of public funding affect which crops you breed?
# Now let's pare down the data for more models
df.full <- df %>% 
  select(crop_intermed, funding_amt_grpd_log, license_yr_grpd, state) %>% 
  filter(complete.cases(.) == T)

# Setting "field crops" as the baseline to which to compare all crops
df.full$crop_intermed <- factor(df.full$crop_intermed)
df.full$crop_intermed <- relevel(df.full$crop_intermed, "Field crops")

multi <- nnet::multinom(crop_intermed ~ funding_amt_grpd_log, 
                        data = df.full, Hess = T)
summary(multi)
