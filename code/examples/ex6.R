#********************************************************************************
#  ** INTRODUCTION TO UNDERSTANDING SOCIETY USING R WORKSHOP
#********************************************************************************
# Set-up
packages <- c('tidyverse','naniar','haven','zoo', 'summarytools')
pkg_notinstall <-  packages[!(packages %in% installed.packages()[,"Package"])]
lapply(pkg_notinstall, install.packages, dependencies = TRUE)
lapply(packages, library, character.only = TRUE)
surveyurl <- "https://cran.r-project.org/src/contrib/Archive/survey/survey_4.0.tar.gz"
install.packages(surveyurl, repos=NULL, type="source")
library(survey)

rm(list=ls())
setwd("")
inpath <- "/"

# Save your script, e.g. "ex6"

#*******************************************************************************
# EXAMPLE 6: Working with weights and complex survey design 
#*******************************************************************************
  
# 6.2 DATA PREPARATION
#*********************

# Open the data
a_indresp <- read_dta(file=paste0(inpath, "ukhls/a_indresp.dta")) %>%
  select(a_hidp, pidp, a_mastat_dv, a_strata, a_psu, a_hhorig, 
         a_indinus_xw,a_indpxus_xw, a_ind5mus_xw, a_ivfio, a_sampst,
         a_age_dv, a_sex_dv, a_hiqual_dv, a_paygu_dv, a_country,
         a_mastat_dv, a_racel_dv, a_remit5, a_xtra5min_dv)

# Examine the data drawing on a range of different functions
view(dfSummary(a_indresp))  

descr(a_indresp,
      headings = FALSE, # remove headings
      stats = c("mean", "sd", "min", "max"),  # choose statistics
      transpose = TRUE)      # change layout

names(a_indresp)
summary(a_indresp)
table(a_indresp$a_remit5)

# find out whether a_remit_5 = -8 for a_xtra5min_dv == 0 cases
a_indresp %>%
  filter(a_xtra5min_dv == 0) %>% 
  group_by(a_remit5) %>%
  summarise(n=n()) 

# check the summary stats for the weights variables for different types of 
# respondents (first creating a smaller temporary df)
a_indw <- a_indresp %>% 
  select(a_indinus_xw, a_indpxus_xw, a_ind5mus_xw, a_ivfio, a_hhorig, a_sampst)

# Check: Are individual response weights 0 for proxy respondents?
stby(data = a_indw, INDICES = a_indw$a_ivfio, 
     FUN = descr, stats = c("mean", "sd", "min", "max"),  
     transpose = TRUE)

# Check: Are the Extra 5 minutes' weights zero for the NI sample?
stby(data = a_indw, INDICES = a_indw$a_hhorig, 
     FUN = descr, stats = c("mean", "sd", "min", "max"),  
     transpose = TRUE)

# Check: Are all wave 1 weights zero for TSMs? NB. In wave 1, the only reason to
# be a TSM is to be a non ethnic minority in the EMBS sample.
stby(data = a_indw, INDICES = a_indw$a_sampst, 
     FUN = descr, stats = c("mean", "sd", "min", "max"),  
     transpose = TRUE)
rm(a_indw)

# Confirm that there are many PSUs and strata in the GB sample but only 1 in the
# NI sample 
tmp_df <- a_indresp %>% select(a_psu, a_strata, a_hhorig) 
stby(data = tmp_df, INDICES = tmp_df$a_hhorig, FUN = descr, 
     stats = c("mean", "sd", "min", "max"), transpose = TRUE)
rm(tmp_df)

## 6.3 Notes on using weights and "svydesign" in R
# no code required.

## 6.4 Analysis: Estimating average monthly pay in UK
# replace missing values (integers -9 to -1) with NA
missval <- c(-9, -8, -7, -2, -1)
for (i in 1:5) {
  a_indresp <- a_indresp %>%
    mutate_all(., list(~na_if(., missval[i])))
}

# calculate the mean and standard error of pay for the sample
a_indresp %>% summarise(mean(a_paygu_dv, na.rm=T), 
            se=sd(a_paygu_dv, na.rm=T)/sqrt(sum(!is.na(a_paygu_dv))))

# Specify the survey design. 
svy_indresp <- svydesign(id=~a_psu, strata=~a_strata, 
                         weights=~a_indinus_xw, data=a_indresp)

# calculate the mean pay for the sample, accounting for complex survey design
svymean(~a_paygu_dv,svy_indresp, na.rm=TRUE)

# 6.5 Problem with strata with a single PSU
# specify the survey design
options(survey.lonely.psu="adjust")
# Changing the option should only change how lonely PSUs are treated in survey
# estimations. You do not need to re-specify the survey design but it is good
# to check.

# calculate the mean pay for the sample, accounting for complex survey design
svymean(~a_paygu_dv,svy_indresp,na.rm=TRUE)

## 6.6 Analysis: Estimating average monthly pay across the countries within UK
# England (a_country==1)
# unweighted mean, survey design ignored
a_indresp %>% filter(a_country == 1) %>% 
  summarise(mean(a_paygu_dv, na.rm=T), 
            se=sd(a_paygu_dv, na.rm=T)/sqrt(sum(!is.na(a_paygu_dv))))
# specify subpopulation
df_e<-subset(svy_indresp,a_country == 1) 
# report mean for subpopulation
svymean(~a_paygu_dv,design=df_e, na.rm=TRUE)   

# Repeat for Wales
a_indresp %>% filter(a_country == 2) %>% 
  summarise(mean(a_paygu_dv, na.rm=T), 
            se=sd(a_paygu_dv, na.rm=T)/sqrt(sum(!is.na(a_paygu_dv))))
df_w<-subset(svy_indresp,a_country == 2)
svymean(~a_paygu_dv,design=df_w, na.rm=TRUE)

# Repeat for Scotland 
a_indresp %>% filter(a_country == 3) %>% 
  summarise(mean(a_paygu_dv, na.rm=T), 
            se=sd(a_paygu_dv, na.rm=T)/sqrt(sum(!is.na(a_paygu_dv))))
df_s<-subset(svy_indresp,a_country == 3)
svymean(~a_paygu_dv,design=df_s, na.rm=TRUE)

# Repeat for Northern Ireland
a_indresp %>% filter(a_country == 4) %>% 
  summarise(mean(a_paygu_dv, na.rm=T), 
            se=sd(a_paygu_dv, na.rm=T)/sqrt(sum(!is.na(a_paygu_dv))))
df_n<-subset(svy_indresp,a_country == 4)
svymean(~a_paygu_dv,design=df_n, na.rm=TRUE)

# To test whether the average pay is different across countries:
svyttest(a_paygu_dv~a_country, svy_indresp)


## 6.7 Analysis: Estimating design effects
# calculate the mean pay for the sample, accounting for complex survey design
svymean(~a_paygu_dv,svy_indresp,na.rm=TRUE,deff=TRUE)
sqrt(40.4)

## 6.8 Analysis: how does remittance behaviour vary by socio-demographic 
#  characteristics? 

# drop when a_remit5<0
remit <- subset(a_indresp, a_remit5>=0,)
table(remit$a_remit5)

# create new variable, remit
remit$remit <- ifelse(remit$a_remit5==1, 0, 
                     ifelse(remit$a_remit5==0, 1, NA))
# check
table(remit$remit, remit$a_remit5)

# specify that these variables are factor variables
remit$a_sex_dv <- factor(remit$a_sex_dv)
remit$a_hiqual_dv <- factor(remit$a_hiqual_dv)
remit$a_mastat_dv <- factor(remit$a_mastat_dv)
remit$a_racel_dv <- factor(remit$a_racel_dv)

# estimate by logit
mylogit <- glm(remit ~ a_age_dv + a_sex_dv + a_hiqual_dv + a_country + 
                       a_mastat_dv + a_racel_dv, 
           data = remit, family = "binomial")
summary(mylogit)

# Weighted estimates
svy_remit <- svydesign(id = ~a_psu, strata = ~a_strata, weights = ~a_indinus_xw, 
                       data = remit)

weighted_logit <- svyglm(remit ~ a_age_dv + a_sex_dv + a_hiqual_dv + a_country 
                  + a_mastat_dv + a_racel_dv, design= svy_remit, 
                  family= "binomial")
summary(weighted_logit)

rm(mylogit,remit,svy_country,svy_indresp,svy_remit,weighted_logit, a_indresp)

## 6.9 Using longitudinal weights

# Generate a regular expression that we will use to select the 
# variables we are interested in
var_list <- paste0("|._", # This means or and goes between variables
                   c("paygu_dv", "strata", "psu"), # List of vars w/o prefixes
                   collapse= "") # treat as one long character string
var_list <- paste0("pidp", var_list) # Adds the ID variable to the list

# We start with Wave 1 
long <- read_dta(file=paste0(inpath, "ukhls/a_indresp.dta")) %>%
  select(matches(var_list)) %>%
  rename_at(vars(starts_with("a_")), ~str_replace(.,"a_", "")) %>%
  mutate(wave = 1)

# Loop to load, select, rename and generate a wave variable, before adding it 
# to long
for (wn in 2:3) {
  wl <- paste0(letters[wn],"_")
  wave_data <- 
    read_dta(paste0(inpath, "ukhls/", wl, "indresp.dta")) %>%
    select(matches(var_list)) %>% 
    rename_at(vars(starts_with(wl)), ~str_replace(.,wl, "")) %>%
    mutate(wave = wn)
  long <- rbind(long, wave_data)
}

# housekeeping
rm(wl, wn, var_list, wave_data)

# replace missing values (integers -9 to -1) with NA
missval <- c(-9, -8, -7, -2, -1)
for (i in 1:5) {
  long <- long %>% mutate_all(., list(~na_if(., missval[i])))
}

# add Wave 3 longitudinal weight variable to this appended file
long <- read_dta(file=paste0(inpath, "ukhls/c_indresp.dta")) %>%
  select(pidp, c_indinus_lw) %>% full_join(long, by="pidp")
long$c_indinus_lw[long$c_indinus_lw<0]<-NA

# Compute 3-year average pay using rollmean() with the allign=right to get 
# the trailing three year moving average
long <- long %>% group_by(pidp) %>%
  mutate(avg_wage = rollmean(paygu_dv, k = 3, fill = NA, align = "right")) %>%
  ungroup() 

# keep only wave 3 observations
long <- long %>% filter(wave ==3)
# specify survey design
svy_long <- 
  svydesign(id=~psu, strata=~strata, weights=~c_indinus_lw, data=long)

# Unweighted
long %>% summarise(mean(avg_wage, na.rm=T), 
            se=sd(avg_wage, na.rm=T)/sqrt(sum(!is.na(avg_wage))))
# Weighted
svymean(~avg_wage, design = svy_long, na.rm=TRUE)


# Optional part showing the data management step involved when working with a
# wide format data set

# Generate the generic variable names string:
var_list <- paste0("|._", # This means or and goes between variables
            c("paygu_dv", "strata", "psu"), # List of vars w/o prefixes
            collapse= "") # Treat as one long character string
var_list <- paste0("pidp", var_list) # Adds the ID variable to the list

# load wave 1 data
wide <- read_dta(file=paste0(inpath, "ukhls/a_indresp.dta")) %>%
  select(matches(var_list)) 

# Loop to load and select variables before adding to wide
for (wn in 2:3) {
  wl <- paste0(letters[wn],"_")
  wave_data <- read_dta(paste0(inpath,"ukhls/", wl,"indresp.dta")) %>%
    select(matches(var_list)) 
  wide <- full_join(wide, wave_data, by="pidp")
}
rm(wl, wn, var_list, wave_data)

# replace missing values (integers -9 to -1) with NA
missval <- c(-9, -8, -7, -2, -1)
for (i in 1:5) {
  wide <- wide %>% mutate_all(., list(~na_if(., missval[i])))
}

# add Wave 3 longitudinal weight variable to this appended file
wide <- read_dta(file=paste0(inpath, "ukhls/c_indresp.dta")) %>%
  select(pidp, c_indinus_lw) %>% full_join(wide, by="pidp")
wide$c_indinus_lw[wide$c_indinus_lw<0]<-NA

## keep observations if a_paygu_dv>= 0 b_paygu_dv>=0, c_paygu_dv>=0
wide <- wide %>% filter(a_paygu_dv>=0 & b_paygu_dv>=0 & c_paygu_dv>=0)

# create 3-year average wage and add to wide df
wide$avg_wage <- (wide$a_paygu_dv + wide$b_paygu_dv + wide$c_paygu_dv)/3


# specify survey design
svy_wide <- 
  svydesign(id=~c_psu, strata=~c_strata, weights=~c_indinus_lw, data= wide)

# unweighted mean:
wide %>% summarise(mean(avg_wage, na.rm=T),
            se=sd(avg_wage, na.rm=T)/sqrt(sum(!is.na(avg_wage))))
# Weighted
svymean(~avg_wage, svy_wide, na.rm=TRUE)

# Comparison with long format - Unweighted and weighted shows that the means and
# SE are the same across both formats:
long %>% summarise(mean(avg_wage, na.rm=T),
                   se=sd(avg_wage, na.rm=T)/sqrt(sum(!is.na(avg_wage))))
# Weighted
svymean(~avg_wage, design = svy_long, na.rm=TRUE)

rm(list=ls())