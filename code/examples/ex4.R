#*******************************************************************************
#   INTRODUCTION TO UNDERSTANDING SOCIETY USING R WORKSHOP
#*******************************************************************************
# Set-up
packages <- c('tidyverse','naniar','haven','summarytools')
pkg_notinstall <-  packages[!(packages %in% installed.packages()[,"Package"])]
lapply(pkg_notinstall, install.packages, dependencies = TRUE)
lapply(packages, library, character.only = TRUE)

surveyurl <- "https://cran.r-project.org/src/contrib/Archive/survey/survey_4.0.tar.gz"
install.packages(surveyurl, repos=NULL, type="source")
library(survey)

rm(list=ls())
setwd("")
inpath <- "/"
# Save your script, e.g. "ex4"
# ******************************************************************************
#  DISTRIBUTING HOUSEHOLD LEVEL INFORMATION TO THE INDIVIDUAL LEVEL            *
# ******************************************************************************

# 4.1. HOUSEHOLD LEVEL VARIABLES
#*******************************

# read in the variables needed from the household level file
a_hhresp <- read_dta(paste0(inpath,"ukhls/a_hhresp.dta"))

# look at variable summary (n, mean, sd, min, max)
descr(a_hhresp, headings = FALSE, stats = c("mean", "sd", "min", "max"), 
      transpose = TRUE)
# use the variable search in the online documentation to search for variables 
# including the word "income" in a_hhresp (restrict by file hhresp and Wave 1)

# In what follows we will work with the household net income but as this is
# constructed from the gross income, which was imputed, let's look at the
# imputations for a_fihhmngrs_dv
summary(a_hhresp$a_fihhmngrs_if, digits = max(3, getOption("digits")-3))

# for continuous variables, it is useful to look at the distribution, e.g. via
# a histogramm
inc_imp_hist <- 
  qplot(as.numeric(a_hhresp$a_fihhmngrs_if), geom="histogram", bins=30)
# defaults to treating variable as continuous - this is fine
inc_imp_hist
ggsave(inc_imp_hist, file="inc_imp_hist.pdf")

# Q: What is the proportion of households for which income was imputed?
# How many households had none, more than 50%, or all of their household
# income imputed?
a_hhresp$imputedNone <- as.integer(a_hhresp$a_fihhmngrs_if==0)
a_hhresp$imputed50 <- as.integer(a_hhresp$a_fihhmngrs_if>0.5)
a_hhresp$imputedAll <- as.integer(a_hhresp$a_fihhmngrs_if==1)

a_hhresp %>%
  group_by(imputedNone) %>%
  summarise(n=n()) %>%
  mutate(Percent = (n / sum(n))*100,
  Cumulative = round(cumsum(freq = n / sum(n))*100,3)) %>%
  ungroup()

a_hhresp %>%
  group_by(imputed50) %>%
  summarise(n=n()) %>%
  mutate(Percent = (n / sum(n))*100,
  Cumulative = round(cumsum(freq = n / sum(n))*100,3)) %>%
  ungroup()

a_hhresp %>%
  group_by(imputedAll) %>%
  summarise(n=n()) %>%
  mutate(Percent = (n / sum(n))*100,
  Cumulative = round(cumsum(freq = n / sum(n))*100,3)) %>%
  ungroup()


# Q: What do the distributions of household monthly net and gross income look
# like? What are their mean, median, minimum and maximum values?
select(a_hhresp, a_fihhmngrs_dv, a_fihhmnnet1_dv) %>%
  summary()

a_fihhmnnet1_dv_hist <- 
  qplot(as.numeric(a_hhresp$a_fihhmnnet1_dv), geom="histogram", bins=50)

a_fihhmnnet1_dv_hist

ggsave(a_fihhmnnet1_dv_hist,file="a_fihhmnnet1_dv_hist.pdf")

# Recode all missings to system missings:
missval <- c(-9, -8, -7, -2, -1)
for (i in 1:5) {
  a_hhresp <- a_hhresp %>%
    mutate_all(., list(~na_if(., missval[i])))
}
# Summarise the data again
descr(a_hhresp, headings = FALSE, stats = c("mean", "sd", "min", "max"), 
      transpose = TRUE)

# Inspect equivalence scale:
summary(a_hhresp$a_ieqmoecd_dv)

# Analyse OECD scale and hh size
a_hhresp %>%
  group_by(a_hhsize) %>%
  summarise(mean_oecd=mean(a_ieqmoecd_dv, na.rm = TRUE),
            min_oecd=min(a_ieqmoecd_dv, na.rm = TRUE),
            max_oecd=max(a_ieqmoecd_dv, na.rm = TRUE)) %>%
  ungroup()

# generate household equivalized income. Use the houshold net income now!
a_hhresp$a_ehhnetinc1 <- 
  labelled(a_hhresp$a_fihhmnnet1_dv/a_hhresp$a_ieqmoecd_dv,
           label= "equivalized HH income", labels=NULL)
summary(a_hhresp$a_ehhnetinc1)

# Q: What proportion of households are one-person households?
# What proportion are couples without children?
a_hhresp %>%
  group_by(a_hhsize) %>%
  summarise(n=n()) %>%
  mutate(Percent = (n / sum(n))*100,
         Cumulative = round(cumsum(freq = n / sum(n)),3)*100) %>%
  ungroup()

a_hhresp %>%
  group_by(a_hhtype_dv) %>%
  summarise(n=n()) %>%
  mutate(Percent = (n / sum(n))*100,
         Cumulative = round(cumsum(freq = n / sum(n)),3)*100) %>%
  ungroup()

# Derive poverty indicator

# First, estimate the poverty line at the conventional 60% of median equivalised 
# net household income. You need to apply population weights to estimate this 
# value based on sample data, using svyquantile(). More information about how 
# this works will be provided in example 6.
a_hhresp_design <-
  svydesign(id=~a_psu, strata=~a_strata, weights=~a_hhdenus_xw, data=a_hhresp)

a_povline <- 
  svyquantile(~a_ehhnetinc1, a_hhresp_design, quantiles=c(0.5), na.rm=TRUE) %>%
             as.double() * 0.6
# display the poverty line
paste("The poverty line =", a_povline)

# Then generate poverty status
a_hhresp$a_pov <- as.integer(a_hhresp$a_ehhnetinc1 < a_povline) %>%
                  labelled(label = "Poverty status",
                           labels = c("Poor" = 1, "Not poor" = 0))
a_hhresp %>%
  group_by(a_pov) %>%
  summarise(n=n()) %>%
  mutate(Percent = (n / sum(n))*100,
         Cumulative = round(cumsum(freq = n / sum(n)),3)*100) %>%
  ungroup()

# keep only the variables used later
a_hhresp_ex4 <- a_hhresp %>%
  select(a_hidp, a_ehhnetinc1, a_hhsize, a_hhtype_dv, a_pov)

# remove the full hhresp dataframe, survey design and histograms from memory
rm(a_hhresp, a_hhresp_design, a_fihhmnnet1_dv_hist, inc_imp_hist)

# 4.2. INDIVIDUAL LEVEL FILE
#***************************

# read in the individual level file
a_indall <- read_dta(file=paste0(inpath, "ukhls/a_indall.dta"))

# Check the number of observations and variables

# Q: How many observations in the dataset?
count(a_indall)

# How many children younger than 16 are represented? (Tip: a_ivfio)
a_indall %>%
  group_by(a_ivfio) %>%
  summarise(n=n()) %>%
  mutate(Percent = (n / sum(n))*100,
         Cumulative = round(cumsum(freq = n / sum(n)),3)*100) %>%
  ungroup()

# keep only variables used later
a_indall_ex4 <- a_indall %>%
  select(pidp, a_hidp, a_agegr10_dv, a_psnenus_xw, a_strata, a_psu)

# remove the full indall dataframe
rm(a_indall)

# 4.3 MERGE HOUSEHOLD LEVEL VARIABLES ONTO INDIVIDUAL LEVEL FILE
#***************************************************************

# create variables, from_hh and from_indall, to indicate whether a record appears in the
# hh level and individual level files
# from_hh equals 1 for all observations in a_hhresp
a_hhresp_ex4$from_hh <- 1
# from_indall equals 1 for all observations from a_indall
a_indall_ex4$from_indall <- 1

# join the two dataframes using full_join
a_povstat <- full_join(a_hhresp_ex4, a_indall_ex4, by="a_hidp")

# check the results of the join
table(a_povstat$from_hh, a_povstat$from_indall, exclude=FALSE, deparse.level = 2)
  # these options ensure missing values are displayed in the table
  # this is needed as the from variables will be equal to either 1 or missing

# drop the variables which indicate where the record came from
a_povstat <- select(a_povstat, -from_indall, -from_hh)

# recode all missings to system missings
for (i in 1:5) {
  a_povstat <- a_povstat %>%
    mutate_all(., list(~na_if(., missval[i])))
}

# inspect the dataframe
summary(a_povstat)
head(a_povstat, 20)

# save the file to the working directory
# remember to include ".rds"
saveRDS(a_povstat, "a_povstat.rds")

# clean up: remove interim dataframes from memory
rm(a_hhresp_ex4, a_indall_ex4)

# 4.4. REPORT POVERTY RATES FOR DIFFERENT TYPES OF PEOPLE
#********************************************************


# proportion of people in poor households:
a_povstat %>%
  group_by(a_pov) %>%
  summarise(n=sum(a_psnenus_xw)) %>%
  mutate(Percent = (n / sum(n))*100,
         Cumulative = round(cumsum(freq = n / sum(n)),3)*100) %>%
  ungroup()

# proportion of people in poor households broken down by age groups:
a_povstat %>%
  group_by(a_agegr10_dv, a_pov) %>%
  summarise(n = sum(a_psnenus_xw)) %>%
  mutate(Percent = (n / sum(n))*100,
         Cumulative = round(cumsum(freq = n / sum(n)),3)*100) %>%
  ungroup() %>%
  print(n=27) 

# proportion of people in poor households broken down by household type:
a_povstat %>%
  group_by(a_hhtype_dv, a_pov) %>%
  summarise(n = sum(a_psnenus_xw)) %>%
  mutate(Percent = (n / sum(n))*100,
         Cumulative = round(cumsum(freq = n / sum(n)),3)*100) %>%
  ungroup() %>%
  print(n=41) 

# Last, don't forget to save your script and produce the log file. 