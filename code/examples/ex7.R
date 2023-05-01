
#********************************************************************************
#  ** INTRODUCTION TO UNDERSTANDING SOCIETY USING R WORKSHOP
#********************************************************************************
# Set-up
packages <- c('tidyverse','naniar','haven')
pkg_notinstall <-  packages[!(packages %in% installed.packages()[,"Package"])]
lapply(pkg_notinstall, install.packages, dependencies = TRUE)
lapply(packages, library, character.only = TRUE)
rm(list=ls())

setwd("")
inpath <- "/"
# Save your script, e.g. "ex7"
options(width=100)
# ********************************************************************
#   EXAMPLE 7: HOW TO MATCH INFORMATION OF ANY TWO HOUSEHOLD MEMBERS?
# ********************************************************************

#  7.1 DATA PREPARATION: METHOD I
# ********************************
a_indresp <- read_dta(paste0(inpath,"ukhls/a_indresp.dta")) %>%
  select(a_hidp, a_pno, a_ppno, a_sex_dv, a_age_dv, a_jbstat, a_mastat_dv)

head(a_indresp, n=20)
summary(a_indresp)

sapply(a_indresp, attr, "labels")

attr(a_indresp$a_ppno, "labels")

table(a_indresp$a_ppno, exclude=FALSE, deparse.level = 2)

a_indresp %>% filter(a_ppno==0) %>% group_by(a_mastat_dv) %>%
  summarise(n=n()) %>% mutate(Percent = (n / sum(n))*100)

a_indresp %>% filter(a_ppno>0) %>% group_by(a_mastat_dv) %>%
  summarise(n=n()) %>% mutate(Percent = (n / sum(n))*100)

# prepare dataset with spouse/partner information
a_indresp_partners <- a_indresp %>% filter(a_ppno>0)

partners <- a_indresp_partners %>% select(-a_pno)

dim(a_indresp_partners)
dim(partners)

# create a flag to indicate where the observations have come from
a_indresp_partners$from_indresp <- 1
partners$from_partners <- 1

a_couples <- a_indresp_partners %>%
  full_join(partners, by=c("a_hidp","a_pno"="a_ppno"), suffix=c("",".p"))

# when a variable is already a labelled type, we can just edit the label
attr(a_couples$a_age_dv.p, "label") <- "spouse/partner's age"
attr(a_couples$a_sex_dv.p, "label") <- "spouse/partner's sex"
attr(a_couples$a_jbstat.p, "label") <- "spouse/partner's current econ activity"
attr(a_couples$a_mastat_dv.p, "label") <- "spouse/partner's marital status"

table(a_couples$from_indresp, a_couples$from_partners, 
      exclude=FALSE, deparse.level = 2)

head(a_couples, n=20)

# Taking a look at the matched cases
a_couples %>% filter(from_indresp==1 & from_partners==1) %>% head(n=20)

# Taking a look at the unmatched cases
a_couples %>% filter(from_indresp==1 & is.na(from_partners)) %>% head(n=20)

a_couples %>% filter(is.na(from_indresp) & from_partners==1) %>% head(n=20)

# Keep only matched cases, that is, where both partners are present in the HH
# and both are respondents.
a_couples <- a_couples %>% filter(from_indresp==1 & from_partners==1) %>%
  select(-from_indresp, -from_partners)

saveRDS(a_couples, "a_couples_1.rds")

rm(a_couples, a_indresp_partners, partners)

#  7.2 DATA PREPARATION: METHOD II
# *********************************

# Drop cases who are single or partner/spouse is not present in HH.
a_indresp_partners <- a_indresp %>% filter(a_ppno>0)

# Create within-household unique partnership identifier.
a_indresp_partners$a_partnum <- 
   labelled(pmin(a_indresp_partners$a_pno, a_indresp_partners$a_ppno),
            label="Partnership identifier, within household", labels=NULL)

table(a_indresp_partners$a_partnum)

# Count the number of respondents for each partnership ID
# Those with non-responding spouses will have a_numinpart=1.
a_indresp_partners <- a_indresp_partners %>% count(a_hidp, a_partnum) %>%
  full_join(a_indresp_partners, by=c("a_hidp", "a_partnum")) %>%
    rename(a_numinpart = n)

table(a_indresp_partners$a_numinpart)

a_indresp_partners %>% select(a_hidp, a_partnum, a_pno, a_ppno, a_numinpart) %>%
  head(n=50)

# Keep only matched cases, that is, where both partners are present in the HH
# and both are respondents, and remove the numinpart variable.
a_indresp_partners <- a_indresp_partners %>% filter(a_numinpart==2) %>%
  select(-a_numinpart)

# create the dataframe of partners' characteristics and then join
partners <- a_indresp_partners %>% select(-a_pno)

# create a flag to indicate where the observations have come from
a_indresp_partners$from_indresp <- 1
partners$from_partners <- 1

a_couples <- a_indresp_partners %>%
  full_join(partners, by=c("a_hidp","a_pno"="a_ppno"), suffix=c("",".p"))
  # warning that column has different attributes. this is due to labelling 
  # differences and can be ignored

attr(a_couples$a_age_dv.p, "label") <- "spouse/partner's age"
attr(a_couples$a_sex_dv.p, "label") <- "spouse/partner's sex"
attr(a_couples$a_jbstat.p, "label") <- "spouse/partner's current econ activity"
attr(a_couples$a_mastat_dv.p, "label") <- "spouse/partner's marital status"

table(a_couples$from_indresp, a_couples$from_partners, 
      exclude=FALSE, deparse.level = 2)
  # this time all of the observations match, as we already removed those where 
  # the partner was not in the dataset

# remove the indicator variables
a_couples <- a_couples %>% select(-from_indresp, -from_partners)

head(a_couples, n=20)

saveRDS(a_couples, "a_couples_2.rds")

rm(a_indresp, a_indresp_partners, partners)

# 7.3 ANALYSIS: EXPLORE VARIATION IN AGE AMONG COUPLES
# *****************************************************

# drop same-sex couples and keep observations with respondent is a woman, 
# partner is a man
a_couples_ms <- a_couples %>% filter(a_sex_dv!=a_sex_dv.p) %>% 
  filter(a_sex_dv==2)

# Assortative matching patterns
table(a_couples_ms$a_sex_dv, a_couples_ms$a_sex_dv.p)
table(a_couples_ms$a_jbstat, a_couples_ms$a_jbstat.p)
prop.table(table(a_couples_ms$a_jbstat, a_couples_ms$a_jbstat.p),1)

# Create plots and calculations
age_plot <- qplot(as.numeric(a_age_dv.p), as.numeric(a_age_dv),
                  data=a_couples_ms, xlab="age of male partner",
      ylab="age of woman", geom="point")
age_plot
ggsave(age_plot, file="age_plot.pdf")

cor.test(a_couples_ms$a_age_dv.p, a_couples_ms$a_age_dv)

a_couples_ms$diff_age <- a_couples_ms$a_age_dv.p - a_couples_ms$a_age_dv

diff_age_plot <- qplot(as.numeric(a_couples_ms$diff_age), geom="histogram", 
                       bins=50, main="Difference in age between a woman and her
                       male partner", xlab="age difference")
diff_age_plot
ggsave(diff_age_plot, file="diff_age_plot.pdf")

# returning to the file containing all couples, again keep one row per couple, 
# this time using aggregate
a_couples_1per <- a_couples %>%
  aggregate(by=list(a_couples$a_hidp, a_couples$a_partnum), FUN="head", n=1)

# generate variables for age difference and a flag for same sex couples
a_couples_1per$agediff <- 
   abs(a_couples_1per$a_age_dv - a_couples_1per$a_age_dv.p)
attr(a_couples_1per$agediff, "label") <- 
    "Absolute value of age difference of couple"
a_couples_1per$samesex <- 
  a_couples_1per$a_sex_dv == a_couples_1per$a_sex_dv.p
attr(a_couples_1per$samesex, "label") <- "Same sex couple"

# Use a t-test
t.test(agediff ~ samesex, data=a_couples_1per, var.equal = T)

# Last, don't forget to save your script and produce the log file. 