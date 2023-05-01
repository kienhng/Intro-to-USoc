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
# Save your script, e.g. "ex5"
options(digits=3, width = 100)  # added so a_hidp, pidp display fully when using
                                # head(df,20) to list cases
#********************************************************************************
#  * EXAMPLE 5: Aggregating individual-level information to the household
#********************************************************************************

#  * 5.1. Calculate HH level variables from individual level information
#*********************************************************************
  
#  Open the data and keep the variables need to create the variables:
#  number employed in the household
#  number age of the oldest male and oldest female in the household

a_indresp <- read_dta(paste0(inpath,"ukhls/a_indresp.dta")) %>%
  select(pidp, a_hidp, a_jbstat, a_age_dv, a_sex_dv, a_ivfio)

# Before creating the variable that counts the number of employed in the 
# household, inspect the main activity status variable a_jbstat
# NB Other measures are available for measuring employment status: w_JBSEMP,
# w_JBOFF, w_JBHAS, w_JBSEMP, etc. 
# Decide which is more suitable for your analysis.
attr(a_indresp$a_jbstat, "labels")
a_indresp %>%
  group_by(a_jbstat) %>%
  summarise(n=n()) %>%
  mutate(Percent = (n / sum(n))*100,
         Cumulative = round(cumsum(freq = n / sum(n)),3)*100) %>%
  ungroup()

# drop proxy respondents
# Note this may not always be appropriate, depends on the analysis
attr(a_indresp$a_ivfio, "labels")
a_indresp <- a_indresp %>%
    filter(a_ivfio != 2)

count(a_indresp)

# recode all missing values to system missing values
missval <- c(-9, -8, -7, -2, -1)
for (i in 1:5) {
  a_indresp <- a_indresp %>%
    mutate_all(., list(~na_if(., missval[i])))
}
summary(a_indresp)

# look at the distribution of a_jbstat again
a_indresp %>%
  group_by(a_jbstat) %>%
  summarise(n=n()) %>%
  mutate(Percent = (n / sum(n))*100,
         Cumulative = round(cumsum(freq = n / sum(n)),3)*100) %>%
  ungroup()


# derive number of HH members in work (employee or self-employed)
a_indresp_nwork <- a_indresp %>%
  filter(a_jbstat==1 | a_jbstat==2) %>%
    count(a_hidp, name="a_nwork") %>%
      full_join(a_indresp, by="a_hidp") %>%
        mutate_at(vars(a_nwork), ~replace(., is.na(.), 0))

a_indresp_nwork <- a_indresp_nwork %>%
  filter(is.na(a_jbstat)) %>%
    select(a_hidp) %>%
      mutate(a_misjbstat = 1) %>%
      full_join(a_indresp_nwork, by="a_hidp") %>%
        mutate(a_nwork = replace(a_nwork, a_misjbstat == 1, NA)) %>%
          select(-a_misjbstat) 
attr(a_indresp_nwork$a_nwork, "label") <- "no. of HH members in work"

a_indresp_nwork %>%
  group_by(a_nwork) %>%
  summarise(n=n()) %>%
  mutate(Percent = (n / sum(n))*100,
         Cumulative = round(cumsum(freq = n / sum(n)),3)*100)  %>%
  ungroup()

a_indresp_nwork <- a_indresp_nwork[order(a_indresp_nwork$a_hidp),]

a_indresp_nwork %>%
  select(pidp, a_hidp, a_jbstat, a_nwork) %>%
  head(n=20)

# derive age of oldest male adult HH member
mage <- a_indresp_nwork %>%
  filter(a_sex_dv==1)
a_indresp_nwork_hageom <- aggregate(mage$a_age_dv, 
                                    by=list(mage$a_hidp), max)
names(a_indresp_nwork_hageom) <- c("a_hidp", "a_hageom")
                                    
a_indresp_nwork <- 
  full_join(a_indresp_nwork, a_indresp_nwork_hageom, by="a_hidp") %>%
    mutate_at(vars(a_hageom), ~replace(., is.na(.), 0))

# inspect variables, e.g.:
head(a_indresp_nwork, 20)

# derive age of oldest female adult HH member
fage <- a_indresp_nwork %>%
  filter(a_sex_dv==2)

a_indresp_nwork_hageof <- 
        aggregate(fage$a_age_dv, by=list(fage$a_hidp), max)
names(a_indresp_nwork_hageof) <- c("a_hidp", "a_hageof")

a_indresp_nwork <- 
  full_join(a_indresp_nwork, a_indresp_nwork_hageof, by="a_hidp") %>%
    mutate_at(vars(a_hageof), ~replace(., is.na(.), 0))

# inspect variables, e.g.:
head(a_indresp_nwork, 20) 

# Housekeeping: remove temporary data frames
rm(mage, a_indresp_nwork_hageom, fage, a_indresp_nwork_hageof)
                      
# check how many unique values the new household-level variables have - it
# should be 1 at this stage
nhh_hageom <- aggregate(data=a_indresp_nwork, a_hageom ~ a_hidp, 
                        function(x) length(unique(x))) 
summary(nhh_hageom)    
nhh_hageof <- aggregate(data=a_indresp_nwork, a_hageof ~ a_hidp, 
                        function(x) length(unique(x)))
summary(nhh_hageof)    
nhh_nwork <- aggregate(data=a_indresp_nwork, a_nwork ~ a_hidp, 
                       function(x) length(unique(x)))
summary(nhh_nwork)
# Housekeeping: remove checking data frames
rm(nhh_hageom, nhh_hageof, nhh_nwork)

# keep only variables used later
a_indresp_nwork <- a_indresp_nwork %>%
  select(a_hidp, a_nwork, a_hageof, a_hageom)

# keep only one observation for every HH
a_nwork <- a_indresp_nwork %>%
  distinct(a_hidp, .keep_all = TRUE)

# count the number of observations in the data
count(a_nwork)

# save the file
saveRDS(a_nwork, file="a_nwork.rds")

# remove indresp and indresp_nwork from memory
rm(a_indresp, a_indresp_nwork)

# 5.2. Merge with individual level file created in Example 4
#************************************************************

# read in the file created in Example 4
a_povstat <- readRDS("a_povstat.rds")

# create an indicator to flag which file the observations appear in 
a_povstat$from_povstat <- 1
a_nwork$from_nwork <- 1

# join the files
a_povstat_nwork <- full_join(a_nwork, a_povstat, by="a_hidp")
  # ignore the warning - due to labelling differences

# check how many observations have come from each file or both
table(a_povstat_nwork$from_nwork, a_povstat_nwork$from_povstat, 
      exclude=FALSE, deparse.level = 2)

# drop individuals in HHs where none of the members completed the 
# interview (i.e. no data for that household in INDRESP)
# and drop the indicator variables
a_povstat_nwork <- a_povstat_nwork %>%
  filter(from_povstat==1 & from_nwork==1) %>%
  select(-from_povstat, -from_nwork)

# remove the indresp, povstat and nwork data frames from memory
rm(a_povstat, a_nwork)

# create indicators for pensioners living alone and single parent households
a_povstat_nwork$a_singleold <- 
  (a_povstat_nwork$a_hhtype_dv==1 | a_povstat_nwork$a_hhtype_dv==2)
a_povstat_nwork$a_singleparent <- 
  (a_povstat_nwork$a_hhtype_dv==4 | a_povstat_nwork$a_hhtype_dv==5)

table(a_povstat_nwork$a_singleold)
table(a_povstat_nwork$a_singleparent)

# 5.3. Analysis: how does poverty status vary with different characteristics?
# *****************************************************************************
  
summary(a_povstat_nwork)

table(a_povstat_nwork$a_hhsize, a_povstat_nwork$a_pov) 

chisq.test(a_povstat_nwork$a_hhsize, a_povstat_nwork$a_pov)
  # the warning message is due to a small number of observations in the larger 
  # household sizes

chisq.test(a_povstat_nwork$a_hhsize, a_povstat_nwork$a_pov, 
           simulate.p.value = TRUE)

chisq.test(a_povstat_nwork$a_hhtype_dv, a_povstat_nwork$a_pov)

a_povstat_nwork %>%
  group_by(a_pov) %>%
  summarise_at(vars(a_hageom, a_hageof), list(mean=mean)) %>%
  ungroup()

a_pov_glm <- glm(a_pov ~ a_nwork + a_hageof + a_hageom + a_hhsize 
                 + a_singleold + a_singleparent, data=a_povstat_nwork, 
                 family=binomial)  
summary(a_pov_glm)

# Last, don't forget to save your script and produce the log file. 
