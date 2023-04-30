#********************************************************************************
#   INTRODUCTION TO UNDERSTANDING SOCIETY USING R WORKSHOP
#********************************************************************************

# BEFORE WE BEGIN: How to write "comments" in R syntax files?
# Comments are bits of text that should not be interpreted by R as commands

# R recognises lines starting with an hash sign (#) as comments
# R recognises anything written after hash sign (#) as comments

# NOTE: R can read multiple lines as one command as long as parentheses() or brackets[] have been used properly.

# set up
search() #search packages that are already installed

# Install and load the necessary packages
packages <- c('tidyverse', 'naniar', 'haven', 'survey')
pkg_notinstall <-  packages[!(packages %in% installed.packages()[,"Package"])]

lapply(pkg_notinstall, install.packages, dependencies = TRUE)
lapply(packages, library, character.only = TRUE)

rm(list=ls())    # remove any objects in current environment

# change to working directory - this is the directory R will save files to
# --> CHANGE THE WORKING DIRECTORY TO YOUR OWN PROJECT SPECIFIC FOLDER. 
setwd("C:/Users/LLR User/OneDrive - The University of Nottingham/1. Others/Work/Intro-to-USoc")

# set up a file path to data directory
# --> CHANGE THE FILE PATH TO WHERE THE DATA IS STORED
dir <- "data/UKDA-6614-stata/stata/stata13_se/"

# open log file
# split =TRUE means that all output will be sent to current screen as well as the log file
sink(file="Example2_output.log", split=TRUE)

#********************************************************************************
#  * EXAMPLE 2: MATCHING DATA FROM RESPONDENTS AT 2 WAVES (WIDE FORMAT)
#********************************************************************************

#  * 2.1  COMBINING WAVE 1 AND WAVE 2 DATA INTO WIDE FORMAT
#********************************************************

# read in the datafile a_indresp using read_dta from the haven package
a_indresp <- read_dta(file=paste0(dir, "ukhls/a_indresp.dta"))

# create a new dataframe containing only the variables a_hidp pidp a_istrtdaty a_sex_dv a_mastat_dv a_julkjb
# a_sclfsato a_paygu_dv
a_indresp_ex <- select(a_indresp, a_hidp, pidp, a_istrtdaty, a_sex_dv, a_mastat_dv, a_julkjb, a_sclfsato,
                     a_paygu_dv, a_hiqual_dv)

# remove the full a_indresp file to save memory
rm(a_indresp)

# look at first 20 obs
a_indresp_ex[1:20,]

# look at variable information
dim(a_indresp_ex) #dimension of the object a_indresp, 50994 lines 1389 columns
names(a_indresp_ex)# list column name of object

# look at variable summary (n, mean, min, max)
summary(a_indresp_ex)

# look at value labels for one variable
attr(a_indresp_ex$a_hiqual_dv, "labels")

# look at value labels for all variables
sapply(a_indresp_ex, attr, "labels")

# recode missing values for a_sex_dv  
a_indresp_ex <- a_indresp_ex %>%
  replace_with_na(replace = list(a_sex_dv=c(-9, -8, -7, -2, -1)))

# recode missing values for all variables  
missval <- c(-9, -8, -7, -2, -1)
for (i in 1:5) {
  a_indresp_ex <- a_indresp_ex %>%
    mutate_all(., list(~na_if(., missval[i])))
}

# Repeat the steps for wave 2
b_indresp <- read_dta(file=paste0(dir, "ukhls_w2/b_indresp.dta"))

b_indresp_ex <- select(b_indresp, b_hidp, pidp, b_istrtdaty, b_sex_dv, b_mastat_dv, b_julkjb, b_sclfsato,
                       b_paygu_dv, b_hiqual_dv)

for (i in 1:5) {
  b_indresp_ex <- b_indresp_ex %>%
    mutate_all(., list(~na_if(., missval[i])))
}

# merge the two files
a_indresp_ex$wave1=1
b_indresp_ex$wave2=1
indresp_ab_wide <- full_join(a_indresp_ex, b_indresp_ex, by="pidp")

# check how many records from wave 1, wave 2, both
table(indresp_ab_wide$wave1, indresp_ab_wide$wave2, exclude=FALSE, deparse.level = 2)

# look at interview dates
addmargins(table(indresp_ab_wide$a_istrtdaty, indresp_ab_wide$b_istrtdaty, exclude=FALSE))

# keep those from both files
indresp_ab_wide <- filter(indresp_ab_wide, wave1 == "1" & wave2 == "1")

# remove created wave variables
indresp_ab_wide <- select(indresp_ab_wide, -wave1, -wave2)


#* 2.2 IDENTIFYING TRANSITIONS - CHANGE IN MARITAL STATUS AND PAY
#****************************************************************

# look at frequencies of a_mastat_dv b_mastat_dv
indresp_ab_wide %>%
  group_by(a_mastat_dv) %>%
  summarise(n=n()) %>%
  mutate(Percent = (n / sum(n))*100,
         Cumulative = round(cumsum(freq = n / sum(n))*100,3))

indresp_ab_wide %>%
  group_by(b_mastat_dv) %>%
  summarise(n=n()) %>%
  mutate(Percent = (n / sum(n))*100,
         Cumulative = round(cumsum(freq = n / sum(n))*100,3))

# recode into summary variables ama, bma: (0 1=1) (2/3 10 = 2) (4/5 7/8 = 3) (6 9 = 4)
indresp_ab_wide$ama <- factor(
            ifelse(indresp_ab_wide$a_mastat_dv %in% c(0,1),1,
            ifelse(indresp_ab_wide$a_mastat_dv %in% c(2,3,10),2,
            ifelse(indresp_ab_wide$a_mastat_dv %in% c(4,5,7,8),3,
            ifelse(indresp_ab_wide$a_mastat_dv %in% c(6,9),4,NA)))),
            level=c(1,2,3,4),
            label=c("single","in partnership","separated","widowed"))

indresp_ab_wide$bma <- factor(
            ifelse(indresp_ab_wide$b_mastat_dv %in% c(0,1),1,
            ifelse(indresp_ab_wide$b_mastat_dv %in% c(2,3,10),2,
            ifelse(indresp_ab_wide$b_mastat_dv %in% c(4,5,7,8),3,
            ifelse(indresp_ab_wide$b_mastat_dv %in% c(6,9),4,NA)))),
            level=c(1,2,3,4),
            label=c("single","in partnership","separated","widowed"))

var_label(indresp_ab_wide$ama) <- "marital status wave 1"
var_label(indresp_ab_wide$bma) <- "marital status wave 2"

# calculate marital change mach=10*ama+bma
indresp_ab_wide$mach <- factor(10*as.numeric(indresp_ab_wide$ama) + as.numeric(indresp_ab_wide$bma),
             level=c(11,12,13,14,21,22,23,24,31,32,33,34,41,42,43,44),
              label=c("remains single",
                      "was single, now in partnership",
                      "was single, now separated",
                      "was single, now widowed",
                      "was in partnership, now single",
                      "remains in partnership",
                      "was in partnership, now separated",
                      "was in partnership, now widowed",
                      "was separated, now single",
                      "was separated, now in partnership",
                      "still separated",
                      "was separated, now widowed",
                      "was widowed, now single",
                      "was widowed, now in partnership",
                      "was widowed, now separated",
                      "still widow"))
var_label(indresp_ab_wide$mach) <- "marital change"

# look at first 20 obs of marital status variables
indresp_ab_wide[1:20,c("a_mastat_dv","ama","b_mastat_dv","bma", "mach")]

# compute change in pay paych
summary(indresp_ab_wide$a_paygu_dv)
summary(indresp_ab_wide$b_paygu_dv)
indresp_ab_wide$paych <- indresp_ab_wide$b_paygu_dv - indresp_ab_wide$a_paygu_dv

# summarise change in pay
summary(indresp_ab_wide$paych)

# tabulate change in pay by sex_dv
indresp_ab_wide %>%
  filter(!is.na(paych)) %>%
  group_by(a_sex_dv) %>%
  summarise(mean=mean(paych), sd=sd(paych), n=n())

# tabulate change in pay by sex_dv and marital status change
indresp_ab_wide %>%
  group_by(fct_explicit_na(indresp_ab_wide$mach, na_level = "(Missing)"), a_sex_dv) %>%
  summarise(mean=mean(paych, na.rm=TRUE), n=n()) %>%
  print(n=35)

# close the log file
sink()
