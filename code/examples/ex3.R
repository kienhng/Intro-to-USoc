#*******************************************************************************
#  ** INTRODUCTION TO UNDERSTANDING SOCIETY USING STATA WORKSHOP
#*******************************************************************************
# set up
packages <- c('tidyverse','naniar','haven','sjlabelled')
pkg_notinstall <-  packages[!(packages %in% installed.packages()[,"Package"])]

lapply(pkg_notinstall, install.packages, dependencies = TRUE)
lapply(packages, library, character.only = TRUE)

rm(list=ls())

setwd("")
inpath <- "/"# Save your script, e.g. "ex3"

#*******************************************************************************
# EXAMPLE 3: MATCHING DATA FROM RESPONDENTS AT 2 OR MORE WAVES (LONG FORMAT)
#*******************************************************************************

# 3.1 COMBINING WAVE 1 AND WAVE 2 DATA INTO LONG FORMAT
#******************************************************
  
# For appending the wave 1 and wave 2 data files into long format we need to 
# (i) create a variable to identify which wave the data belongs to: 
# 1 for wave 1, 2 for wave 2,...
# (ii) drop the wave prefix from all variables so that R can recognize 
# the variables from two waves as the same variable.

# Let us start with Wave 1
a_indresp <- read_dta(file=paste0(inpath, "ukhls/a_indresp.dta"))

a_indresp <- a_indresp %>%
  select(a_hidp, pidp, a_istrtdaty, a_sex_dv, a_mastat_dv, a_julkjb, 
         a_sclfsato, a_paygu_dv)

# Needed to distinguish between waves
a_indresp$wave <- 1

# Rename variables to remove the wave prefix 
a_indresp <- a_indresp %>% 
  rename_at(vars(starts_with("a_")),
            ~str_replace(.,"a_", ""))
  
# Then we repeat these steps for wave 2
b_indresp <- read_dta(file=paste0(inpath, "ukhls/b_indresp.dta"))

b_indresp <- b_indresp %>%
  select(b_hidp, pidp, b_istrtdaty, b_sex_dv, b_mastat_dv, b_julkjb, 
         b_sclfsato, b_paygu_dv)

b_indresp$wave <- 2

b_indresp <- b_indresp %>% 
  rename_at(vars(starts_with("b_")),
            ~str_replace(.,"b_", ""))

# To combine these two files into long format we use the command "rbind"
indresp_ab_long <- rbind(a_indresp, b_indresp)

# 3.2 PRODUCING A BALANCED PANEL
#****************************************************************************
 
# recode values from -1 to -9 to system missing for all variables
missval <- c(-9, -8, -7, -2, -1)
for (i in 1:5) {
  indresp_ab_long <- indresp_ab_long %>%
    mutate_all(., list(~na_if(., missval[i])))
}

# derive variable capturing who responded wave 1, wave 2, or both
indresp_ab_long <- indresp_ab_long %>%
  group_by(pidp) %>%
  add_tally() %>%
  ungroup() %>%
  mutate(waves = factor(ifelse(n == 1 & wave == 1, 1,
                        ifelse(n == 1 & wave == 2, 2,
                        3)),
                        level = c(1, 2, 3),
                        label = c("wave 1","wave 2","both"))) %>%
  select(-n) 


# examine who responded wave 1, wave 2, or both
indresp_ab_long %>%
  distinct(pidp, .keep_all = T) %>%
  group_by(waves) %>%
  summarise(n=n()) %>%
  ungroup()

# Keep only those present in both waves: BALANCED PANEL
indresp_ab_long <- indresp_ab_long %>%
  filter(waves == "both") %>%
  select(-waves)


# 3.3 IDENTIFYING TRANSITIONS - CHANGE IN MARITAL STATUS AND PAY
#***************************************************************

# Identifying marital status transitions
# Recode marital status into smaller number of categories
# and look at types of marital change over time
# Combine married, cohabiting and in civil partnership into 1 category 
# in partnership.
# Combine separated, divorced, separated from civil partner into 1 category 
# "separated". As dates of formal divorce are generally much later than when 
# the couple first separated, researchers generally consider separation as the 
# more important event for partnership dissolution. 
# Combine widowed or civil partner deceased as 1 category - widowed
indresp_ab_long$ma <- 
  factor(ifelse(indresp_ab_long$mastat_dv %in% c(0,1),1,
         ifelse(indresp_ab_long$mastat_dv %in% c(2, 3, 10),2,
         ifelse(indresp_ab_long$mastat_dv %in% c(4, 5, 7, 8),3,
         ifelse(indresp_ab_long$mastat_dv %in% c(6, 9), 4, NA)))),
           level=c(1,2,3,4),
           label=c("single","in partnership","separated","widowed"))

# Create a 2-digit summary of marital change
indresp_ab_long <- indresp_ab_long %>%
  arrange(pidp, wave) %>%
  mutate(ma_num = as.numeric(ma)) %>%
  group_by(pidp) %>%
  mutate(ma_lag = lag(ma_num, n = 1, default = NA)) %>%
  mutate(mach = factor(ma_lag * 10 + as.numeric(ma_num),
          level=c(11,12,13,14,21,22,23,24,31,32,33,34,41,42,43,44),
          label=c("remains single", "was single, now in partnership ",
                  "was single, now separated", "was single, now widowed",
                  "was in partnership, now single", "remains in partnership",
                  "was in partnership, now separated",
                  "was in partnership, now widowed",
                  "was separated, now single",
                  "was separated, now in partnership", "still separated",
                  "was separated, now widowed", "was widowed, now single",
                  "was widowed, now in partnership",
                  "was widowed, now separated", "still widow"))) %>%
      ungroup() %>%
        select(-ma_num, -ma_lag)
attr(indresp_ab_long$mach, "label") <- "marital change"

indresp_ab_long[1:20,c("pidp", "mastat_dv","ma", "mach")]
  
# Measuring change in pay
indresp_ab_long <- indresp_ab_long %>%
  mutate(pay_num = as.numeric(paygu_dv)) %>%
  group_by(pidp) %>%
  mutate(pay_lag = lag(pay_num, n = 1, default = NA)) %>%
  mutate(paych = pay_num - pay_lag) %>%
  ungroup()

# Q: How does the change in income vary by sex?
indresp_ab_long %>%
  filter(!is.na(paych)) %>%
  group_by(sex_dv) %>%
  summarise(mean=mean(paych), sd=sd(paych), n=n()) %>%
  ungroup()

# Q: How does the change in pay vary by sex and type of marital status change?
indresp_ab_long %>%
  mutate(mach_num = as.numeric(mach)) %>%
  filter(!is.na(paych) & !is.na(mach_num)) %>%
  group_by(mach, sex_dv) %>%
  summarise(mean=mean(paych, na.rm=TRUE), n=n()) %>%
  ungroup() %>%
  print(n=29)

# Save the data file for later analyses

saveRDS(indresp_ab_long, "indresp_ab_long.rds")

# 3.4 COMBINING DATASETS INTO LONG FORMAT WHEN THERE ARE MORE THAN TWO WAVES
#***************************************************************************
 
# First we generate a regular expression that we will use to select the 
# variables we are interested in
var_list <- paste0("|._", # This means or and goes between variables,
             c("hidp","istrtdaty","sex_dv","mastat_dv","julkjb","paygu_dv", 
               "sclfsato"), # List of variables without wave prefixes
             collapse= "") # Tells R we wants this as one long character string
var_list <- paste0("pidp", var_list) # Adds the ID variable to the list


# We again start with Wave 1 - this time we add it to the long data frame we
# want to create - longfile
longfile <- read_dta(file=paste0(inpath, "ukhls/a_indresp.dta")) %>%
  select(matches(var_list)) %>%
  rename_at(vars(starts_with("a_")), ~str_replace(.,"a_", "")) %>%
  mutate(wave = 1) %>%
  add_labels(sclfsato, labels=c("Not available in IEMB"=-10))
# Loop to load, select, rename and generate a wave variable, before adding it 
# to longfile
for (wn in 2:6) {
  wl <- paste0(letters[wn],"_")
  wave_data <- read_dta(paste0(inpath, "ukhls/", wl, "indresp.dta")) %>%
    select(matches(var_list)) %>% 
    rename_at(vars(starts_with(wl)), ~str_replace(.,wl, "")) %>%
    mutate(wave = wn) 
  longfile <- rbind.data.frame(longfile, wave_data)
}

rm(wl, wn, var_list, wave_data)

# Clean up: delete temporary files no longer needed
rm(list=ls())  

# Last, don't forget to produce the log file for your script. 
# DO NOT ENTER THIS IN THE SCRIPT:
# 

