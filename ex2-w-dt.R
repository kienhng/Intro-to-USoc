## Example using data.table
install.packages("data.table")

library(data.table)
library(haven)
library(dplyr)

setwd("C:/Users/LLR User/OneDrive - The University of Nottingham/1. Others/Work/Intro-to-USoc")
dir <- "data/UKDA-6614-stata/stata/stata13_se/"

# read in the datafile a_indresp using read_dta from the haven package
a_indresp <- as.data.table(read_dta(file=paste0(dir, "ukhls/a_indresp.dta")))
keep.list = c("a_hidp", "pidp", "a_istrtdaty", "a_sex_dv", "a_mastat_dv", "a_julkjb", "a_sclfsato", "a_paygu_dv")

a_indresp_ex <- a_indresp[, ..keep.list]
a_indresp_tbl <- as_tibble(a_indresp_ex)

#---- Summary ----
str(a_indresp_ex)
sapply(a_indresp_ex, attr, "label") # get variable description
sapply(a_indresp_ex, attr, "labels") # get variable values description

dim(a_indresp_ex)
head(a_indresp_ex, 10)

summary(a_indresp_ex)
View(a_indresp_ex)

a_indresp_ex[,.N, by = "a_mastat_dv"]

a_indresp_tbl %>% 
  group_by(a_sex_dv) %>%
  summarise(n = n())
#---- Recode missing values ----

