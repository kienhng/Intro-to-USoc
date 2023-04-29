## Example using data.table
install.packages("data.table")

library(data.table)
library(haven)

setwd("C:/Users/LLR User/OneDrive - The University of Nottingham/1. Others/Work/Intro-to-USoc")
dir <- "data/UKDA-6614-stata/stata/stata13_se/"

# read in the datafile a_indresp using read_dta from the haven package
a_indresp <- as.data.table(read_dta(file=paste0(dir, "ukhls/a_indresp.dta")))
keep.list = c("a_hidp", "pidp", "a_istrtdaty", "a_sex_dv", "a_mastat_dv", "a_julkjb", "a_sclfsato", "a_paygu_dv")

a_indresp_ex <- a_indresp[, ..keep.list]

dim(a_indresp_ex)
head(a_indresp_ex, 10)
