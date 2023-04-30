rm(list = ls())

## Example using data.table
library(data.table)
library(haven)
library(dplyr)
library(tidyr)
library(stringr)

setwd("C:/Users/LLR User/OneDrive - The University of Nottingham/1. Others/Work/Intro-to-USoc")
dir <- "data/UKDA-6614-stata/stata/stata13_se/"

# read in the datafile a_indresp using read_dta from the haven package
a_indresp <- as.data.table(read_dta(file=paste0(dir, "ukhls/a_indresp.dta")))
b_indresp <- as.data.table(read_dta(file=paste0(dir, "ukhls/b_indresp.dta")))
keep.list.a = c("a_hidp", "pidp", "a_istrtdaty", "a_sex_dv", "a_mastat_dv", "a_julkjb", "a_sclfsato", "a_paygu_dv")
keep.list.b = c("b_hidp", "pidp", "b_istrtdaty", "b_sex_dv", "b_mastat_dv", "b_julkjb", "b_sclfsato", "b_paygu_dv")

a_indresp_dt <- a_indresp[, ..keep.list.a] ## data table
a_indresp_tib <- as_tibble(a_indresp_dt) ## tibble

b_indresp_dt <- b_indresp[, ..keep.list.b]
b_indresp_tib <- as_tibble(b_indresp_dt)

rm(a_indresp, b_indresp)

#---- Summary ----
sapply(a_indresp_dt, attr, "label") # get variable description
sapply(a_indresp_dt, attr, "labels") # get variable values description

## Summary between data.table vs dplyr
a_indresp_tib %>% 
  group_by(a_sex_dv) %>%
  summarise(n = n())
a_indresp_dt[, .N, by = "a_mastat_dv"]

#---- Recode missing values ----
missval <- c(-9, -8, -7, -2, -1)

## Using data.table
for (i in 1:length(missval)) {
  a_indresp_dt[a_indresp_dt == missval[i]] <- NA
  b_indresp_dt[b_indresp_dt == missval[i]] <- NA
}

## Using dplyr
for (i in 1:length(missval)) {
  a_indresp_tib <- a_indresp_tib %>% 
    mutate(across(everything(), ~na_if(., missval[i])))
  b_indresp_tib <- b_indresp_tib %>%
    mutate(across(everything(), ~na_if(., missval[i])))
}

#---- Merge with tidyr ----
indresp_wide_tib <- full_join(a_indresp_tib, b_indresp_tib, by = "pidp") # Wide merge
colnames(indresp_wide_tib)

indresp_long_tib <- indresp_wide_tib %>% # Transform to long format
  pivot_longer(
    cols = c(a_hidp, a_istrtdaty:b_paygu_dv),
    names_to = c("wave", ".value"),
    names_sep = "\\_",
    ) %>%
  mutate(wave = ifelse(wave == "a",1,2))

#---- Merge in data.table ----
# Wide merge
a_indresp_dt$wave <- 1
b_indresp_dt$wave <- 2

indresp_wide <- merge(a_indresp_dt, b_indresp_dt, all = TRUE, by = "pidp") # Merge with data.table 1
indresp_wide <- a_indresp_dt[b_indresp_dt, on = "pidp"] # Merge with data.table 2

# Long merge
## Rename column
names.a = colnames(a_indresp_dt)
colnames(a_indresp_dt) <- str_replace_all(names.a, "^a_", "")
colnames(b_indresp_dt) <- str_replace_all(names.a, "^a_", "")

a_indresp_dt$hidp <- as.numeric(a_indresp_dt$hidp)
indresp_long_dt <- rbind(a_indresp_dt, b_indresp_dt)

## 
indresp_long_dt[, .N, by = wave]
indresp_long_tib %>%
  group_by(wave) %>%
  summarise(n = n())