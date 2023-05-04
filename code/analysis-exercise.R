#---- Set up ----
## Needed packages
packages <- c('tidyverse','naniar','haven','summarytools','survey','data.table')
need.install <- packages[!(packages %in% installed.packages()[, "Package"])]
install.packages(need.install)
lapply(packages, library, character.only = TRUE)

rm(list = ls())

## Load data
inpath <- "C:/Users/LLR User/OneDrive - The University of Nottingham/1. Others/Work/Intro-to-USoc/data/UKDA-6614-stata/stata/stata13_se/"
a_hhresp <- as_tibble(read_dta(paste0(inpath,"ukhls/a_hhresp.dta")))

#---- Sample and Summary ----
## Create data dictionary
label <- c()
attributes(a_hhresp$a_strata)

for(i in 1:length(a_hhresp)) {
  label <- append(label, attr(a_hhresp[[i]], "label"))
}
label_dt <- data.table(var = colnames(a_hhresp),
                       label = label)

print("Hello World")

## Search for variables with "income" in description
income_vars <- c()
for (i in 1:nrow(label_dt)) {
  if (grepl("income", label_dt[i,label])) {
    income_vars <- append(income_vars, label_dt[i, var])
  } # (1) using long 'for loop'
}

income_vars <- label_dt[label %like% "income"][,var] # (2) using syntax of data.table

## Create sampled data for searched term
keeps = c(income_vars, "a_ehhnetinc1")

a_hhrespi <- as.data.table(a_hhresp[keeps])
label_dt[var %in% colnames(a_hhrespi)]

#---- Analysis and Graph ----
ggplot(a_hhrespi, aes(x = a_fihhmngrs_if)) +
  geom_histogram()

