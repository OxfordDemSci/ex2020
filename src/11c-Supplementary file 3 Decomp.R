
# Init ------------------------------------------------------------
library(here); library(glue)
library(data.table)
library(tidyverse)
library(gt); library(openxlsx)


# global const
wd <- here()
cnst <- list()
dat <- list()
tab <- list()

# load data
decomp_data.full <- readRDS(glue('{wd}/out/decomposition_results.rds'))

# groups by age and year
cnst$length.grouping <- 10
cnst$analysis.breaks.age   <- c(0,60,80)
cnst$analysis.breaks.years <- c(2015,2019,2020)
cnst$path_tmp <- glue('{wd}/tmp')

ids <- read_rds("{wd}/out/ids.rds" %>% glue)


# Analysis by age ---------------------------------------------------------
# create variable for more aggregate analysis
decomp_data.full$decomposition_results_by_age[, x.aggregated := cut(x, breaks=c(cnst$analysis.breaks.age ,Inf),ordered_result = T,include.lowest = T,right = F)]
decomp_data.full$decomposition_results_by_age_cause[, x.aggregated := cut(x, breaks=c(cnst$analysis.breaks.age ,Inf),ordered_result = T,include.lowest = T,right = F)]

# create variable for new year grouping
decomp_data.full$decomposition_results_by_age[, year.new := cut(year.initial, breaks=c(cnst$analysis.breaks.years),ordered_result = T,include.lowest = T,right = F)]
decomp_data.full$decomposition_results_by_age_cause[, year.new := cut(year.initial, breaks=c(cnst$analysis.breaks.years),ordered_result = T,include.lowest = T,right = F)]


# Prepare datasets for prelim figures  --------------------------------------------------------

#aggregate over the new grouping
dat$decomp_large_age_groups <- decomp_data.full$decomposition_results_by_age[!(is.na(contribution)),
                                                                             .(contribution = sum(contribution)), 
                                                                             by = .(region_iso,sex,cause,x.aggregated,year.new)]
names(dat$decomp_large_age_groups)[1] <- 'code'


dat$cause.group <- decomp_data.full$decomposition_results_by_age_cause[!(is.na(contribution)),
                                                                                         .(contribution = sum(contribution)), 
                                                                                         by = .(region_iso,sex,year.new,cause)]
names(dat$cause.group)[1] <- 'code'


age_decomp_table <- dat$decomp_large_age_groups %>% left_join(ids)
age_decomp_table$contribution <-  round(age_decomp_table$contribution, 3)
age_decomp_table <- dcast(age_decomp_table,  name + x.aggregated  ~ sex + year.new,value.var = 'contribution')

cause_decomp_table <- dat$cause.group %>% left_join(ids)
cause_decomp_table$contribution <-  round(cause_decomp_table$contribution, 3)
cause_decomp_table <- dcast(cause_decomp_table,  name + cause  ~ sex + year.new ,value.var = 'contribution')

# export results to supplementary tables

tab$tab_decomp_results <- createWorkbook()
addWorksheet(tab$tab_decomp_results, sheetName = 'Decomposition by age')
addWorksheet(tab$tab_decomp_results, sheetName = 'Decomposution by cause')
writeData(tab$tab_decomp_results, sheet = 'Decomposition by age', age_decomp_table)
writeData(tab$tab_decomp_results, sheet = 'Decomposution by cause', cause_decomp_table)
saveWorkbook(
  tab$tab_decomp_results, glue('{wd}/out/tab_decomp_results.xlsx'),
  overwrite = TRUE
)

