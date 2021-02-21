# Report the quality of the input data

# Init ------------------------------------------------------------

library(here); library(glue)
library(dplyr); library(tidyr)
library(gt)

# Constants -------------------------------------------------------

wd <- here()
source(glue('{wd}/src/00-global.R'))

cnst <- list()
cnst <- within(cnst, {
  # harmonized data set
  path_harmonized = glue('{wd}/out/lt_input.rds')
})

dat <- list()

# Data ------------------------------------------------------------

dat$lt_input <- readRDS(cnst$path_harmonized)

# Total deaths data quality table  --------------------------------

dat$lt_input %>%
  filter(year %in% 2019:2020) %>%
  group_by(region_iso, year) %>%
  summarise(
    nageraw = min(death_total_minnageraw),
    nweeksmiss = max(death_total_nweeksmiss),
    minopenageraw = min(death_total_minopenageraw)
  ) %>%
  pivot_wider(names_from = year, values_from = c(nageraw, nweeksmiss, minopenageraw)) %>%
  select(-nweeksmiss_2019) %>% 
  ungroup() %>%
  gt() %>%
  cols_label(
    region_iso = 'Country',
    nageraw_2019 = '# raw age-groups 2019',
    nageraw_2020 = '# raw age-groups 2020',
    minopenageraw_2019 = 'Open-age group 2019',
    minopenageraw_2020 = 'Open-age group 2020',
    nweeksmiss_2020 = '# missing weeks 2020'
  )

# Covid deaths data quality table ---------------------------------

dat$lt_input %>%
  filter(year == 2020) %>%
  group_by(region_iso, year) %>%
  summarise(
    death_covid_date = min(death_covid_date)
  ) %>%
  select(-year) %>% 
  ungroup() %>%
  gt() %>%
  cols_label(
    region_iso = 'Country',
    death_covid_date = 'Latest COVID death updates'
  )

# Export ----------------------------------------------------------
