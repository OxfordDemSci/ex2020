# Report the quality of the input data

# Init ------------------------------------------------------------

library(here); library(glue)
library(dplyr); library(tidyr)
library(gt); library(ggplot2)

# Constants -------------------------------------------------------

wd <- here()
source(glue('{wd}/src/00-global.R'))

cnst <- list()
cnst <- within(cnst, {
  # harmonized data set
  path_harmonized = glue('{wd}/out/lt_input.rds')
})

dat <- list()
tab <- list()

# Data ------------------------------------------------------------

dat$lt_input <- readRDS(cnst$path_harmonized)

# Total deaths data quality table  --------------------------------

tab$total_deaths_quality <-
  dat$lt_input %>%
  filter(year %in% 2019:2020) %>%
  group_by(region_iso, year) %>%
  summarise(
    nageraw = min(death_total_q90nageraw),
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

tab$covid_deaths_quality <- 
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

# Coverage --------------------------------------------------------

stmf <- readRDS(glue('{wd}/dat/stmf/stmf.rds'))

# check STMF data coverage
stmf %>%
  mutate(
    Week =
      ifelse(Week == 'UNK', 0, Week) %>%
      as.numeric(),
    week_type = case_when(
      Week == 0 ~ 'UNK',
      Week == 53 ~ '53',
      TRUE ~ 'other'
    )
  ) %>%
  filter(Sex == 'f') %>%
  group_by(PopCode, Year, Week, week_type) %>%
  summarise(death = sum(Deaths)) %>%
  drop_na(death) %>%
  ggplot(aes(x = Week, y = Year, fill = week_type)) +
  geom_tile(height = 0.5) +
  scale_y_continuous(breaks = 2015:2021) +
  coord_cartesian(ylim = c(2015, 2021), xlim = c(0, 53)) +
  scale_fill_manual(values = c(UNK = 'blue', other = 'grey', `53` = 'red')) +
  facet_wrap(~PopCode) +
  theme_dark()


# Export ----------------------------------------------------------
