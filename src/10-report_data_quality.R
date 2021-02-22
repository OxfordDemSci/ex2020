# Report the quality of the input data

# Init ------------------------------------------------------------

library(here); library(glue); library(yaml)
library(dplyr); library(tidyr)
library(gt); library(ggplot2)

# Constants -------------------------------------------------------

wd <- here()
source(glue('{wd}/src/00-global.R'))

config <- read_yaml(glue('{wd}/cfg/config.yaml'))
cnst <- list()
cnst <- within(cnst, {
  regions_for_analysis = config$regions_for_all_cause_analysis
  # harmonized data set
  path_harmonized = glue('{wd}/out/lt_input.rds')
  path_out = glue('{wd}/out')
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
  mutate(
    included_in_all_cause_analysis = ifelse(
      region_iso %in% cnst$regions_for_analysis,
      'Regions included in all-cause life table analysis',
      'Regions excluded from all-cause life table analysis'
    )
  ) %>% 
  ungroup() %>%
  gt(groupname_col = 'included_in_all_cause_analysis') %>%
  cols_label(
    region_iso = 'Country',
    nageraw_2019 = '# raw age-groups 2019',
    nageraw_2020 = '# raw age-groups 2020',
    minopenageraw_2019 = 'Open-age group 2019',
    minopenageraw_2020 = 'Open-age group 2020',
    nweeksmiss_2020 = '# missing weeks 2020'
  ) %>%
  # show countries not used in analysis in grey
  tab_style(
    style = cell_text(color = 'lightgrey'),
    locations =
      cells_body(rows = !(region_iso %in% cnst$regions_for_analysis))
  )

gtsave(
  tab$total_deaths_quality, filename = 'tab-data_quality_all_cause_deaths.html',
  path = cnst$path_out
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
