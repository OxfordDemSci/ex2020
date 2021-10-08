# Report the quality of the input data

# Init ------------------------------------------------------------

library(here); library(glue)
library(yaml); library(readr)
library(dplyr); library(tidyr)
library(gt); library(ggplot2)

# Constants -------------------------------------------------------

wd <- here()
source(glue('{wd}/src/00-global.R'))

region_meta <- read_csv(glue('{wd}/cfg/region_metadata.csv'))
config <- read_yaml(glue('{wd}/cfg/config.yaml'))
cnst <- list()
cnst <- within(cnst, {
  regions_for_all_cause_analysis =
    config$regions_for_all_cause_analysis
  regions_for_covid_cause_analysis =
    config$regions_for_covid_cause_analysis
  # harmonized data set
  path_harmonized = glue('{wd}/out/lt_input.rds')
  path_out = glue('{wd}/out')
})

source(glue('{wd}/cfg/fig_specs.R'))

dat <- list()
tab <- list()
fig <- list()

# Data ------------------------------------------------------------

dat$lt_input <- readRDS(cnst$path_harmonized)

# Total deaths data quality table  --------------------------------

tab$total_deaths_quality <-
  dat$lt_input %>%
  filter(year %in% 2019:2020) %>%
  group_by(region_iso, year) %>%
  summarise(
    rangenageraw = paste0('[',
                          min(death_total_minnageraw), ',',
                          max(death_total_maxnageraw), ']'),
    nweeksmiss = max(death_total_nweeksmiss),
    minopenageraw = min(death_total_minopenageraw)
  ) %>%
  pivot_wider(
    names_from = year,
    values_from = c(rangenageraw, nweeksmiss, minopenageraw)
  ) %>%
  ungroup() %>%
  # full country names
  mutate(
    include_in_all_cause_analysis =
      region_iso %in% config$regions_for_all_cause_analysis,
    include_in_covid_cause_analysis =
      region_iso %in% config$regions_for_covid_cause_analysis,
    region_name = factor(region_iso,
                         levels = region_meta$region_code_iso3166_2,
                         labels = region_meta$region_name
    )
  ) %>%
  # 2019 is completely observed, don't show in table
  select(region_iso, region_name, everything(), -nweeksmiss_2019) %>%
  gt() %>%
  cols_label(
    region_iso = 'Country ISO',
    region_name = 'Country',
    rangenageraw_2019 = 'Range # raw age-groups 2019',
    rangenageraw_2020 = 'Range # raw age-groups 2020',
    minopenageraw_2019 = 'Min. open-age group 2019',
    minopenageraw_2020 = 'Min. open-age group 2020',
    nweeksmiss_2020 = '# missing weeks 2020',
    include_in_all_cause_analysis = 'All cause analysis',
    include_in_covid_cause_analysis = 'Covid analysis'
  ) %>%
  cols_hide('region_iso') %>%
  tab_source_note('Source data retreived 2021-05-26.')

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

# Check STMF data coverage ----------------------------------------

stmf <- readRDS(glue('{wd}/dat/stmf/stmf.rds'))

# check STMF data coverage
fig$stmf_data_availability <-
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

# Check STMF age grouping patterns --------------------------------

dat$stmf_age_pattern <-
  stmf %>%
  group_by(PopCode, Year, Sex, Week) %>%
  summarise(age_pattern = paste0(Age, collapse = '-')) %>%
  group_by(PopCode, Year, Sex, age_pattern) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  pivot_wider(names_from = Sex, values_from = n)

#dat$stmf_age_pattern %>% View()

# Export ----------------------------------------------------------

fig_spec$ExportFigure(fig$stmf_data_availability, path = cnst$path_out)
