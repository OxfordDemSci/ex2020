# Harmonize life expectancy estimates

# Init ------------------------------------------------------------

library(here); library(glue)
library(yaml)
library(readr); library(dplyr); library(tidyr); library(purrr)

# Constants -------------------------------------------------------

wd <- here()

config <- read_yaml(glue('{wd}/cfg/config.yaml'))
region_meta <- read_csv(glue('{wd}/cfg/region_metadata.csv'), na = '.')

cnst <- list()
cnst <- within(cnst, {
  # path to global objects
  path_global = glue('{wd}/src/00-global.R')
  # where to put the harmonized data
  path_harmonized = glue('{wd}/tmp')
  # where to find the life expectancy data
  path_wpp = glue('{wd}/dat/wpp')
  # skeleton path
  path_skeleton = glue('{wd}/tmp/harmonized_skeleton.rds')
  # translation of ex sex code to harmonized sex code
  code_sex_wpp =
    c(`Male` = config$skeleton$sex$Male,
      `Female` = config$skeleton$sex$Female)
  # lookup table for region codes
  # only countries defined in skeleton
  region_lookup = 
    region_meta %>%
    filter(region_code_iso3166_2 %in% config$skeleton$region) %>%
    select(region_code_iso3166_2, region_code_wpp) %>%
    drop_na()
})

# Functions -------------------------------------------------------

source(cnst$path_global)

# Data ------------------------------------------------------------

dat <- list()

dat$skeleton <- readRDS(cnst$path_skeleton)

dat$ex <- list()
dat$ex <- readRDS(glue('{cnst$path_wpp}/wpp_ex.rds'))

# Harmonize -------------------------------------------------------

dat$ex_clean <-
  dat$ex %>%
  # select columns of interest
  select(
    ex, sex = Sex,
    region_code_wpp = LocID, iso_year = MidPeriod, 
    age_start = AgeGrpStart, age_width = AgeGrpSpan
  ) %>%
  mutate(
    age_width = ifelse(age_width==-1, Inf, age_width),
    sex =
      factor(sex, levels = names(cnst$code_sex_wpp),
             labels = cnst$code_sex_wpp) %>%
      as.character(),
    region_iso = factor(
      region_code_wpp,
      levels = cnst$region_lookup$region_code_wpp,
      labels = cnst$region_lookup$region_code_iso3166_2
    ) %>% as.character()
  ) %>%
  # add row id
  mutate(id = GenerateRowID(region_iso, sex, age_start, iso_year)) %>%
  select(id, age_width, ex)

# Join with skeleton ----------------------------------------------

# Please be noted that the WPP life expectancy estimates use 
# 0, 1-4, & 5-year age groups instead of single-year age groups

# join the different sources of population count data
# with the skeleton
dat$joined <-
  dat$skeleton %>% 
  select(!age_width) %>%
  left_join(
    dat$ex_clean,
    by = 'id'
  ) %>%
  select(id, age_width, ex)
# keep the age_width column 
# because the ex data is not disaggregated by sing-year age group

# Export ----------------------------------------------------------

saveRDS(
  dat$joined,
  file = glue('{cnst$path_harmonized}/harmonized_ex.rds')
)