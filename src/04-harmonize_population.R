# Harmonize mid-year population counts

# (1) years prior to 2020 from World Population Prospects estimates
# - max age=100+
# (2) year 2020 from World Population Prospects projections
# - max age=100+
# (3) TODO: England & Wales, Northern Ireland and Scotland from HMD?

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
  # where to find the wpp population data
  path_wpp = glue('{wd}/dat/wpp')
  # skeleton path
  path_skeleton = glue('{wd}/tmp/harmonized_skeleton.rds')
  # translation of wpp sex code to harmonized sex code
  code_sex_wpp =
    c(`male` = config$skeleton$sex$Male,
      `female` = config$skeleton$sex$Female)
  # lookup table for region codes
  # only countries defined in skeleton
  region_lookup = 
    region_meta %>%
    filter(region_code_iso3166_2 %in% config$skeleton$region) %>%
    select(region_code_iso3166_2, region_code_wpp) %>%
    drop_na()
  # first year in harmonized data set
  skeleton_first_year = config$skeleton$year$min
  # last year in harmonized data set
  skeleton_final_year = config$skeleton$year$max
})

# Functions -------------------------------------------------------

source(cnst$path_global)

# Data ------------------------------------------------------------

dat <- list()

dat$skeleton <- readRDS(cnst$path_skeleton)

dat$wpp <- list()
dat$wpp <- readRDS(glue('{cnst$path_wpp}/wpp_population.rds'))

# Harmonize -------------------------------------------------------

dat$wpp_clean <-
  dat$wpp %>%
  # select columns of interest
  select(
    source_population,
    region_code_wpp = LocID,
    iso_year = Time, age = AgeGrpStart,
    male = PopMale, female = PopFemale
  ) %>%
  # sex to long format
  pivot_longer(
    cols = c(female, male),
    names_to = 'sex',
    values_to = 'population_july1st'
  ) %>%
  # population is in factor 1000, convert to actual number
  mutate(
    population_midyear = population_july1st*1000
  ) %>%
  # ensure proper names of factor variables
  mutate(
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
  mutate(id = GenerateRowID(region_iso, sex, age, iso_year)) %>%
  select(id, population_midyear, source_population)

# Join with skeleton ----------------------------------------------

# join the different sources of population count data
# with the skeleton
dat$joined <-
  dat$skeleton %>% 
  left_join(
    dat$wpp_clean,
    by = 'id'
  ) %>%
  select(id, population_midyear, source_population)

# Export ----------------------------------------------------------

saveRDS(
  dat$joined,
  file = glue('{cnst$path_harmonized}/harmonized_population.rds')
)
