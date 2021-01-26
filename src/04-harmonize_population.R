# Harmonize mid-year population counts

# (1) years prior to 2020 from World Population Prospects estimates
# - max age=100+
# (2) year 2020 from World Population Prospects projections
# - max age=100+
# (3) TODO: England & Wales, Northern Ireland and Scotland from HMD

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
  global_path = glue('{wd}/src/00-global.R')
  # where to put the harmonized data
  out_path_harmonized = glue('{wd}/tmp/harmonized_subsets')
  # skeleton path
  skeleton_path = glue('{wd}/tmp/harmonized_subsets/skeleton.rds')
  # translation of wpp sex code to harmonized sex code
  sex_code_wpp =
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

source(cnst$global_path)

# Data ------------------------------------------------------------

dat <- list()

dat$skeleton <- readRDS(cnst$skeleton_path)

dat$wpp <- list()
dat$wpp$estimates <- read_csv(glue('{wd}/dat/wpp/wpp_medium_pop_estimates.csv'))
dat$wpp$projections <- read_csv(glue('{wd}/dat/wpp/wpp_medium_pop_projections.csv'))

# Harmonize -------------------------------------------------------

# apply the same harmonization operations to both
# the wpp population estimates and the wpp population
# projections as both feature the same data layout
dat$wpp$clean <- map(dat$wpp, ~{
    .x %>%
    # select columns of interest
    select(
      region_code_wpp = LocID,
      iso_year = Time, age = AgeGrpStart,
      male = PopMale, female = PopFemale
    ) %>%
    # filter down to data of interest
    filter(
      iso_year >= cnst$skeleton_first_year,
      iso_year <= cnst$skeleton_final_year,
      region_code_wpp %in% cnst$region_lookup$region_code_wpp
    ) %>%
    # sex to long format
    pivot_longer(
      cols = c(female, male),
      names_to = 'sex',
      values_to = 'population_july1st'
    ) %>%
    # population is in factor 1000, convert to actual number
    mutate(
      population_july1st = population_july1st*1000
    ) %>%
    # ensure proper names of factor variables
    mutate(
      sex =
        factor(sex, levels = names(cnst$sex_code_wpp),
               labels = cnst$sex_code_wpp) %>%
        as.character(),
      region_iso = factor(
        region_code_wpp,
        levels = cnst$region_lookup$region_code_wpp,
        labels = cnst$region_lookup$region_code_iso3166_2
      ) %>% as.character()
    ) %>%
    # add row id
    mutate(id = GenerateRowID(region_iso, sex, age, iso_year)) %>%
    select(id, population_july1st)
})

# Join with skeleton ----------------------------------------------

# join the different sources of population count data
# with the skeleton
dat$joined <-
  dat$skeleton %>% 
  left_join(
    dat$wpp$clean$estimates %>%
      rename(population_july1st_wpp_estimates = population_july1st),
    by = 'id'
  ) %>%
  left_join(
    dat$wpp$clean$projections %>%
      rename(population_july1st_wpp_projections = population_july1st),
    by = 'id'
  )

# for each row, select which source to use for final
# population estimate variable
dat$population <-
  dat$joined %>%
  mutate(
    population_midyear =
      population_july1st_wpp_estimates,
    population_midyear =
      ifelse(is.na(population_midyear),
             population_july1st_wpp_projections,
             population_midyear)
  ) %>%
  select(id, population_midyear)

# Export ----------------------------------------------------------

saveRDS(
  dat$population,
  file = glue('{cnst$out_path_harmonized}/population.rds')
)
