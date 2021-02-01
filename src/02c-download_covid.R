# Download data on COVID-19 death counts from COVerAGE-DB

# Init ------------------------------------------------------------

library(here); library(glue)
library(readr); library(yaml)
library(httr); library(dplyr); library(tidyr)

# Constants -------------------------------------------------------

wd <- here()

config <- read_yaml(glue('{wd}/cfg/config.yaml'))
region_meta <- read_csv(glue('{wd}/cfg/region_metadata.csv'), na = '.')

cnst <- list()
cnst <- within(cnst, {
  # location of coverage-db file in 5 year age grouping
  url_covid = 'https://osf.io/7tnfh/download'
  path_tmp = glue('{wd}/tmp')
  path_coverage = glue('{wd}/dat/coverage')
  # lookup table for region codes
  # only countries defined in skeleton
  region_lookup = 
    region_meta %>%
    filter(region_code_iso3166_2 %in% config$skeleton$region) %>%
    select(region_code_iso3166_2, region_code_coverage) %>%
    drop_na()
})

dat <- list()

# Download --------------------------------------------------------

# download international COVID-19 death by age and sex counts
dat$coverage_zip <- GET(cnst$url_covid, progress())

# Unzip and bind to single table ----------------------------------

# save downloaded zip to file
writeBin(
  object = content(dat$coverage_zip, 'raw'),
  con = glue('{cnst$path_tmp}/coverage.zip')
)

# bind all .csv files in stmf zip archive into single file
dat$coverage <-
  unz(
    glue('{cnst$path_tmp}/coverage.zip'),
    filename = 'Data/Output_5.csv'
  ) %>%
  read_csv(
    col_types = 'ccccciiddd',
    skip = 3
  )

# subset to data of interest
dat$coverage_sub <-
  dat$coverage %>%
  filter(
    Sex %in% c('f', 'm'),
    # only country level
    Country %in% cnst$region_lookup$region_code_coverage,
    Region == 'All'
  )

# Export ----------------------------------------------------------

saveRDS(dat$coverage_sub, file = glue('{cnst$path_coverage}/coverage.rds'))
