# Download data on population estimates and projections

# We download yearly population estimates and projections from WPP
# by sex age and region.

# Init ------------------------------------------------------------

library(here); library(glue)
library(httr); library(yaml); library(readr)
library(dplyr); library(tidyr); library(purrr)

# Constants -------------------------------------------------------

wd <- here()

config <- read_yaml(glue('{wd}/cfg/config.yaml'))
region_meta <- read_csv(glue('{wd}/cfg/region_metadata.csv'), na = '.')

cnst <- list()
cnst <- within(cnst, {
  url_wpp_estimates = 'https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2019_PopulationBySingleAgeSex_1950-2019.csv'
  url_wpp_projections = 'https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2019_PopulationBySingleAgeSex_2020-2100.csv'
  path_wpp = glue('{wd}/dat/wpp')
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

dat <- list()

# Download --------------------------------------------------------

# download World Population Prospects
# - single year-age population estimates 1950-2019
# - single year-age population projections 2020-2100
dat$wpp_get <- map(
  c(wpp_estimates = cnst$url_wpp_estimates,
    wpp_projections = cnst$url_wpp_projections), ~{
  GET(url = .x, progress())
})

# Subset to data of interest --------------------------------------

dat$wpp_df <-
  dat$wpp_get %>%
  map(~{
    content(.x, as = 'parsed', type = 'text/csv', encoding = 'UTF-8') %>%
      filter(
        # subset to regions of interest
        LocID %in% cnst$region_lookup$region_code_wpp,
        # subset to years of interest
        Time %in% seq(cnst$skeleton_first_year, cnst$skeleton_final_year, 1)
      )
  }) %>%
  # the population projections start one year after the estimates end
  # and both data source have the same format, so we can safely bind them
  bind_rows(.id = 'population_source')

# Export ----------------------------------------------------------

# Save to file
saveRDS(
  dat$wpp_df,
  file = glue('{cnst$path_wpp}/wpp_population.rds')
)
