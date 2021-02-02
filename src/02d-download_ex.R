# Download data on life expectancy estimates


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
  url_wpp_ex = 'https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2019_Life_Table_Medium.csv'
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

# download World Population Prospects: Life Tables
dat$wpp_get <- map(
  c(wpp_ex = cnst$url_wpp_ex), ~{
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
        MidPeriod %in% seq(cnst$skeleton_first_year, cnst$skeleton_final_year, 1),
        Sex %in% c('Male','Female')
      )
  })

# Export ----------------------------------------------------------

# Save to file
saveRDS(
  dat$wpp_df,
  file = glue('{cnst$path_wpp}/wpp_ex.rds')
)
