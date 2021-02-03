# Download data on population estimates and projections

# We download yearly population estimates and projections from WPP
# by sex age and region.

# For the sub-regions of the United Kingdom, i.e. Northern Ireland,
# England & Wales, and Scotland we download
#  - midyear population estimates by age and sex for 2015-2018 from HMD
#  - mortality rates by age and sex for 2018 from HMD
#  - fertility rates by age for 2018 from the HFD
# and then (in the harmonization step) project the midyear population
# 2019 and 2020 using a simple Leslie matrix approach

# Init ------------------------------------------------------------

library(here); library(glue)
library(httr); library(yaml); library(readr)
library(HMDHFDplus)
library(dplyr); library(tidyr); library(purrr)

# Constants -------------------------------------------------------

wd <- here()

config <- read_yaml(glue('{wd}/cfg/config.yaml'))
region_meta <- read_csv(glue('{wd}/cfg/region_metadata.csv'), na = '.')

cnst <- list()
cnst <- within(cnst, {
  # urls to wpp data
  url_wpp_estimates = 'https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2019_PopulationBySingleAgeSex_1950-2019.csv'
  url_wpp_projections = 'https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2019_PopulationBySingleAgeSex_2020-2100.csv'
  # paths
  path_wpp = glue('{wd}/dat/wpp')
  path_hmdhfd = glue('{wd}/dat/hmdhfd')
  # hmd credentials (be careful not to commit)
  hmd_usr = ''
  hmd_pwd = ''
  # hfd credentials (be careful not to commit)
  hfd_usr = ''
  hfd_pwd = ''
  # hmd and hfd region codes for UK subdivisions
  uk_region_codes_hmd = c(`GBRTENW` = 'GBRTENW', `GBR_NIR` = 'GBR_NIR', `GBR_SCO` = 'GBR_SCO')
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

# Download WPP data -----------------------------------------------

# download World Population Prospects
# - single year-age population estimates 1950-2019
# - single year-age population projections 2020-2100
dat$wpp_get <- map(
  c(wpp_estimates = cnst$url_wpp_estimates,
    wpp_projections = cnst$url_wpp_projections), ~{
  GET(url = .x, progress())
})

# Download HMDHFD data --------------------------------------------

# download hmd midyear population
# for the UK subdivisions
dat$gb_projection_raw$hmd_pop <-
  map(cnst$uk_region_codes_hmd, ~{
    readHMDweb(
      CNTRY = .x, item = 'Exposures_1x1',
      username = cnst$hmd_usr, password = cnst$hmd_pwd,
      fixup = TRUE
    )
  })

# download hmd death rates
# for the UK subdivisions
dat$gb_projection_raw$hmd_mort <-
  map(cnst$uk_region_codes_hmd, ~{
    readHMDweb(
      CNTRY = .x, item = 'Mx_1x1',
      username = cnst$hmd_usr, password = cnst$hmd_pwd,
      fixup = TRUE
    )
  })

# download hfd fertility rates
# for the UK subdivisions
dat$gb_projection_raw$hfd_fert <-
  map(cnst$uk_region_codes_hmd, ~{
    readHFDweb(
      CNTRY = .x, item = 'asfrRR',
      username = cnst$hfd_usr, password = cnst$hfd_pwd,
      fixup = TRUE
    )
  })

# Preliminary format of WPP data ----------------------------------

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


# Preliminary format of HMD data ----------------------------------

# subset to year range of interest and
# bundle all preliminaries for a projection
# (mortality, fertility, population) into a list for export

dat$gb_projection_export$hmd_pop <-
  dat$gb_projection_raw$hmd_pop %>%
  map(~{
    .x %>%
      select(Year, Age, Female, Male) %>%
      filter(Year >= cnst$skeleton_first_year)
  }) %>%
  bind_rows(.id = 'region_code_hmd')

dat$gb_projection_export$hmd_mort <-
  dat$gb_projection_raw$hmd_mort %>%
  map(~{
    .x %>%
      select(Year, Age, Female, Male) %>%
      filter(Year >= cnst$skeleton_first_year)
  }) %>%
  bind_rows(.id = 'region_code_hmd')


dat$gb_projection_export$hfd_fert <-
  dat$gb_projection_raw$hfd_fert %>%
  map(~{
    .x %>%
      select(Year, Age, ASFR) %>%
      filter(Year >= cnst$skeleton_first_year)
  }) %>%
  bind_rows(.id = 'region_code_hmd')

# Export ----------------------------------------------------------

# Save to file
saveRDS(
  dat$wpp_df,
  file = glue('{cnst$path_wpp}/wpp_population.rds')
)

saveRDS(
  dat$gb_projection_export,
  file = glue('{cnst$path_hmdhfd}/hmdhfd_gb_projection.rds')
)
