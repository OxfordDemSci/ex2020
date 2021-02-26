# Download data on external life expectancy estimates

# (1) Life expectancy estimates from WPP
# (2) Life expectancy estimates from HMD

# Init ------------------------------------------------------------

library(here); library(glue)
library(httr); library(yaml); library(readr)
library(dplyr); library(tidyr); library(purrr)
library(HMDHFDplus)

# Constants -------------------------------------------------------

wd <- here()

config <- read_yaml(glue('{wd}/cfg/config.yaml'))
region_meta <- read_csv(glue('{wd}/cfg/region_metadata.csv'), na = '.')

cnst <- list()
cnst <- within(cnst, {
  url_wpp_ex = 'https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2019_Life_Table_Medium.csv'
  path_wpp = glue('{wd}/dat/wpp')
  path_hmdhfd = glue('{wd}/dat/hmdhfd')
  # hmd credentials (be careful not to commit)
  hmd_usr = ''
  hmd_pwd = ''
  # lookup table for wpp region codes
  # only countries defined in skeleton
  region_lookup_wpp = 
    region_meta %>%
    filter(region_code_iso3166_2 %in% config$skeleton$region) %>%
    pull(region_code_wpp) %>% na.omit()
  names(region_lookup_wpp) <- region_lookup_wpp
  # lookup table for hmd region codes
  # only countries defined in skeleton
  region_lookup_hmd = 
    region_meta %>%
    filter(region_code_iso3166_2 %in% config$skeleton$region) %>%
    pull(region_code_hmd) %>% na.omit()
  names(region_lookup_hmd) <- region_lookup_hmd
  # first year in harmonized data set
  skeleton_first_year = config$skeleton$year$min
  # last year in harmonized data set
  skeleton_final_year = config$skeleton$year$max
})

dat <- list()

# Download WPP life tables ----------------------------------------

# download World Population Prospects: Life Tables
dat$wpp_get <- GET(url = cnst$url_wpp_ex, progress())

# Download HMD life tables ----------------------------------------

# females
dat$hmd_lt_female <-
  map(cnst$region_lookup_hmd, ~{
    readHMDweb(
      CNTRY = .x, item = 'fltper_1x1',
      username = cnst$hmd_usr, password = cnst$hmd_pwd,
      fixup = TRUE
    )
  })

# males
dat$hmd_lt_male <-
  map(cnst$region_lookup_hmd, ~{
    readHMDweb(
      CNTRY = .x, item = 'mltper_1x1',
      username = cnst$hmd_usr, password = cnst$hmd_pwd,
      fixup = TRUE
    )
  })


# Subset WPP ------------------------------------------------------

dat$wpp_df <-
  dat$wpp_get %>%
  content(as = 'parsed', type = 'text/csv', encoding = 'UTF-8') %>%
  filter(
    # subset to regions of interest
    LocID %in% cnst$region_lookup_wpp,
    # subset to years of interest
    MidPeriod %in% seq(cnst$skeleton_first_year, cnst$skeleton_final_year, 1),
    Sex %in% c('Male','Female')
  )

# Subset HMD ------------------------------------------------------

dat$hmd_mltper_1x1 <-
  dat$hmd_lt_male %>%
  bind_rows(.id = 'region_code_hmd') %>%
  filter(Year %in% cnst$skeleton_first_year:cnst$skeleton_final_year)

dat$hmd_fltper_1x1 <-
  dat$hmd_lt_female %>%
  bind_rows(.id = 'region_code_hmd') %>%
  filter(Year %in% cnst$skeleton_first_year:cnst$skeleton_final_year)

# Export ----------------------------------------------------------

# Save to files

saveRDS(
  dat$wpp_df,
  file = glue('{cnst$path_wpp}/wpp_ex.rds')
)

saveRDS(
  dat$hmd_fltper_1x1,
  file = glue('{cnst$path_hmdhfd}/fltper_1x1.rds')
)

saveRDS(
  dat$hmd_mltper_1x1,
  file = glue('{cnst$path_hmdhfd}/mltper_1x1.rds')
)
