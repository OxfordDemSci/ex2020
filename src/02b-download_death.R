# Download data on death counts

# (1) Download weekly death count data for various countries from
#     STMF input file
# (2) Download annual death counts for E&W pre 2020 from ONS

# Init ------------------------------------------------------------

library(here); library(glue)
library(httr); library(readr); library(dplyr); library(purrr)
library(readxl)

# Constants -------------------------------------------------------

wd <- here()

cnst <- list()
cnst <- within(cnst, {
  # STMF input data files
  url_stmf = 'https://www.mortality.org/Public/STMF/Inputs/STMFinput.zip'
  # ONS annual death counts pre 2020 for England and Wales
  url_ons = 'https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/deathsregisteredinenglandandwalesseriesdrreferencetables/2019/finalreftables2019.xlsx'
  path_tmp = glue('{wd}/tmp')
  path_stmf = glue('{wd}/dat/stmf')
  path_ons = glue('{wd}/dat/ons')
})

dat <- list()


# Download STMF data ----------------------------------------------

# download international weekly death counts by age and sex from STMF
dat$stmf_zip <-
  GET(url = cnst$url_stmf, progress())

# Download ONS data -----------------------------------------------

# download GB-EAW annual death count by age and sex counts from ONS
dat$ons <-
  GET(url = cnst$url_ons, progress())

# Preliminary format STMF data ------------------------------------

# save downloaded zip to file
writeBin(
  object = content(dat$stmf_zip, 'raw'),
  con = glue('{cnst$path_tmp}/stmf.zip')
)

# list all files in archive
dat$stmf_filenames <- unzip(
  glue('{cnst$path_tmp}/stmf.zip'),
  list = TRUE
)[['Name']]

# bind all .csv files in stmf zip archive into single file
dat$stmf <-
  dat$stmf_filenames %>%
  map(~{
    unz(glue('{cnst$path_tmp}/stmf.zip'), filename = .x) %>%
      read_csv(
        col_names = c('PopCode', 'Area', 'Year', 'Week', 'Sex', 'Age',
                      'AgeInterval', 'Deaths', 'Type', 'Access'),
        col_types = 'ciiccccdcc',
        skip = 1,
        na = '.'
      )
  }) %>%
  bind_rows()

# Preliminary format ONS data -------------------------------------

# save downloaded excel table to file
writeBin(
  object = content(dat$ons, 'raw'),
  con = glue('{cnst$path_tmp}/ons_annual_deaths.xlsx')
)

# subset to data of interest and bind the tables
# for males and females together
dat$ons_annual_deaths <-
  bind_rows(
    male = read_xlsx(
      path = glue('{cnst$path_tmp}/ons_annual_deaths.xlsx'),
      sheet = 8, range = 'A9:K115'
    ),
    female = read_xlsx(
      path = glue('{cnst$path_tmp}/ons_annual_deaths.xlsx'),
      sheet = 9, range = 'A9:K115'
    ),
    .id = 'sex'
  )

# Export ----------------------------------------------------------

saveRDS(dat$stmf, file = glue('{cnst$path_stmf}/stmf.rds'))
saveRDS(dat$ons_annual_deaths, file = glue('{cnst$path_ons}/ons_annual_deaths.rds'))
