# Download data on weekly death counts

# STMF input data

# Init ------------------------------------------------------------

library(here); library(glue)
library(httr); library(readr); library(dplyr); library(purrr)

# Constants -------------------------------------------------------

wd <- here()

cnst <- list()
cnst <- within(cnst, {
  url_stmf = 'https://www.mortality.org/Public/STMF/Inputs/STMFinput.zip'
  path_tmp = glue('{wd}/tmp')
  path_stmf = glue('{wd}/dat/stmf')
})

dat <- list()

# Download --------------------------------------------------------

# download international weekly death by age and sex counts from STMF
dat$stmf_zip <-
  GET(url = cnst$url_stmf, progress())

# Unzip and bind to single table ----------------------------------

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
  bind_rows() %>%
  mutate(source_death = 'stmf')

# Export ----------------------------------------------------------

saveRDS(dat$stmf, file = glue('{cnst$path_stmf}/stmf.rds'))
