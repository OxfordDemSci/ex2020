# Create data base skeleton

# Here we define the "skeleton" of the data base used
# for analysis. It's a definition of years, ages, sexes, and
# regions that we wish to acquire data for.

# Init ------------------------------------------------------------

library(here); library(glue)
library(yaml)
library(dplyr); library(tidyr)

# Constants -------------------------------------------------------

wd <- here()

config <- read_yaml(glue('{wd}/cfg/config.yaml'))

cnst <- list()
cnst <- within(cnst, {
  path_tmp = glue('{wd}/tmp')
  path_global = glue('{wd}/src/00-global.R')
})

dat <- list()

# Functions -------------------------------------------------------

source(cnst$path_global)

# Generate skeleton -----------------------------------------------

dat$skeleton <-
  expand_grid(
    region_iso =
      config$skeleton$region,
    sex =
      unlist(config$skeleton$sex),
    year =
      seq(config$skeleton$year$min, config$skeleton$year$max, 1) %>%
      as.integer(),
    tibble(
      age_start = seq(config$skeleton$age$min, config$skeleton$age$max, 1),
      age_width = c(diff(age_start), Inf)
    )
  )

# Add unique row id -----------------------------------------------

dat$skeleton <-
  dat$skeleton %>%
  mutate(
    id = GenerateRowID(region_iso, sex, age_start, year)
  )

# Define order of rows and columns --------------------------------

col_order <- quos(id, region_iso, sex, year, age_start, age_width)
dat$skeleton <-
  dat$skeleton %>%
  arrange(id) %>%
  select(!!!col_order)

# Export ---------------------------------------------------------

saveRDS(dat$skeleton, file = glue('{cnst$path_tmp}/harmonized_skeleton.rds'))
