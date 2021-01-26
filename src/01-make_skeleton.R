# Create data base skeleton

# Init ------------------------------------------------------------

library(here); library(glue)
library(yaml)
library(dplyr); library(tidyr)

# Constants -------------------------------------------------------

wd <- here()

config <- read_yaml(glue('{wd}/cfg/config.yaml'))

cnst <- list()
cnst <- within(cnst, {
  out_path_harmonized = glue('{wd}/tmp/harmonized_subsets')
  global_path = glue('{wd}/src/00-global.R')
})

# Functions -------------------------------------------------------

source(cnst$global_path)

# Generate skeleton -----------------------------------------------

skeleton <-
  expand_grid(
    region_iso = config$skeleton$region,
    sex = unlist(config$skeleton$sex),
    year = as.integer(seq(config$skeleton$year$min, config$skeleton$year$max, 1)),
    tibble(
      age_start = seq(config$skeleton$age$min, config$skeleton$age$max, 1),
      age_width = c(diff(age_start), Inf)
    )
  )

# Add unique row id -----------------------------------------------

skeleton <-
  skeleton %>%
  mutate(
    id = GenerateRowID(region_iso, sex, age_start, year)
  )

# Define order of rows and columns --------------------------------

col_order <- quos(id, region_iso, sex, year, age_start, age_width)
skeleton <-
  skeleton %>%
  arrange(id) %>%
  select(!!!col_order)

# Export ---------------------------------------------------------

saveRDS(skeleton, file = glue('{cnst$out_path_harmonized}/skeleton.rds'))
