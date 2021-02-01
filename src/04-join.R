# Assemble final data set

# Init ------------------------------------------------------------

library(here); library(glue)
library(dplyr); library(readr); library(openxlsx)

# Constants -------------------------------------------------------

wd <- here()
source(glue('{wd}/src/00-global.R'))

cnst <- list()
cnst <- within(cnst, {
  # skeleton path
  path_skeleton = glue('{wd}/tmp/harmonized_skeleton.rds')
  # population path
  path_population = glue('{wd}/tmp/harmonized_population.rds')
  # death path
  path_death = glue('{wd}/tmp/harmonized_death.rds')
  # covid path
  path_covid = glue('{wd}/tmp/harmonized_covid.rds')
  # out path of harmonized data
  path_out = glue('{wd}/out')
})

dat <- list()

# Data ------------------------------------------------------------

dat$skeleton <- readRDS(cnst$path_skeleton)
dat$population <- readRDS(cnst$path_population)
dat$death <- readRDS(cnst$path_death)
dat$covid <- readRDS(cnst$path_covid)

# Join ------------------------------------------------------------

dat$lt_input <-
  dat$skeleton %>% 
  mutate(nweeks_year = ifelse(YearHasIsoWeek53(year), 53L, 52L)) %>%
  arrange(region_iso, sex, year, age_start) %>%
  left_join(dat$death, by = 'id') %>%
  left_join(dat$population, by = 'id') %>%
  left_join(dat$covid, by = 'id')

# Export ----------------------------------------------------------

saveRDS(dat$lt_input, file = glue('{cnst$path_out}/lt_input.rds'))

write_csv(dat$lt_input, file = glue('{cnst$path_out}/lt_input.csv'))

write.xlsx(dat$lt_input, glue('{cnst$path_out}/lt_input.xlsx'),
           keepNA = TRUE, na.string = '.',
           firstRow = TRUE, firstCol = TRUE)
