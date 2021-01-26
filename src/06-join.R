# Assemble final data set

# Init ------------------------------------------------------------

library(here); library(glue)
library(dplyr); library(readr); library(openxlsx)


# Constants -------------------------------------------------------

wd <- here()
source(glue('{wd}/src/00-global.R'))

dat <- list()

# Data ------------------------------------------------------------

dat$skeleton <- readRDS(glue('{wd}/tmp/harmonized_subsets/skeleton.rds'))
dat$population <- readRDS(glue('{wd}/tmp/harmonized_subsets/population.rds'))
dat$death <- readRDS(glue('{wd}/tmp/harmonized_subsets/death.rds'))

# Join ------------------------------------------------------------

dat$lt_input <-
  dat$skeleton %>% 
  mutate(year_has_53_weeks = YearHasIsoWeek53(year)) %>%
  arrange(region_iso, sex, year, age_start) %>%
  left_join(dat$death %>%
              select(id, death, n_missing_weeks, n_agegroups_raw),
            by = 'id') %>%
  left_join(dat$population %>% select(id, population_midyear), by = 'id')

# Export ----------------------------------------------------------

saveRDS(dat$lt_input, file = glue('{wd}/out/lt_input.rds'))

write_csv(dat$lt_input, file = glue('{wd}/out/lt_input.csv'))

write.xlsx(dat$lt_input, glue('{wd}/out/lt_input.xlsx'),
           keepNA = TRUE, na.string = '.',
           firstRow = TRUE, firstCol = TRUE)
