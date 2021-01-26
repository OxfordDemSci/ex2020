# Harmonize data on death counts

# Weekly death counts come in widely different age groupings.
# Age groupings vary by country, year, and even week.
# This code harmonizes the age groupings to single year age groups
# 0 to 100+ using the Penalized Composite Link Model
# (doi:10.1093/aje/kwv020) implemented in
# https://github.com/mpascariu/ungroup).
#
# The ungrouping is separately applied to each
# region x sex x year x week combination in the input data.
#
# The ungrouped weekly deaths are then aggregated into yearly death
# counts by age.

# Init ------------------------------------------------------------

library(here); library(glue)
library(yaml); library(readr)
library(dplyr); library(purrr); library(tidyr); library(ggplot2)
library(ungroup)
library(foreach); library(doParallel)

dat <- list()
fig <- list()

# Constants -------------------------------------------------------

wd <- here()

config <- read_yaml(glue('{wd}/cfg/config.yaml'))
region_meta <- read_csv(glue('{wd}/cfg/region_metadata.csv'))
source(glue('{wd}/cfg/fig_specs.R'))

# lookup tables to translate STMF codes to harmonized codes
cnst <- list()
cnst <- within(cnst, {
  # path to global objects
  global_path = glue('{wd}/src/00-global.R')
  # path to pclm output
  pclm_path = glue('{wd}/tmp/pclm_output')
  # path to logfile for pclm
  log_path = glue('{wd}/tmp/pclm_output/log.txt')
  # skeleton path
  skeleton_path = glue('{wd}/tmp/harmonized_subsets/skeleton.rds')
  # path to stmf data
  stmf_path = glue('{wd}/dat/stmf')
  stmf_files = list.files(stmf_path)
  # where to put the harmonized data
  out_path_harmonized = glue('{wd}/tmp/harmonized_subsets')
  
  sex_code_stmf =
    c(m = config$skeleton$sex$Male, f = config$skeleton$sex$Female)
  # pclm life-table closeout age
  pclm_highest_age = 110
  # number of cores to run pclm on
  n_cores = 8
})

# Functions -------------------------------------------------------

source(cnst$global_path)

# Register cluster ------------------------------------------------

# register cluster for parallel processing
cl <- makeCluster(cnst$n_cores, outfile = cnst$log_path)
registerDoParallel(cl)

# Load data -------------------------------------------------------

# stmf death counts
dat$stmf_raw <-
  map(cnst$stmf_files, ~{
    # extract stmf region code from filename
    region_code_stmf <-
      sub(pattern = 'stmf.+$', replacement = '', x = .x)
    read_csv(
      glue('{cnst$stmf_path}/{.x}'),
      col_names = c('PopCode', 'Area', 'Year', 'Week', 'Sex', 'Age',
                    'AgeInterval', 'Deaths', 'Type', 'Access'),
      col_types = 'ciiccccdcc',
      skip = 1,
      na = '.'
    )
  })

# skeleton
dat$skeleton <- readRDS(cnst$skeleton_path)

# Harmonize labels ------------------------------------------------

dat$stmf_cleaned <-
  dat$stmf_raw %>%
  bind_rows() %>%
  # harmonize variable names
  select(
    region_code_stmf = PopCode,
    iso_year = Year, iso_week = Week,
    sex = Sex, age_start = Age, age_width = AgeInterval,
    death = Deaths
  ) %>%
  filter(
    # ignore total sex category
    sex != 'b',
    # ignore total age category
    # ignore deaths with unknown age for now
    !(age_start %in% c('TOT', 'UNK'))
  ) %>%
  mutate(
    # harmonize sex to common format
    sex = as.character(factor(
      sex, levels = names(cnst$sex_code_stmf),
      labels = cnst$sex_code_stmf)
    ),
    # add iso region codes
    region_iso = as.character(factor(
      region_code_stmf,
      levels = region_meta$region_code_stmf,
      labels = region_meta$region_code_iso3166_2
    )),
    # convert week variable to numeric
    # deaths with unknown week are coded as 0
    iso_week = as.numeric(ifelse(iso_week == 'UNK', '0', iso_week)),
    # convert age variables to numeric
    age_start = as.numeric(age_start),
    # if open age group, code width as Inf
    age_width = as.numeric(ifelse(age_width == '+', 'Inf', age_width))
  ) %>%
  select(-region_code_stmf)

# Harmonize age groupings -----------------------------------------

# This may take hours to run...

# prepare data for harmonization of age groups
# executed in parallel over regions
stmf_isplit_by_region <-
  isplit(dat$stmf_cleaned, f = dat$stmf_cleaned$region_iso)

dat$stmf_single_ages <-
  foreach(
    single_region = stmf_isplit_by_region,  
    .packages = c('dplyr', 'tidyr', 'ungroup'),
    .export = c('cnst')
  ) %dopar% {suppressPackageStartupMessages({
    
    single_region_ungrouped_deaths <-
      single_region[['value']] %>%
      group_by(region_iso, sex, iso_year, iso_week) %>%
      group_modify(~{
        cat('PCLM on', .y$region_iso, .y$sex, .y$iso_year, .y$iso_week, '\n')
        
        # number of age groups
        n_agegroups_raw <- length(.x$death)
        # any NA's in age-specific death counts or age strata?
        anyna = any(is.na(.x$death) | is.na(.x$age_start))
        # width of the last age group
        nlast <- cnst$pclm_highest_age-tail(.x$age_start, 1)+1
        
        if (!anyna) {
          
          fit_pclm <- pclm(
            x = .x$age_start, y = .x$death, nlast = nlast, out.step = 1#,
            #control = list(lambda = 0.1)
          )
          single_age_death <- round(fitted.values(fit_pclm), 1)
          pclm_lambda <- fit_pclm$smoothPar[1]
          
        } else {
          single_age_death <- NA
          pclm_lambda <- NA
        }
        
        ungrouped_deaths <- tibble(
          age = 0:cnst$pclm_highest_age,
          death = single_age_death,
          lambda = pclm_lambda,
          n_agegroups_raw = n_agegroups_raw
        )
        
        return(ungrouped_deaths)
      })
    
    return(single_region_ungrouped_deaths)
    
  })}

stopCluster(cl)

dat$stmf_single_ages <- bind_rows(dat$stmf_single_ages)

# Aggregate weeks into years --------------------------------------

dat$stmf_single_ages_years <-
  dat$stmf_single_ages %>%
  group_by(region_iso, sex, iso_year, age) %>%
  summarise(
    # within a region, sex, year, and age
    # data quality indicators
    # proper number of weeks in that year
    true_max_week = ifelse(YearHasIsoWeek53(iso_year[1]), 53, 52),
    # number of weeks with observed death counts
    n_observed_weeks = sum(!is.na(death)),
    # number of missing weeks
    # value of "-1" means that 53 weeks are given in the data
    # although the year does not feature a leap week, e.g.
    # strange week coding in original data
    n_missing_weeks =
      true_max_week - n_observed_weeks,
    # number of original age groups before ungrouping
    n_agegroups_raw =
      min(n_agegroups_raw),
    # aggregate deaths over weeks into years
    death = sum(death, na.rm = TRUE)
  ) %>%
  ungroup()

# Summarize all ages past 100 in single age group -----------------

dat$stmf_ready_for_join <-
  dat$stmf_single_ages_years %>%
  mutate(age = ifelse(age >= 100, 100, age)) %>%
  group_by(region_iso, sex, iso_year, age) %>%
  summarise(
    death = sum(death),
    n_missing_weeks = n_missing_weeks[1],
    n_agegroups_raw = n_agegroups_raw[1]
  ) %>%
  ungroup() %>%
  # add id
  mutate(
    id = GenerateRowID(region_iso, sex, age, iso_year)
  ) %>%
  select(id, death, n_missing_weeks, n_agegroups_raw)

# Join ------------------------------------------------------------

# join death counts with data base skeleton
dat$death_pre <-
  dat$skeleton %>%
  left_join(
    dat$stmf_ready_for_join,
    by = 'id'
  ) %>%
  # reconstruct the data quality indicators
  mutate(
    true_max_week = ifelse(YearHasIsoWeek53(year), 53, 52),
    n_missing_weeks =
      ifelse(
        # if <n_missing_weeks> is NA than this year only appears
        # in the skeleton file and not in the data, so the missing
        # weeks will be the total number of weeks in that year
        is.na(n_missing_weeks), true_max_week, n_missing_weeks)
  )

# Diagnostic plot -------------------------------------------------

# diagnostic plots for week-by-week ungrouping
walk(unique(dat$stmf_single_ages$region_iso), ~{
  the_plot <-
    dat$stmf_single_ages %>%
    filter(region_iso == .x) %>%
    ggplot(aes(x = age, y = death, color = sex)) +
    geom_line() +
    facet_grid(iso_year ~ iso_week) +
    theme_minimal() +
    scale_x_continuous(breaks = NULL) +
    scale_color_manual(values = fig_spec$sex_colors) +
    labs(subtitle = .x) +
    fig_spec$MyGGplotTheme()
  fig_spec$ExportFigure(
    the_plot,
    path = glue('{cnst$pclm_path}'),
    filename = glue('{.x}_pclm'),
    scale = 2
  )
})

# diagnostic plots for year to year age distribution of deaths
dat$deathplot <-
  dat$death_pre %>%
  filter(sex == 'Male') %>%
  mutate(is2020 = ifelse(year == 2020, TRUE, FALSE))

dat$deathplot_without_last_age <-
  dat$deathplot %>%
  filter(age_start < 100)

dat$deathplot_last_age <-
  dat$deathplot %>%
  filter(age_start == 100)

dat$deathplot_dataquality_2020 <-
  dat$deathplot %>%
  filter(year == 2020) %>%
  group_by(region_iso) %>%
  summarise(
    n_missing_weeks = max(n_missing_weeks),
    n_agegroups_raw = max(n_agegroups_raw)
  )

plot_harmonized_deaths <-
  dat$deathplot_without_last_age %>%
  ggplot() +
  geom_line(
    aes(
      x = age_start, y = death, color = is2020,
      group = interaction(year), size = is2020
    )
  ) +
  geom_label(
    aes(x = 0, y = 0, label = paste0(n_missing_weeks, '-', n_agegroups_raw)),
    hjust = 0, data = dat$deathplot_dataquality_2020, label.size = 0, alpha = 0.8
  ) +
  scale_size_manual(values = c(0.1, 1)) +
  scale_color_manual(values = c(`FALSE` = 'grey', `TRUE` = 'red')) +
  facet_wrap(~region_iso, scales = 'free_y') +
  guides(color = 'none', size = 'none') +
  fig_spec$MyGGplotTheme() +
  labs(
    title = 'Ungrouped male death counts by age and country',
    subtitle = 'Year 2020 is red, prior years grey. Numeric code <number of weeks missing from 2020 reported deaths>-<minimum number of raw age groups for 2020 ungrouping>',
    x = '',
    y = ''
  )
fig_spec$ExportFigure(
  plot_harmonized_deaths,
  path = glue('{cnst$pclm_path}'),
  filename = glue('plot_harmonized_deaths'),
  scale = 2
)

death <-
  dat$death_pre %>%
  select(id, death, n_missing_weeks, n_agegroups_raw)

# Export ----------------------------------------------------------

saveRDS(death, file = glue('{cnst$out_path_harmonized}/death.rds'))
