# Harmonize data on death counts
#
# We harmonize death counts data from 3 sources:
#
# (1): The HMD-STMF input data base for weekly death counts by age
#      and sex for numerous countries. This is our main source. The data
#      is updated weekly and includes the most recent information on
#      death counts.
# (2): The UK's Office for National Statistics (ONS) annual death counts
#      by age and sex for England & Wales. We use this data for years
#      2015 to 2019 for England & Wales because the age grouping in the
#      STMF data for these years is too coarse.
# (3): The US CDC for annual death counts by age and sex for the United
#      States. We use this data for years 2015 to 2019 for the US,
#      lacking any STMF data for these years.
#
# In a first step we harmonize the labels of all three data sources, and
# select the preferred data source for each region and year. We then
# perform a model based ungrouping (doi:10.1093/aje/kwv020) of deaths
# into single ages for each region-year-sex stratum. During this
# ungrouping we also derive person-weeks of exposure from the mid-year
# population counts for each age within a stratum.
#
# Exposures are adjusted for the length of the observation period within
# a year, taking into weeks missing from the input data and leap-weeks.
#
# Because the age grouping scheme may vary within a year, the
# ungrouping is separately applied to each
# region x sex x year x age pattern combination in the input data.
# The ungrouped STMF deaths and exposures are then aggregated into
# annual death counts by age.

# Init ------------------------------------------------------------

library(here); library(glue)
library(yaml); library(readr)
library(dplyr); library(purrr); library(tidyr); library(stringr); library(ISOweek)
library(ggplot2)
library(ungroup)

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
  path_global = glue('{wd}/src/00-global.R')
  
  # path to pclm output
  path_pclm = glue('{wd}/tmp')
  # path to logfile for pclm
  path_pclm_log = glue('{wd}/tmp/pclm_log.txt')
  
  # skeleton path
  path_skeleton = glue('{wd}/tmp/harmonized_skeleton.rds')
  
  # path to stmf data
  path_stmf = glue('{wd}/dat/stmf/stmf.rds')
  # path to ons data
  path_ons = glue('{wd}/dat/ons/ons_annual_deaths.rds')
  # path to cdc data
  path_cdc = glue('{wd}/dat/cdc/us_annual_deaths.txt')
  
  # path to harmonized exposures
  path_midyearpop = glue('{wd}/tmp/harmonized_population.rds')
  
  # where to put the harmonized data
  path_harmonized = glue('{wd}/tmp/harmonized_death.rds')
  
  # out path for figures
  path_fig = glue('{wd}/out')
  
  # lookup table for region codes
  # only countries defined in skeleton
  region_lookup = 
    region_meta %>%
    filter(region_code_iso3166_2 %in% config$skeleton$region) %>%
    select(region_code_iso3166_2, region_code_stmf) %>%
    drop_na()
  
  stmf_regions_reporting_iso_week_year =
    region_meta %>%
    filter(calendar_stmf == 'iso_week_date') %>%
    pull(region_code_iso3166_2)
  
  # lookup table for sex codes by source
  code_sex_stmf =
    c(m = config$skeleton$sex$Male, f = config$skeleton$sex$Female)
  code_sex_ons =
    c(male = config$skeleton$sex$Male, female = config$skeleton$sex$Female)
  code_sex_cdc =
    c(M = config$skeleton$sex$Male, F = config$skeleton$sex$Female)
  # pclm life-table closeout age
  pclm_highest_age = 110
})

# Functions -------------------------------------------------------

source(cnst$path_global)

MidyearPop2Exposures <- function (midyearpop, nweeks_observed) {
  midyearpop*nweeks_observed/52
}

MakeAgeGroupPattern <- function (age_start) {
  paste0(age_start, collapse = '-')
}

ConstructReadyForUngroup <- function (
  df,
  region_iso, sex, year, agegroup_pattern, age_start,
  age_width, deaths, nweeksobserved, nweeksyear, source
) {
  require(dplyr)
  df %>%
    arrange(region_iso, sex, year, age_start) %>%
    mutate(source = source) %>%
    select(
      region_iso, sex, year, agegroup_pattern, age_start,
      deaths, nweeksobserved, nweeksyear, source
    ) %>%
    nest(
      'deaths_{source}' := c(
        agegroup_pattern, age_start, deaths,
        nweeksobserved, nweeksyear, source
      )
    )
}

# Load data -------------------------------------------------------

# skeleton
dat$skeleton <- readRDS(cnst$path_skeleton)

# midyear population
dat$midyearpop <- readRDS(cnst$path_midyearpop)

# stmf death counts
dat$stmf <- readRDS(cnst$path_stmf)
# ons death counts
dat$ons <- readRDS(cnst$path_ons)
# cdc death counts
dat$cdc <- read_tsv(cnst$path_cdc, n_max = 1010)

# STMF prepare for ungroup ----------------------------------------

dat$stmf_harmonized_labels <-
  dat$stmf %>%
  # harmonize variable names
  select(
    region_code_stmf = PopCode,
    year = Year, iso_week = Week,
    sex = Sex, age_start = Age, age_width = AgeInterval,
    deaths = Deaths
  ) %>%
  # subset to data of interest
  filter(
    # ignore total sex category
    sex != 'b',
    # ignore total age category
    # ignore deaths with unknown age for now
    !(age_start %in% c('TOT', 'UNK')),
    # subset to years of interest
    year %in% seq(config$skeleton$year$min, config$skeleton$year$max, 1)
  ) %>%
  mutate(
    # harmonize sex to common format
    sex = as.character(factor(
      sex, levels = names(cnst$code_sex_stmf),
      labels = cnst$code_sex_stmf)
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
  select(region_iso, sex, year, everything(), -region_code_stmf)

# (1) within each region-sex-year-week stratum, check what kind of
# age grouping pattern is used and give it a unique label
# (2) within each region-sex-year stratum, disregard the weeks and
# just iterate over the unique age grouping patterns, creating
# age-specific yearly death counts by unique age grouping pattern

# region_iso, sex, year: strata
# agegroup_pattern: the age grouping pattern, possibly multiple per stratum
# age_start, age_width
# deaths: summed over weeks of year into for weeks reporting same
#         agegroup_pattern
# nweeksobserved: number of completely observed weeks contained in the
#                 summed death counts
# nweeksyear: the number of weeks in the year, may vary according to the
#             calendar used by the data provider
# source: where does the data originate?
dat$stmf_ready_for_ungroup <-
  dat$stmf_harmonized_labels %>%
  arrange(region_iso, sex, year, iso_week, age_start) %>%
  group_by(region_iso, sex, year, iso_week) %>%
  # the STMF input files can be unreliable, therefore we check for
  # missing data within each week before we sum over weeks.
  # for each week within a region-sex-year stratum determine
  # - occurrence of any missing values in death count column, i.e.
  #   missing deaths within any age groups, or implicitly missing
  #   age groups
  mutate(
    # explicit missing deaths in any age
    any_explicit_missing_deaths =
      any(is.na(deaths)),
    # implicitly missing age groups whenever age_start + age_width
    # don't align with the following age_start
    any_implicit_missing_deaths =
      any(head(age_start + age_width, -1) != age_start[-1]),
    any_missing_deaths =
      any_explicit_missing_deaths | any_implicit_missing_deaths,
  ) %>%
  # drop a week from further consideration if it features missing deaths
  filter(!any_missing_deaths) %>%
  # this is the age grouping scheme used in a particular week
  # it may change between weeks
  mutate(agegroup_pattern = MakeAgeGroupPattern(age_start)) %>%
  # now sum deaths over weeks within each
  # region-sex-year-age_pattern stratum
  group_by(region_iso, sex, year, agegroup_pattern, age_start) %>%
  summarise(
    deaths = sum(deaths),
    # number of completely observed weeks being summed
    nweeksobserved = length(unique(iso_week[iso_week != 0]))
  ) %>%
  ungroup() %>%
  # - number of weeks in reporting year, i.e. if the deaths are reported
  #   over the Gregorian calendar, there would be approximately 52 weeks
  #   in every year, for an ISO-week-date calendar there would either be
  #   52 or 53 weeks in a year. 
  mutate(
    nweeksyear = case_when(
      # if STMF region reports over iso-week-date and year has leap-week
      # use 53 weeks as reporting length
      (region_iso %in% cnst$stmf_regions_reporting_iso_week_year) & 
        YearHasIsoWeek53(year) ~ 53,
      # for all else use 52 weeks
      TRUE ~ 52
    )
  ) %>%
  ConstructReadyForUngroup(
    region_iso, sex, year, agegroup_pattern, age_start,
    deaths, nweeksobserved, nweeksyear, source = 'stmf'
  )

# ONS prepare for ungroup -----------------------------------------

dat$ons_ready_for_ungroup <-
  dat$ons %>%
  pivot_longer(
    cols = c(-sex, -Age),
    names_to = 'year',
    values_to = 'deaths'
  ) %>%
  mutate(
    region_iso = 'GB-EAW',
    sex = as.character(factor(
      sex, levels = names(cnst$code_sex_ons),
      labels = cnst$code_sex_ons)
    ),
    year = as.numeric(year)
  ) %>%
  # harmonize age, i.e. make 100+ the open age group
  mutate(
    age_start = ifelse(Age == '105+', '105', Age) %>% as.numeric(),
    age_start = ifelse(age_start >= 100, 100, age_start)
  ) %>%
  group_by(region_iso, sex, year, age_start) %>%
  summarise(deaths = sum(deaths)) %>%
  ungroup() %>%
  # add data quality indicators
  mutate(
    agegroup_pattern = MakeAgeGroupPattern(age_start),
    # we know that the data is complete
    nweeksobserved = 52,
    # the data is reported over a Gregorian year
    nweeksyear = 52
  ) %>%
  ConstructReadyForUngroup(
    region_iso, sex, year, agegroup_pattern, age_start,
    deaths, nweeksobserved, nweeksyear, source = 'ons'
  )

# CDC prepare for ungroup -----------------------------------------

# The CDC data comes in single ages 0:100 for the US. For 2020 we only
# have the STMF data in a much coarser age grouping. In order to
# calculate life-tables in a manner consistent with 2020, we summarise
# the pre 2020 US death counts into the 2020 age grouping.

age_start_2020 = c(0, 1, 5, 15, 25, 35, 45, 55, 65, 75, 85, Inf)
dat$cdc_ready_for_ungroup <-
  dat$cdc %>%
  select(
    year = Year, sex = `Gender Code`,
    age_start = `Single-Year Ages`, deaths = Deaths
  ) %>%
  mutate(
    region_iso = 'US',
    sex = as.character(factor(
      sex, levels = names(cnst$code_sex_cdc),
      labels = cnst$code_sex_cdc)
    ),
    age_start = case_when(
      age_start == '< 1 year' ~ 0,
      age_start == '100+ years' ~ 100,
      TRUE ~ as.numeric(stringr::str_extract(age_start, '^[[:digit:]]+'))
    )
  ) %>%
  # for each year-sex, for consistency reasons,
  # aggregate death counts into 2020 grouping
  # add data quality indicators
  mutate(
    age_start =
      cut(age_start, breaks = age_start_2020,
          labels = head(age_start_2020, -1), right = FALSE) %>%
      as.character() %>% as.numeric()
  ) %>%
  group_by(region_iso, sex, year, age_start) %>%
  summarise(deaths = sum(deaths)) %>%
  group_by(region_iso, sex, year) %>%
  # add data quality indicators
  mutate(
    agegroup_pattern = MakeAgeGroupPattern(age_start),
    # we know that the data is complete
    nweeksobserved = 52,
    # the data is reported over a Gregorian year
    nweeksyear = 52
  ) %>%
  ConstructReadyForUngroup(
    region_iso, sex, year, agegroup_pattern, age_start,
    deaths, nweeksobserved, nweeksyear, source = 'cdc'
  )

# Choose which sources to use -------------------------------------

dat$all_ready_for_ungroup <-
  expand_grid(
    region_iso = config$skeleton$region,
    sex = unlist(config$skeleton$sex),
    year = config$skeleton$year$min:config$skeleton$year$max
  ) %>%
  left_join(dat$stmf_ready_for_ungroup) %>%
  left_join(dat$ons_ready_for_ungroup) %>%
  left_join(dat$cdc_ready_for_ungroup) %>%
  mutate(
    deaths =
      case_when(
        region_iso %in% 'GB-EAW' & year < 2020 ~ deaths_ons,
        region_iso %in% 'US' & year < 2020 ~ deaths_cdc,
        TRUE ~ deaths_stmf
      )
  ) %>%
  select(region_iso, sex, year, deaths) %>%
  unnest(deaths)

# PCLM age harmonization ------------------------------------------

# prepare midyear population for pclm exposures
dat$midyearpop_for_pclm <- left_join(dat$skeleton, dat$midyearpop)

dat$ungrouped_by_unique_age_pattern <-
  dat$all_ready_for_ungroup %>%
  group_by(region_iso, sex, year, agegroup_pattern) %>%
  group_modify(~{
    
    # number of age groups
    nageraw <- length(.x$age_start)
    # start of open age group
    openageraw <- tail(.x$age_start, 1)
    # width of the last age group
    nlast <- cnst$pclm_highest_age-openageraw+1
    
    # characteristics of this data series
    # number of observed weeks making up these counts
    nweeksobserved <- .x$nweeksobserved[1]
    # number of weeks that should be reported this year
    nweeksyear <- .x$nweeksyear[1]
    # data source
    source <- .x$source[1]

    # get the corresponding midyear population
    midyearpop <-
      dat$midyearpop_for_pclm %>%
      filter(region_iso == .y$region_iso, sex == .y$sex, year == .y$year) %>%
      pull(population_midyear)    

    # adjust midyearpop for the fraction of the observed year and
    # for eventual leap weeks
    population_py <- MidyearPop2Exposures(midyearpop, nweeksobserved)
        
    cat('Try PCLM on', .y$region_iso, .y$sex, .y$year, .y$agegroup_pattern, Sys.time(), '\n')
    # ungroup deaths via PCLM in specific country, sex, year and
    # age grouping pattern; catch any errors
    ungrouped_deaths <- tryCatch({
      
      # ungroup deaths with exposures, output is deathrates
      fit_pclm <-
        pclm(
          y = .x$deaths, x = .x$age_start,
          #offset = population_py,
          nlast = nlast, out.step = 1
        )
      ungrouped_deaths <- fitted.values(fit_pclm)[1:100]
      ungrouped_deaths[101] <- sum(fitted.values(fit_pclm)[101:111])
      
      # PCLM return
      ungrouped_deaths <- tibble(
        age_start = 0:100,
        deaths = round(ungrouped_deaths, 2),
        population_py = round(population_py, 2),
        lambda = fit_pclm$smoothPar[1],
        error = FALSE,
        message = as.character(NA),
        nweeksobserved,
        nweeksyear,
        nageraw,
        openageraw,
        source
      )
      
    },
    
    # any unexpected errors are trapped here
    error = function(e) {
      cat('Error: Exception in ', .y$region_iso, .y$sex, .y$year, .y$agegroup_pattern, geterrmessage(), '\n')
      # error return
      tibble(
        age_start = 0:100,
        deaths = as.numeric(NA),
        population_py = population_py,
        lambda = as.numeric(NA),
        error = TRUE,
        message = geterrmessage(),
        nweeksobserved = 0,
        nweeksyear = nweeksyear,
        nageraw,
        openageraw,
        source = source
      )
    })# End of tryCatch()
    
    return(ungrouped_deaths)
    
  }) %>% # End of group_modify() looping over sex, age, year, grouping pattern
  ungroup()

# sum deaths and exposures over eventual multiple
# age patterns observed within a year and derive some
# data quality metrics
dat$ungrouped <-
  dat$ungrouped_by_unique_age_pattern %>%
  group_by(region_iso, sex, year, age_start) %>%
  summarise(
    nweeksyear = nweeksyear[1],
    deaths = sum(deaths),
    population_py = sum(population_py),
    nweeksobserved = sum(nweeksobserved),
    nweeksmiss = nweeksyear - nweeksobserved,
    minnageraw = min(nageraw),
    maxnageraw = max(nageraw),
    minopenageraw = min(openageraw),
    maxopenageraw = max(openageraw),
    source = source[1]
  ) %>%
  mutate(
    id = GenerateRowID(region_iso, sex, age_start, year)
  ) %>%
  ungroup()

# prepare the data for export
dat$death <-
  dat$ungrouped %>%
  select(
    id, death_total = deaths,
    population_py,
    death_total_nweeksmiss = nweeksmiss,
    death_total_minnageraw = minnageraw,
    death_total_maxnageraw = maxnageraw,
    death_total_minopenageraw = minopenageraw,
    death_total_maxopenageraw = maxopenageraw,
    death_total_source = source
  )
dat$death <-
  dat$skeleton %>% select(id) %>%
  left_join(dat$death, by = 'id')

# Diagnostic plots ------------------------------------------------

dat$all_ready_for_ungroup

walk(cnst$region_lookup$region_code_iso3166_2, ~{
  grouped <-
    dat$all_ready_for_ungroup %>%
    filter(region_iso == .x) %>%
    arrange(region_iso, sex, year, agegroup_pattern, age_start) %>%
    mutate(
      age_width = c(diff(age_start), -1),
      age_width = ifelse(age_width < 0, cnst$pclm_highest_age-age_start, age_width),
      deaths = deaths/age_width
    )
  ungrouped <-
    dat$ungrouped_by_unique_age_pattern %>%
    filter(region_iso == .x)
  fig$pclm[[glue('death_pclm_{.x}')]] <<-
    ungrouped %>%
    ggplot(aes(x = age_start, y = deaths, color = sex)) +
    geom_rect(aes(
      xmin = age_start, xmax = age_start+age_width,
      ymin = 0, ymax = deaths,
      fill = sex
    ),
    alpha = 0.4, color = NA,
    data = grouped) +
    geom_line() +
    facet_grid(year ~ agegroup_pattern) +
    theme_minimal() +
    scale_color_manual(values = fig_spec$sex_colors) +
    scale_fill_manual(values = fig_spec$sex_colors) +
    labs(subtitle = glue('PCLM estimated singe age death counts in {.x}')) +
    fig_spec$MyGGplotTheme()
})

# diagnostic plots for year to year age distribution of deaths
dat$deathplot <-
  dat$ungrouped %>%
  filter(sex == 'Male') %>%
  mutate(is2020 = ifelse(year == 2020, TRUE, FALSE))

fig$death_pclm <-
  dat$deathplot %>%
  ggplot() +
  geom_line(
    aes(
      x = age_start, y = deaths, color = is2020,
      group = interaction(year), size = is2020
    )
  ) +
  scale_size_manual(values = c(0.1, 0.3)) +
  scale_color_manual(values = c(`FALSE` = 'grey', `TRUE` = 'red')) +
  facet_wrap(~region_iso, scales = 'free_y') +
  guides(color = 'none', size = 'none') +
  fig_spec$MyGGplotTheme() +
  labs(
    title = 'Ungrouped male death counts by age and country',
    subtitle = 'Year 2020 is red, prior years grey',
    x = '',
    y = ''
  )

fig$hazard_pclm <-
  dat$deathplot %>%
  ggplot() +
  geom_line(
    aes(
      x = age_start, y = deaths/population_py, color = is2020,
      group = interaction(year), size = is2020
    )
  ) +
  scale_size_manual(values = c(0.1, 0.3)) +
  scale_color_manual(values = c(`FALSE` = 'grey', `TRUE` = 'red')) +
  facet_wrap(~region_iso, scales = 'free_y') +
  guides(color = 'none', size = 'none') +
  fig_spec$MyGGplotTheme() +
  labs(
    title = 'Ungrouped male death rates by age and country',
    subtitle = 'Year 2020 is red, prior years grey',
    x = '',
    y = ''
  ) +
  scale_y_log10()

# Export ----------------------------------------------------------

saveRDS(dat$death, file = cnst$path_harmonized)

fig_spec$ExportFigure(
  fig$death_pclm, path = cnst$path_fig, scale = 1.5
)

fig_spec$ExportFigure(
  fig$hazard_pclm, path = cnst$path_fig, scale = 1.5
)

library(gridExtra)
ggsave(
  filename = glue('{cnst$path_fig}/pclm_death_ungroup_diagnostics.pdf'), 
  plot = marrangeGrob(fig$pclm, nrow=1, ncol=1), 
  width = 10, height = 8
)
