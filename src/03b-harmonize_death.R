# Harmonize data on death counts
#
# We harmonize death counts data from 3 sources:
# (1): the HMD-STMF input data base for weekly death counts by age
#      and sex for numerous countries. This is our main source. The data
#      is updated weekly and includes the most recent information on
#      death counts.
# (2): the UK's Office for National Statistics (ONS) annual death counts
#      by age and sex for England & Wales. We use this data for years
#      2015 to 2019 for England & Wales because the age grouping in the
#      STMF data for these years is too coarse.
# (3): the US CDC for annual death counts by age and sex for the United
#      States. We use this data for years 2015 to 2019 for the US,
#      lacking any STMF data for these years.
#
# The STMF data is harmonizes as follows:
#
# STMF weekly death counts come in widely different age groupings.
# Age groupings vary by country, year, and even week.
# This code harmonizes the age groupings to single year age groups
# 0 to 100+ using the Penalized Composite Link Model
# (doi:10.1093/aje/kwv020) implemented in
# https://github.com/mpascariu/ungroup).
#
# The ungrouping is separately applied to each
# region x sex x year x age pattern combination in the input data.
#
# The ungrouped STMF deaths are then aggregated into yearly death
# counts by age.

# Init ------------------------------------------------------------

library(here); library(glue)
library(yaml); library(readr)
library(dplyr); library(purrr); library(tidyr); library(stringr)
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
  # where to put the harmonized data
  path_harmonized = glue('{wd}/tmp/harmonized_death.rds')
  
  # lookup table for region codes
  # only countries defined in skeleton
  region_lookup = 
    region_meta %>%
    filter(region_code_iso3166_2 %in% config$skeleton$region) %>%
    select(region_code_iso3166_2, region_code_stmf) %>%
    drop_na()
  
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

# Load data -------------------------------------------------------

# skeleton
dat$skeleton <- readRDS(cnst$path_skeleton)
# stmf death counts
dat$stmf <- readRDS(cnst$path_stmf)
# ons death counts
dat$ons <- readRDS(cnst$path_ons)
# cdc death counts
dat$cdc <- read_tsv(cnst$path_cdc, n_max = 1010)

# 01 STMF harmonize labels ----------------------------------------

dat$stmf_harmonized_01 <-
  dat$stmf %>%
  # harmonize variable names
  select(
    region_code_stmf = PopCode,
    iso_year = Year, iso_week = Week,
    sex = Sex, age_start = Age, age_width = AgeInterval,
    death = Deaths
  ) %>%
  # subset to data of interest
  filter(
    # ignore total sex category
    sex != 'b',
    # ignore total age category
    # ignore deaths with unknown age for now
    !(age_start %in% c('TOT', 'UNK')),
    iso_year %in% seq(config$skeleton$year$min, config$skeleton$year$max, 1),
    region_code_stmf %in% cnst$region_lookup$region_code_stmf
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
    age_width = as.numeric(ifelse(age_width == '+', 'Inf', age_width)),
    # EXCEPTION for Portugal where the open age group is wrongly
    # coded in the input data
    age_width = as.numeric(ifelse(
      region_iso == 'PT' & iso_year < 2020 & age_start == 90,
      'Inf', age_width))
  ) %>%
  select(-region_code_stmf)

# 02 STMF ungroup ages --------------------------------------------

# (1) within each region-sex-year-week stratum, check what kind of
# age grouping pattern is used and give it a unique label
# (2) within each region-sex-year stratum, disregard the weeks and
# just iterate over the unique age grouping patterns, creating
# age-specific yearly death counts by unique age grouping pattern
# (3) ungroup deaths to single year age groups within each
# region-sex-year-agegroup_pattern stratum

# label unique age grouping patterns within a
# region-sex-year-week stratum
dat$stmf_harmonized_01_ready_for_ungroup <-
  dat$stmf_harmonized_01 %>%
  group_by(region_iso, sex, iso_year, iso_week) %>%
  mutate(
    agegroup_pattern = paste0(age_start, collapse = '-')
  ) %>%
  # now aggregate over weeks by age within each
  # region-sex-year-age_pattern stratum
  group_by(region_iso, sex, iso_year, agegroup_pattern,
           age_start, age_width) %>%
  summarise(
    death = sum(death, na.rm = TRUE)
  ) %>%
  ungroup()

# ungroup to single year of age
# this may take some time to run...
dat$stmf_harmonized_02 <-
  dat$stmf_harmonized_01_ready_for_ungroup %>%
  group_by(region_iso, sex, iso_year, agegroup_pattern) %>%
  group_modify(~{
    cat('Try PCLM on', .y$region_iso, .y$sex, .y$iso_year, .y$agegroup_pattern, Sys.time(), '\n')
    # ungroup deaths via PCLM in specific country, sex, year and
    # age grouping pattern; catch any errors
    ungrouped_deaths <- tryCatch({
      
      # number of age groups
      n_agegroups_raw <- length(.x$death)
      # any explicit NA's in age-specific death counts or age strata?
      anyna1 = any(is.na(.x$death) | is.na(.x$age_start))
      # any implicit NA's in age-specific death counts?
      anyna2 = any(head(.x$age_start + .x$age_width, -1) != .x$age_start[-1])
      # width of the last age group
      nlast <- cnst$pclm_highest_age-tail(.x$age_start, 1)+1
      
      # if there are any NA's in the input series of death counts
      # return NA results with a corresponding error message...
      if (anyna1 | anyna2) {
        cat('Error: NA in', .y$region_iso, .y$sex, .y$iso_year, .y$agegroup_pattern, '\n')
        # NA return
        tibble(
          age = 0:cnst$pclm_highest_age,
          death = as.numeric(NA),
          lambda = as.numeric(NA),
          error = TRUE,
          message = 'NAs in age-specific death counts'
        )
        # ... otherwise apply pclm ungrouping
      } else {
        fit_pclm <- pclm(
          x = .x$age_start, y = .x$death, nlast = nlast, out.step = 1
        )
        # PCLM return
        tibble(
          age = 0:cnst$pclm_highest_age,
          death = round(fitted.values(fit_pclm), 1),
          lambda = fit_pclm$smoothPar[1],
          error = FALSE,
          message = as.character(NA)
        )
      }
      
    },
    
    # any unexpected errors are trapped here
    error = function(e) {
      cat('Error: Exception in ', .y$region_iso, .y$sex, .y$iso_year, .y$agegroup_pattern, geterrmessage(), '\n')
      # error return
      tibble(
        age = 0:cnst$pclm_highest_age,
        death = as.numeric(NA),
        lambda = as.numeric(NA),
        error = TRUE,
        message = geterrmessage()
      )
    })# End of tryCatch()
    
    return(ungrouped_deaths)
    
  }) %>% # End of group_modify() looping over sex, age, year, grouping pattern
  ungroup()

# 03 STMF aggregate into years ------------------------------------

# we keep track of missing weeks in a later stage of the analysis.
# therefore we can safely sum death counts over weeks using
# sum(na.rm=TRUE) without needing to worry about keeping track of
# missing deaths.

# aggregate over unique age groupings within a year into years
dat$stmf_harmonized_03 <-
  dat$stmf_harmonized_02 %>%
  group_by(region_iso, sex, iso_year, age) %>%
  summarise(
    # aggregate deaths over weeks into years
    death = sum(death, na.rm = TRUE)
  ) %>%
  ungroup()

# 04 STMF harmonize open age group --------------------------------

dat$stmf_harmonized_04 <-
  dat$stmf_harmonized_03 %>%
  mutate(age = ifelse(age >= 100, 100, age)) %>%
  group_by(region_iso, sex, iso_year, age) %>%
  summarise(
    death = sum(death)
  ) %>%
  ungroup() %>%
  # add id
  mutate(
    id = GenerateRowID(region_iso, sex, age, iso_year)
  )

# 05 STMF join and add data quality -------------------------------

# we need to track the data quality within each region-sex-year
# stratum, especially the number of weeks with explicitly or
# implicitly missing data, even if it's just within a single age
# group. In other words: when we sum deaths over the weeks within a year,
# we need to know what we're missing from that sum.
dat$stmf_data_quality <-
  dat$stmf_harmonized_01 %>%
  arrange(region_iso, sex, iso_year, iso_week, age_start) %>%
  # for each week (w) within a region-sex-year (rsy) stratum determine
  # - occurrence of any missing values in death count column, i.e.
  # missing deaths within any age groups
  # - number of age groups
  # - age of open age group
  group_by(region_iso, sex, iso_year, iso_week) %>%
  summarise(
    # explicit missing deaths in any age
    any_explicit_missing_deaths_rsyw =
      any(is.na(death)),
    # implicitly missing age groups whenever age_start + age_width
    # don't align with the following age_start
    any_implicit_missing_deaths_rsyw =
      any(head(age_start + age_width, -1) != age_start[-1]),
    any_missing_deaths_rsyw =
      any_explicit_missing_deaths_rsyw | any_implicit_missing_deaths_rsyw,
    # number of age groups
    nageraw_rsyw = n() %>% as.numeric(),
    openageraw = age_start[is.infinite(age_width)]
  ) %>%
  # for each year within a region-sex stratum determine
  # - number of weeks with complete death counts
  # - number of weeks with missing death counts
  # - minimum number of age groups
  # - q90 number of age groups
  # - minimum age for open age group
  group_by(region_iso, sex, iso_year) %>%
  summarise(
    nweeks_year = ifelse(YearHasIsoWeek53(iso_year)[1], 53, 52),
    nweeks_complete_deaths = sum(!any_missing_deaths_rsyw),
    nweeks_missing_deaths = nweeks_year - nweeks_complete_deaths,
    minnageraw = min(nageraw_rsyw),
    q90nageraw = quantile(nageraw_rsyw, probs = 0.9),
    minopenageraw = min(openageraw)
  ) %>%
  ungroup() %>%
  select(
    region_iso, sex, year = iso_year,
    death_total_nweeksmiss = nweeks_missing_deaths,
    death_total_minnageraw = minnageraw,
    death_total_q90nageraw = q90nageraw,
    death_total_minopenageraw = minopenageraw
  )

# join death counts with data base skeleton
dat$stmf_harmonized_05 <-
  dat$skeleton %>%
  # add info on weeks with missing data to skeleton
  left_join(
    dat$stmf_data_quality,
    by = c('region_iso', 'sex', 'year')
  ) %>%
  # if <death_total_nweeksmiss> is NA than this year only appears
  # in the skeleton file and not in the data, so the missing
  # weeks will be the total number of weeks in that year
  mutate(
    nweeks_year = ifelse(YearHasIsoWeek53(year), 53, 52),
    death_total_nweeksmiss =
      ifelse(
        is.na(death_total_nweeksmiss),
        nweeks_year, death_total_nweeksmiss
      )
  ) %>%
  # add ungrouped death counts
  left_join(
    select(dat$stmf_harmonized_04, id, death_total = death),
    by = 'id'
  )

dat$stmf_ready_for_join <-
  dat$stmf_harmonized_05 %>%
  select(
    id,
    death_total_stmf = death_total,
    death_total_nweeksmiss_stmf = death_total_nweeksmiss,
    death_total_minnageraw_stmf = death_total_minnageraw,
    death_total_q90nageraw_stmf = death_total_q90nageraw,
    death_total_minopenageraw_stmf = death_total_minopenageraw
  )

# STMF diagnostic plots -------------------------------------------

# diagnostic plots for week-by-week ungrouping
walk(unique(dat$stmf_harmonized_02$region_iso), ~{
  fig[[glue('death_pclm_{.x}')]] <<-
    dat$stmf_harmonized_02 %>%
    filter(region_iso == .x) %>%
    ggplot(aes(x = age, y = death, color = sex)) +
    geom_line() +
    facet_grid(iso_year ~ agegroup_pattern) +
    theme_minimal() +
    scale_x_continuous() +
    scale_color_manual(values = fig_spec$sex_colors) +
    labs(subtitle = glue('PCLM ungroup of death counts in {.x} (STMF)')) +
    fig_spec$MyGGplotTheme()
})

# diagnostic plots for year to year age distribution of deaths
dat$deathplot <-
  dat$stmf_harmonized_05 %>%
  filter(sex == 'Male', age_start < 100) %>%
  mutate(is2020 = ifelse(year == 2020, TRUE, FALSE))

fig$death_pclm <-
  dat$deathplot %>%
  ggplot() +
  geom_line(
    aes(
      x = age_start, y = death_total, color = is2020,
      group = interaction(year), size = is2020
    )
  ) +
  scale_size_manual(values = c(0.1, 1)) +
  scale_color_manual(values = c(`FALSE` = 'grey', `TRUE` = 'red')) +
  facet_wrap(~region_iso, scales = 'free_y') +
  guides(color = 'none', size = 'none') +
  fig_spec$MyGGplotTheme() +
  labs(
    title = 'Ungrouped male death counts by age and country (STMF)',
    subtitle = 'Year 2020 is red, prior years grey',
    x = '',
    y = ''
  )

fig$death_quality_q90nageraw <-
  dat$stmf_data_quality %>%
  ggplot(aes(x = year, y = death_total_q90nageraw, fill = sex, group = sex)) +
  geom_col(position = 'dodge') +
  facet_wrap(~region_iso) +
  scale_fill_manual(values = fig_spec$sex_colors) +
  fig_spec$MyGGplotTheme(panel_border = TRUE, grid = 'xy') +
  labs(
    title = '90% of the weeks in a year feature at least this many age groups (STMF)',
    x = 'Year', y = 'Count'
  )

fig$death_quality_minnageraw <-
  dat$stmf_data_quality %>%
  ggplot(aes(x = year, y = death_total_minnageraw, fill = sex, group = sex)) +
  geom_col(position = 'dodge') +
  facet_wrap(~region_iso) +
  scale_fill_manual(values = fig_spec$sex_colors) +
  fig_spec$MyGGplotTheme(panel_border = TRUE, grid = 'xy') +
  labs(
    title = 'The weeks in a year feature at least this many age groups (STMF)',
    x = 'Year', y = 'Count'
  )

fig$death_quality_minopenageraw <-
  dat$stmf_data_quality %>%
  ggplot(aes(x = year, y = death_total_minopenageraw, fill = sex, group = sex)) +
  geom_col(position = 'dodge') +
  facet_wrap(~region_iso) +
  scale_fill_manual(values = fig_spec$sex_colors) +
  fig_spec$MyGGplotTheme(panel_border = TRUE, grid = 'xy') +
  labs(
    title = 'The open age group in a year is at least this high (STMF)',
    x = 'Year', y = 'Count'
  )

fig$death_quality_nweeksmiss <-
  dat$stmf_data_quality %>%
  ggplot(
    aes(x = year, y = death_total_nweeksmiss,
        fill = sex, group = sex)
  ) +
  geom_col(position = 'dodge') +
  facet_wrap(~region_iso) +
  scale_x_continuous(breaks = 2015:2020) +
  scale_y_continuous(limits = c(0, 53)) +
  scale_fill_manual(values = fig_spec$sex_colors) +
  fig_spec$MyGGplotTheme(panel_border = TRUE, grid = 'xy') +
  labs(
    title = 'Number of weeks with at least one missing age-specific death count (STMF)',
    subtitle = 'Implicitly missing weeks and ages are counted as well',
    x = 'Year', y = 'Count'
  )

# ONS harmonize data ----------------------------------------------

dat$ons_ready_for_join <-
  dat$ons %>%
  pivot_longer(
    cols = c(-sex, -Age),
    names_to = 'year',
    values_to = 'death_total'
  ) %>%
  mutate(
    year = as.numeric(year),
    sex = as.character(factor(
      sex, levels = names(cnst$code_sex_ons),
      labels = cnst$code_sex_ons)
    )
  ) %>%
  # harmonize age, i.e. make 100+ the open age group
  mutate(
    age_start = ifelse(Age == '105+', '105', Age) %>% as.numeric(),
    age_start = ifelse(age_start >= 100, 100, age_start)
  ) %>%
  group_by(year,sex, age_start) %>%
  summarise(death_total = sum(death_total)) %>%
  ungroup() %>%
  # add data quality indicators
  mutate(
    death_total_nweeksmiss = 0,
    death_total_minnageraw = 106,
    death_total_q90nageraw = 106,
    death_total_minopenageraw = 105
  ) %>%
  # add id
  mutate(
    id = GenerateRowID(region_iso = 'GB-EAW', sex, age_start, year)
  ) %>%
  select(
    id, death_total_ons = death_total,
    death_total_nweeksmiss_ons = death_total_nweeksmiss,
    death_total_minnageraw_ons = death_total_minnageraw,
    death_total_q90nageraw_ons = death_total_q90nageraw,
    death_total_minopenageraw_ons = death_total_minopenageraw
  )

# CDC harmonize data ----------------------------------------------

# The CDC data comes in single ages 0:100 for the US. For 2020 we only
# have the STMF data in a much coarser age grouping. In order to
# calculate life-tables in a manner consistent with 2020, we summarise
# the pre 2020 US death counts into the 2020 age grouping and then apply
# the pclm ungrouping to single ages, mirroring the approach to the 2020
# data.

age_start_2020 = c(0, 1, 5, 15, 25, 35, 45, 55, 65, 75, 85)
dat$cdc_ready_for_join <-
  dat$cdc %>%
  select(
    year = Year, sex = `Gender Code`,
    age_start = `Single-Year Ages`, death_total = Deaths
  ) %>%
  mutate(
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
  # aggregate death counts into 2020
  # grouping and ungroup to single ages
  group_by(year, sex) %>%
  group_modify(~{
    
    # group to 2020 scheme
    deaths_aggregated_2020 <- aggregate(
      x = .x$death_total,
      by = list(cut(.x$age_start, c(age_start_2020,Inf), right = FALSE)),
      FUN = sum
    )[['x']]
    
    # number of age groups
    n_agegroups_raw <- length(deaths_aggregated_2020)
    # width of the last age group
    nlast <- cnst$pclm_highest_age-tail(age_start_2020, 1)+1
    
    # pclm
    fit_pclm <- pclm(
      x = age_start_2020, y = deaths_aggregated_2020,
      nlast = nlast, out.step = 1
    )
    # PCLM return
    tibble(
      age_start = 0:cnst$pclm_highest_age,
      death_total = round(fitted.values(fit_pclm), 1),
      lambda = fit_pclm$smoothPar[1]
    )
    
  }) %>%
  ungroup() %>%
  # add data quality indicators
  mutate(
    death_total_nweeksmiss = 0,
    death_total_minnageraw = as.numeric(length(age_start_2020)),
    death_total_q90nageraw = as.numeric(length(age_start_2020)),
    death_total_minopenageraw = tail(age_start_2020, 1)
  ) %>%
  # add id
  mutate(
    id = GenerateRowID(region_iso = 'US', sex, age_start, year)
  ) %>%
  select(
    id, death_total_cdc = death_total,
    death_total_nweeksmiss_cdc = death_total_nweeksmiss,
    death_total_minnageraw_cdc = death_total_minnageraw,
    death_total_q90nageraw_cdc = death_total_q90nageraw,
    death_total_minopenageraw_cdc = death_total_minopenageraw
  )

# Final join ------------------------------------------------------

dat$death <-
  dat$skeleton %>%
  left_join(
    dat$stmf_ready_for_join
  ) %>%
  left_join(
    dat$ons_ready_for_join
  ) %>%
  left_join(
    dat$cdc_ready_for_join
  ) %>%
  # for GB-EAW in the years prior to 2020, choose the ONS data,
  # else choose the STMF data
  mutate(
    death_total_source = case_when(
      region_iso == 'GB-EAW' & year < 2020 ~ 'ons',
      region_iso == 'US' & year < 2020 ~ 'cdc',
      TRUE ~ 'stmf'
    )
  ) %>%
  mutate(
    death_total = case_when(
      death_total_source == 'ons' ~ death_total_ons,
      death_total_source == 'stmf' ~ death_total_stmf,
      death_total_source == 'cdc' ~ death_total_cdc
    ),
    death_total_nweeksmiss = case_when(
      death_total_source == 'ons' ~ death_total_nweeksmiss_ons,
      death_total_source == 'stmf' ~ death_total_nweeksmiss_stmf,
      death_total_source == 'cdc' ~ death_total_nweeksmiss_cdc
    ),
    death_total_minnageraw = case_when(
      death_total_source == 'ons' ~ death_total_minnageraw_ons,
      death_total_source == 'stmf' ~ death_total_minnageraw_stmf,
      death_total_source == 'cdc' ~ death_total_minnageraw_cdc
    ),
    death_total_q90nageraw = case_when(
      death_total_source == 'ons' ~ death_total_q90nageraw_ons,
      death_total_source == 'stmf' ~ death_total_q90nageraw_stmf,
      death_total_source == 'cdc' ~ death_total_q90nageraw_cdc
    ),
    death_total_minopenageraw = case_when(
      death_total_source == 'ons' ~ death_total_minopenageraw_ons,
      death_total_source == 'stmf' ~ death_total_minopenageraw_stmf,
      death_total_source == 'cdc' ~ death_total_minopenageraw_cdc
    )
  ) %>%
  select(
    id, death_total,
    death_total_nweeksmiss, death_total_minnageraw,
    death_total_q90nageraw, death_total_minopenageraw,
    death_total_source
  )

# Export ----------------------------------------------------------

fig_spec$ExportFiguresFromList(
  lst = fig,
  path = glue('{cnst$path_pclm}'),
  scale = 2
)

saveRDS(dat$death, file = cnst$path_harmonized)
