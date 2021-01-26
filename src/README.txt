# Harmonized mid-year population and annual death counts years 2000-2020 for STMF countries by single ages 0:100 and sex.

- `id`: unique row identifier
- `region_iso`: iso3166-2 region codes
- `sex`: Male, Female
- `year`: iso year
- `age_start`: start of age group
- `age_width`: width of age group, Inf for age_start 100, otherwise 1
- `year_has_53_weeks`: does the iso-year officialy feature 53 weeks?
- `death`: number of deaths
- `n_missing_weeks`: number of missing weeks in the original death data for this region-sex-year-age stratum
- `n_agegroups_raw`: minimum number of age groups in raw death data prior to ungrouping for this region-sex-year stratum
- `population_midyear`: midyear population (July 1st)

## Deaths

- source: STMF input data series
- harmonized to single ages via pclm
  - pclm iterates over country, sex, year, week and converts irregular age groupings, which may vary by country, year and week into a regular age grouping of 0:110
  - smoothing parameters estimated via BIC grid search seperately for every pclm iteration
  - last age group set to [110,111)
  - ages 100:110 are then summed into 100+ to be consistent with mid-year population information
- weekly ungrouped deaths are summed into yearly deaths for a particular country-sex stratum
- deaths in unknown weeks are considered; deaths in unknown ages are not considered

## Population

- source:
  - for years 2000 to 2019: World Population Prospects 2019 single year-age population estimates 1950-2019
  - for year 2020: World Population Prospects 2019 single year-age population projections 2020-2100
- mid-year population