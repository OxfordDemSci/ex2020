# Download data on population estimates and projections

# Init ------------------------------------------------------------

library(here); library(glue)
library(httr)

# Constants -------------------------------------------------------

wd <- here()

cnst <- list()
cnst <- within(cnst, {
  wpp_estimates_url = 'https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2019_PopulationBySingleAgeSex_1950-2019.csv'
  wpp_projections_url = 'https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2019_PopulationBySingleAgeSex_2020-2100.csv'
  out_path_wpp = glue('{wd}/dat/wpp')
})

# Download --------------------------------------------------------

# download World Population Prospects
# single year-age population estimates 1950-2019
wpp_medium_pop_estimates <-
  GET(url = cnst$wpp_estimates_url, progress())

# download World Population Prospects
# single year-age population projections 2020-2100
wpp_medium_pop_projections <-
  GET(url = cnst$wpp_projections_url, progress())

# Export ----------------------------------------------------------

# Save to file
writeBin(
  object = content(wpp_medium_pop_estimates, 'raw'),
  con = glue('{cnst$out_path_wpp}/wpp_medium_pop_estimates.csv')
)

writeBin(
  object = content(wpp_medium_pop_projections, 'raw'),
  con = glue('{cnst$out_path_wpp}/wpp_medium_pop_projections.csv')
)
