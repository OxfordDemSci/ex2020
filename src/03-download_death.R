# Download data on weekly death counts

# STMF input data

# Init ------------------------------------------------------------

library(here); library(glue)
library(httr)

# Constants -------------------------------------------------------

wd <- here()

cnst <- list()
cnst <- within(cnst, {
  stmf_url = 'https://www.mortality.org/Public/STMF/Inputs/STMFinput.zip'
  out_path_tmp = glue('{wd}/tmp')
  out_path_stmf = glue('{wd}/dat/stmf')
})

# Download --------------------------------------------------------

# download international weekly death by age and sex counts from STMF
stmf_zip <-
  GET(url = cnst$stmf_url, progress())

# Export ----------------------------------------------------------

# Save to file
writeBin(
  object = content(stmf_zip, 'raw'),
  con = glue('{cnst$out_path_tmp}/stmf.zip')
)

# unzip
unzip(glue('{cnst$out_path_tmp}/stmf.zip'), exdir = glue('{cnst$out_path_stmf}'))
