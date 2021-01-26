# Global objects

#' Create Unique Row ID
#'
#' @param region_iso iso-3166-1 alpha 2 country code with optional
#'   iso-3166-2 region code added, separated by a hyphen.
#' @param sex 'Male' or 'Female'
#' @param age_start Positive Integer.
#' @param year Positive Integer.
#' @param week Positive Integer.
#'
#' @return
#' String with fixed length row ID constructed from input.
#'
#' @examples
#' GenerateRowID('DE-BW', 'Male', 0, 2020)
GenerateRowID <- function(region_iso, sex, age_start, year) {
  region_id <- sapply(region_iso, function (x) {
    expanded_region <- '------'
    substr(expanded_region, 1, nchar(x)) <- x
    return(expanded_region)
  })
  sex_id <- as.character(factor(sex, c('Male', 'Female'), c('M', 'F')))
  age_id <- sprintf('%03d', age_start)
  year_id <- sprintf('%04d', year)
  
  row_id <- paste0(region_id, sex_id, age_id, year_id)
  
  return(row_id)
}

#' Check if ISO-Year Features Week 53
YearHasIsoWeek53 <- function (year) {
  require(ISOweek)
  a <- ISOweek::ISOweek(ISOweek::ISOweek2date(paste0(year, '-W53-1')))
  b <- paste0(year, '-W53')
  a == b
}
