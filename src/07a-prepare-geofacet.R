#===============================================================================
# 2021-02-01 -- ex2020
# Re-touch figures decomposition
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com
#===============================================================================
# UPD  2021-02-09 ------------------------------

library(tidyverse)
library(magrittr)
library(geofacet)
library(countrycode)

cntr <- c(
    'AT', 'BE', 'BG', 'CH', 'CL', 'CZ', 'DE', 'DK', 'EE', 'ES', 'FI', 'FR',
    'GB-EAW', 'GB-NIR', 'GB-SCT',
    'HU', 'IL', 'LT', 'NL', 'PL', 'PT', 'SE', 'SI'
)

ids <- tibble(
    code = cntr,
    name = cntr %>%
        countrycode(origin = "iso2c", destination = "country.name")
) %>%
    mutate(
        name = case_when(
                code=="GB-EAW" ~ "England and Wales",
                code=="GB-SCT" ~ "Scotland",
                code=="GB-NIR" ~ "Northern Ireland",
                TRUE ~ name
            )
    )

# use the browser interface to align
ids %>% geofacet::grid_design()

# paste output of the visual grid aligning (did without names)
grid_ex2020 <- data.frame(
    code = c("FI", "GB-NIR", "GB-SCT", "SE", "EE", "DK", "GB-EAW", "LT", "NL", "PL", "BE", "CZ", "DE", "IL", "AT", "CH", "FR", "HU", "CL", "BG", "ES", "PT", "SI"),
    row = c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 5),
    col = c(8, 3, 4, 7, 9, 7, 4, 9, 6, 8, 6, 8, 7, 1, 8, 6, 5, 9, 1, 9, 4, 3, 8),
    stringsAsFactors = FALSE
) %>%
    # attach names
    left_join(ids, .)

grid_ex2020 %>% grid_preview()

write_rds(grid_ex2020, path = "out/grid_ex2020.rds")
