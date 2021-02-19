#===============================================================================
# 2021-02-19 -- ex2020
# Sensitivity checks
#===============================================================================

library(tidyverse)
library(magrittr)
library(countrycode)
library(here); wd <- here()
library(glue)
library(fs)


# hmd life tables ---------------------------------------------------------

# !!! The code below assumes that one has full HMD downloaded and unpacked locally. To get a copy of it go to
# https://www.mortality.org/cgi-bin/hmd/hmd_download.php
# and choose "All statistics for HMD", currently 160326 Kb
# Ilya: my local copy was downloaded on 2021-02-02


# functions to read locally stored HMD data
fread_hmd <- function(x) x %>%
    data.table::fread(skip = 2, na.strings = ".") %>%
    mutate(Age = Age %>% str_remove("\\+") %>% as.integer()) %>%
    mutate_all(as.numeric)

fread_hmd_dir <- function(thedir) {
    require(fs)
    require(magrittr)
    require(janitor)

    suppressWarnings(
        thedir %>%
            dir_ls() %>%
            map_df(fread_hmd, .id = "country") %>%
            janitor::clean_names() %>%
            mutate(
                country = country %>%
                    str_remove(thedir %>% path_real()) %>%
                    str_remove("\\..*") %>%
                    str_remove("\\/"),
                age = age %>% str_remove("\\+") %>% as.integer()
            )
    )
}

# !!! this is the local HMD path -- change accordingly
hmdpath <- fs::as_fs_path("~/data/hmd/")

# deaths
d1x1 <- path(hmdpath, "deaths", "Deaths_1x1") %>% fread_hmd_dir() %>%
    rename(b = total, f = female, m = male) %>%
    # for the size of the dataset, only filter data since 2000
    filter(year %>% is_weakly_greater_than(2000))

saveRDS(d1x1, file = glue("{wd}/dat/hmdhfd/deaths-1x1.rds"), compress = "xz")

# life expectancy
lt1x1 <- bind_rows(
    b = path(hmdpath, "lt_both", "bltper_1x1") %>% fread_hmd_dir(),
    f = path(hmdpath, "lt_female", "fltper_1x1") %>% fread_hmd_dir(),
    m = path(hmdpath, "lt_male", "mltper_1x1") %>% fread_hmd_dir(),
    .id = "sex"
) %>%
    # for the size of the dataset, only filter data since 2000
    filter(year %>% is_weakly_greater_than(2000))

saveRDS(lt1x1, file = glue("{wd}/dat/hmdhfd/lt-1x1.rds"), compress = "xz")
