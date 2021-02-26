#===============================================================================
# 2021-02-24 -- ex2020
# Sensitivity checks
#===============================================================================

library(tidyverse)
library(magrittr)
library(countrycode)
library(here); wd <- here()
library(glue)
library(fs)
library(patchwork)
library(hrbrthemes)
library(cowplot)

# identifiers helper table
ids <- read_rds("{wd}/out/ids.rds" %>% glue)

# our life tables
lt_85 <- read_rds("{wd}/out/lt_output_85.rds" %>% glue)
lt_100 <- read_rds("{wd}/out/lt_output_100.rds" %>% glue)

# WPP
ex_wpp <- read_rds("{wd}/dat/wpp/wpp_ex.rds" %>% glue)

# HMD
lt1x1 <- read_rds(glue("{wd}/dat/hmdhfd/lt-1x1.rds"))


# join
ex_comp <-
    left_join(
        lt_85 %>%
            transmute(code = region_iso, year, sex, age = x, ex_85 = ex),
        lt_100 %>%
            transmute(code = region_iso, year, sex, age = x, ex_100 = ex)
    ) %>%
    left_join(ids, "code") %>%
    left_join(
        lt1x1 %>%
            filter(!sex == "b") %>%
            transmute(
                code_hmd = country,
                year,
                age,
                sex = sex %>% as_factor %>%
                    lvls_revalue(c("Female", "Male")),
                ex_hmd = ex
            )
    ) %>%
    left_join(
        ex_wpp %>%
            transmute(name = Location, sex = Sex, age = AgeGrpStart, ex_wpp = ex)
    )

