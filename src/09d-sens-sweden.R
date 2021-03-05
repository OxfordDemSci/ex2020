#===============================================================================
# 2021-03-04-- ex2020
# Sensitivity checks. Sweden
#===============================================================================

library(tidyverse)
library(magrittr)
library(countrycode)
library(here); wd <- here()
library(glue)
library(fs)
library(patchwork)
library(hrbrthemes)


# read in the data
se_exp <- read_csv("dat/scb/BE0101A5_20210304-132026.csv", skip = 1) %>%
    pivot_longer(`2006`:`2020`, names_to = "year", values_to = "exp")


se_death <- read_csv("dat/scb/BE0101D9_20210304-130831.csv", skip = 1) %>%
    pivot_longer(`1968`:`2020`, names_to = "year", values_to = "death")

# join and calc death rates
se_dr <- se_exp %>%
    left_join(se_death) %>%
    mutate(mx = death / exp)

# # trapez approx of life expectancy from a logmx schedule over ages 0..99
# e0 = function(logmx) {
#     mx = exp(logmx)
#     px = exp(-mx)
#     lx = c(1,cumprod(px))
#     return( sum(head(lx,-1) + tail(lx,-1)) / 2)
# }
#
# # calc e0
# se_e0 <- se_dr %>%
#     group_by(sex, year) %>%
#     summarise(e0 = mx %>% log %>% e0)
#
# se_e0 %>%
#     filter(year %in% 2019:2020) %>%
#     mutate(e0 = e0 %>% round(2))%>%
#     view

# simple piecewise-exponential life-table
CalculateLifeTable <-
    function (df, x, nx, Dx, Ex) {

        require(dplyr)

        df %>%
            transmute(
                x = {{x}},
                nx = {{nx}},
                mx = {{Dx}}/{{Ex}},
                px = exp(-mx*{{nx}}),
                qx = 1-px,
                lx = head(cumprod(c(1, px)), -1),
                dx = c(-diff(lx), tail(lx, 1)),
                Lx = ifelse(mx==0, lx*nx, dx/mx),
                Tx = rev(cumsum(rev(Lx))),
                ex = Tx/lx
            )

    }

# use alternative LT func
se_e0 <- se_dr %>%
    group_by(sex, year) %>%
    group_modify(~{
        CalculateLifeTable(.x, age, 1, death, exp)
    }) %>%
    ungroup() %>%
    # fix age starting from 0 (came through factor levels to numeric)
    mutate(x = x - 1)

saveRDS(se_e0, file = glue("{wd}/out/patch-sweden-lt.rds"), compress = "xz")

se_e0 %>%
    filter(year %in% 2019:2020, x == 1) %>%
    transmute(e0 = ex %>% round(2))%>%
    view
