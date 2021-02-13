#===============================================================================
# 2021-02-09 -- ex2020
# All the data preparation for plotting
#===============================================================================

library(tidyverse)
library(magrittr)
library(countrycode)
library(here); wd <- here()
library(glue)



# Create a minimal table with codes and names of the countries ------------

cntr <- c(
    'AT', 'BE', 'BG', 'CH', 'CL', 'CZ',
    'DE', 'DK', 'EE', 'ES', 'FI', 'FR',
    'GB-EAW', 'GB-NIR', 'GB-SCT', 'HU',
    'IL', 'LT', 'NL', 'PL', 'PT', 'SE', 'SI'
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

write_rds(ids, glue('{wd}/out/ids.rds'))


# dataset for figures 1 and 2 ---------------------------------------------

# get the lt estimates
df_lt <- read_rds("{wd}/out/lt_output.rds" %>% glue)

ex_diff <- df_lt %>%
    transmute(
        code = region_iso,
        sex, year, age = x,
        ex
    ) %>%
    group_by(code, sex, age) %>%
    mutate(ex_diff = ex %>% subtract(lag(ex))) %>%
    ungroup()

ex20 <- ex_diff %>% filter(year == 2020)

ex1519 <- ex_diff %>%
    filter(! year %in% c(2020, 2015)) %>%
    select(-ex) %>%
    group_by(code, sex, age) %>%
    summarise(ex_diff_1519 = ex_diff %>% sum) %>%
    ungroup()

ex1519asb <- df_lt %>%
    transmute(
        code = region_iso,
        sex, year, age = x,
        ex
    ) %>%
    filter(year %in% c(2015, 2019)) %>%
    pivot_wider(names_from = year, values_from = ex, names_prefix = "ex_")

# ranking variable for Fig 1 (absolute levels) -- based on Female e0 2019
rank_e0f19 <- ex1519asb %>%
    filter(age == 0, sex == "Female") %>%
    arrange(ex_2019) %>%
    transmute(code, rank_e0f19 = seq_along(code))

# ranking variable for Fig 2 (changes) -- based on the Male e0 change 2020
rank_d0m20 <- ex20 %>%
    filter(age == 0, sex == "Male") %>%
    arrange(ex_diff) %>%
    transmute(code, rank_d0m20 = seq_along(code))


df_ex <- left_join(ex20, ex1519) %>%
    left_join(ex1519asb) %>%
    left_join(ids) %>%
    left_join(rank_e0f19) %>%
    left_join(rank_d0m20)

# save the data frame for figures 1 and 2
write_rds(df_ex, "{wd}/out/df_ex.rds" %>% glue)
write_csv(df_ex, "{wd}/out/df_ex.csv" %>% glue) # export for Ian


# datasets for figures 3 and 4 --------------------------------------------


df_dec <- read_rds("{wd}/out/decomposition_results.rds" %>% glue)

# for Fig 3
df_dec_age <- df_dec$decomposition_results_by_age %>%
    mutate(
        age3 = x %>% cut(c(0, 60, 80, Inf), right = FALSE) %>%
            lvls_revalue(c("0 to 59", "60 to 79", "80 +")),
        period = year.final %>% cut(c(2015, 2019, 2020)) %>%
            lvls_revalue(c("2015-19", "2019-20"))
    ) %>%
    group_by(code = region_iso, sex, age3, period) %>%
    summarise(ctb = contribution %>% sum(na.rm = T)) %>%
    ungroup() %>%
    left_join(ids) %>%
    left_join(rank_d0m20) %>%
    drop_na(name) %>%
    mutate(name = fct_reorder(name, rank_d0m20))

write_rds(df_dec_age, "{wd}/out/df_dec_age.rds" %>% glue)

# for Fig 4
df_dec_age_cause <- df_dec$decomposition_results_by_age_cause %>%
    filter(! (year.final!=2020&cause=="covid")) %>%  # remove impossible combination
    mutate(
        age3 = x %>% cut(c(0, 60, 80, Inf), right = FALSE) %>%
            lvls_revalue(c("0 to 59", "60 to 79", "80 +")),
        period = year.final %>% cut(c(2015, 2019, 2020)) %>%
            lvls_revalue(c("2015-19", "2019-20")),
        period_cause = paste(period, cause, sep = "_") %>%
            as_factor() %>% droplevels() %>%
            lvls_revalue(c("2015-19", "2019-20 COVID", "2019-20 non-COVID")) %>%
            lvls_reorder(c(1,3,2))
    ) %>%
    group_by(code = region_iso, sex, age3, period_cause) %>%
    summarise(ctb = contribution %>% sum(na.rm = T)) %>%
    ungroup() %>%
    left_join(ids) %>%
    left_join(rank_d0m20) %>%
    drop_na(name) %>%
    mutate(name = fct_reorder(name, rank_d0m20))

write_rds(df_dec_age_cause, "{wd}/out/df_dec_age_cause.rds" %>% glue)
