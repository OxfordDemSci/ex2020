#===============================================================================
# 2021-02-09 -- ex2020
# All the data preparation for plotting
#=============================================================================# # # UPD  2021-06-09 ------------------------------
# UPD  2021-07-20 ------------------------------




library(tidyverse)
library(magrittr)
library(countrycode)
library(here); wd <- here()
library(glue)



# Create a minimal table with codes and names of the countries ------------
config <- yaml::read_yaml(glue::glue('{wd}/cfg/config.yaml'))
cntr = config$regions_for_all_cause_analysis

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
    ) %>%
    # add HMD codes
    mutate(
        code_hmd = name %>%
            countrycode(origin = "country.name", destination = "iso3c")
    ) %>%
    mutate(
        code_hmd = case_when(
            code=="DE" ~ "DEUTNP",
            code=="FR" ~ "FRATNP",
            code=="GB-EAW" ~ "GBRTENW",
            code=="GB-NIR" ~ "GBR_NIR",
            code=="GB-SCT" ~ "GBR_SCO",
            TRUE ~ code_hmd
        )
    )

saveRDS(ids, glue('{wd}/out/ids.rds'))


# dataset for figures 1 and 2 ---------------------------------------------

# get the lt estimates
df_lt <- read_rds("{wd}/out/lt_output_85.rds" %>% glue)


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
    summarise(ex_diff_1519 = ex_diff %>% sum(na.rm = T)) %>%
    ungroup()

ex1519asb <- df_lt %>%
    transmute(
        code = region_iso,
        sex, year, age = x,
        ex
    ) %>%
    # hard fix for Germany and Chile -- take 2016 values
    pivot_wider(names_from = year, values_from = ex, names_prefix = "ex_") %>%
    mutate(ex_2015 = if_else(is.na(ex_2015), ex_2016, ex_2015)) %>%
    select(-ex_2016, -ex_2017, -ex_2018, ex_2020)



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
    left_join(rank_d0m20) %>%
    # average difference 2015-19
    mutate(
        # avg_ex_diff_1519 = case_when(
        #     code %in% c("CL", "DE") ~ ex_diff_1519/3,
        #     TRUE ~ ex_diff_1519/4
        # ),
        # !!! now averages are calculated by Jonas in lt_ex_diff.rds
        name = name %>%
            # asterix for Germany, Greece, and Chile
            str_replace("Chile", "Chile*") %>%
            str_replace("Germany", "Germany*") %>%
            str_replace("Greece", "Greece*") %>%
            as_factor() %>%
            fct_reorder(rank_d0m20)
    )


# attach CIs from Jonas' export
df_ex_ci <- df_ex %>%
    left_join(
        read_rds("{wd}/out/lt_ex_diff.rds" %>% glue) %>%
            transmute(
                code = region_iso,
                sex,
                age = x,
                ex_diff_1920_q025,
                ex_diff_1920_q975,
                ex_avgdiff_pre2020,
                ex_avgdiff_pre2020_q025,
                ex_avgdiff_pre2020_q975
            ),
        by = c("code", "sex", "age")

    )

# save the data frame for figures 1 and 2
write_rds(df_ex_ci, "{wd}/out/df_ex_ci.rds" %>% glue)


# export a simplified CSV for the dashboard -------------------------------

df_export <- df_ex_ci %>%
    filter(age %in% c(seq(0, 80, 10), 85)) %>%
    transmute(
        name, code, sex, age,
        ex_2015, ex_2019, ex_2020,
        ex_diff_1519,
        ex_diff_1920 = ex_diff,
        ex_diff_1920_q025, ex_diff_1920_q975,
        ex_avgdiff_pre2020,
        ex_avgdiff_pre2020_q025,
        ex_avgdiff_pre2020_q975,
        rank_e0f19, rank_d0m20
    )


write_csv(df_export, "{wd}/out/df_export.csv" %>% glue) # export for Ian


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
    mutate(
        name = name %>%
            # asterix for Germany and Chile
            str_replace("Chile", "Chile*") %>%
            str_replace("Germany", "Germany*") %>%
            str_replace("Greece", "Greece*") %>%
            as_factor() %>%
            fct_reorder(rank_d0m20),
        period = period %>% fct_rev
    )

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
    mutate(
        name = fct_reorder(name, rank_d0m20),
        period_cause = period_cause %>% fct_rev
    )

write_rds(df_dec_age_cause, "{wd}/out/df_dec_age_cause.rds" %>% glue)
