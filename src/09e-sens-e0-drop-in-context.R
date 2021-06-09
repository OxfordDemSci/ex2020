#===============================================================================
# 2021-03-17 -- ex2020
# How exceptional is e0 change in 2020
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

ids <- readRDS("{wd}/out/ids.rds" %>% glue)
lt1x1 <- readRDS("{wd}/dat/hmdhfd/lt-1x1.rds" %>% glue)
ex_diff_2020 <- read_rds("{wd}/out/df_ex.rds" %>% glue)

# small dataset with either the first year or 1900
hmd_start <- lt1x1 %>%
    group_by(country) %>%
    summarise(
        year_first = year %>% min
    ) %>%
    mutate(
        year_first = case_when(
            year_first %>% is_weakly_greater_than(1900) ~ year_first,
            TRUE ~ 1900
        )
    )

# add 2020
df20 <- read_rds("{wd}/out/df_ex.rds" %>% glue) %>%
    filter(age == 0) %>%
    transmute(name, country = code_hmd, sex, year, e0_diff = ex_diff) %>%
    left_join(hmd_start) %>%
    # arrange countries by decreasing length of the time series
    group_by(sex) %>%
    arrange((year - year_first)) %>%
    mutate(
        country = country %>% as_factor %>% fct_inorder(),
        name = name %>% as_factor %>% fct_inorder()
    ) %>%
    # create facet positioning variables on a 7x5 canvas
    mutate(
        row = name %>%
            lvls_revalue(
                new_levels = 1:6 %>% rep(5) %>% paste %>% tail(29)
            ),
        col = name %>%
            lvls_revalue(
                new_levels = 1:5 %>% rep(each =6) %>% paste %>% tail(29)
            )
    ) %>%
    ungroup()

# long series
df00 <- lt1x1 %>%
    filter(age == 0,  year > 1899) %>%
    group_by(country, sex) %>%
    transmute(
        year,
        e0_diff = ex - lag(ex)
    ) %>%
    drop_na() %>%
    ungroup() %>%
    right_join(df20 %>% distinct(country, row, col))


# e0 yearly diff ----------------------------------------------------------

df20 %>%
    filter(sex == "Male") %>%
    ggplot()+
    geom_hline(
        data = . %>% distinct(row, col),
        aes(yintercept = 0), color = "#ffea00", size = .5
    )+
    # geom_ribbon(
    #     data = df00 %>% filter(sex == "m"),
    #     aes(x = year, ymin = -1, ymax = 1),
    #     fill = "#dfff00", alpha = .1
    # )+
    geom_col(aes(year, e0_diff), fill = "#64B6EEFF", width = 1.5)+
    geom_col(
        data = df00 %>% filter(sex == "m"),
        aes(year, e0_diff),fill = "#777777", width = 1
    )+
    geom_segment(
        aes(y = e0_diff, yend = e0_diff, x = year_first, xend = 2020),
        color = "#64B6EEFF", size = .3
    )+
    # mark outlier years
    geom_point(
        data = df00 %>% filter(sex == "m") %>% filter(e0_diff > 3),
        aes(x = year, y = 2.98),
        shape = 45, size = 2, color = "#ffea00"
    )+
    geom_point(
        data = df00 %>% filter(sex == "m") %>% filter(e0_diff < -3),
        aes(x = year, y = -2.98),
        shape = 45, size = 2, color = "#ffea00"
    )+
    facet_grid(row~col, scales = "free_x", space="free")+
    scale_x_continuous(
        breaks = c(1900, 1918, 1939, 1945, 1965, 1990, 2010),
        labels = c("1900", "'18", "", "'45", "'65", "'90", "2010")
    )+
    scale_y_continuous(breaks = seq(-4, 4, 2), position = "right")+

    coord_cartesian(ylim = c(-3, 3), expand = F)+
    theme_minimal(base_family = font_rc)+
    theme(
        panel.grid.minor = element_blank(),
        panel.spacing.x = unit(.75, "lines"),
        strip.text = element_blank(),
        strip.background = element_blank(),
        legend.position = c(.88, .04),
        plot.title = element_text(family = "Roboto Slab")
    )+
    labs(
        x = NULL,
        y = "Yearly change in life expectancy at birth",
        fill = "Change e0, years"
    )+
    geom_text(
        aes(label = name, y = 2.9, x = year),
        size = 3, hjust = 1, vjust = 1,
        family = font_rc
    )+
    geom_text(
        data = tibble(row = 6, col = 5, year = 1990, e0_diff = -2),
        aes(year, e0_diff),
        label = "Change in 2020", size = 3, family = font_rc,
        color = "#64B6EEFF", fontface = 2
    )+
    # annotate sex in the free space
    # first, createe the white background
    geom_rect(
        data = tibble(row = 1, col = 1, year = 2000),
        aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf),
        color = NA, fill = "#ffffff"
    )+
    geom_text(
        data = tibble(row = 1, col = 1, year = 1990, e0_diff = 0),
        aes(label = "MALES", year, e0_diff),
        size = 5, family = font_rc, angle = 90,
        color = "#64B6EEFF", fontface = 2
    )


diff_m <- last_plot()

ggsave(
    "{wd}/out/fig-3.pdf" %>% glue,
    diff_m, width = 7, height = 9, device = cairo_pdf
)

# the same for females

df20 %>%
    filter(sex == "Female") %>%
    ggplot()+
    geom_hline(
        data = . %>% distinct(row, col),
        aes(yintercept = 0), color = "#18ffff", size = .5
    )+
    # geom_ribbon(
    #     data = df00 %>% filter(sex == "f"),
    #     aes(x = year, ymin = -1, ymax = 1),
    #     fill = "#18ffff", alpha = .1
    # )+
    geom_col(aes(year, e0_diff), fill = "#B5223BFF", width = 1.5)+
    geom_col(
        data = df00 %>% filter(sex == "f"),
        aes(year, e0_diff),fill = "#777777", width = 1
    )+
    geom_segment(
        aes(y = e0_diff, yend = e0_diff, x = year_first, xend = 2020),
        color = "#B5223BFF", size = .25
    )+
    # mark outlier years
    geom_point(
        data = df00 %>% filter(sex == "f") %>% filter(e0_diff > 3),
        aes(x = year, y = 2.98),
        shape = 45, size = 2, color = "#18ffff"
    )+
    geom_point(
        data = df00 %>% filter(sex == "f") %>% filter(e0_diff < -3),
        aes(x = year, y = -2.98),
        shape = 45, size = 2, color = "#18ffff"
    )+
    facet_grid(row~col, scales = "free_x", space="free")+
    scale_x_continuous(
        breaks = c(1900, 1918, 1939, 1945, 1965, 1990, 2010),
        labels = c("1900", "'18", "", "'45", "'65", "'90", "2010")
    )+
    scale_y_continuous(breaks = seq(-4, 4, 2), position = "right")+

    coord_cartesian(ylim = c(-3, 3), expand = F)+
    theme_minimal(base_family = font_rc)+
    theme(
        panel.grid.minor = element_blank(),
        panel.spacing.x = unit(.75, "lines"),
        strip.text = element_blank(),
        strip.background = element_blank(),
        legend.position = c(.88, .04),
        plot.title = element_text(family = "Roboto Slab")
    )+
    labs(
        x = NULL,
        y = "Yearly change in life expectancy at birth",
        fill = "Change e0, years"
    )+
    geom_text(
        aes(label = name, y = 2.9, x = year),
        size = 3, hjust = 1, vjust = 1,
        family = font_rc
    )+
    geom_text(
        data = tibble(row = 6, col = 5, year = 1990, e0_diff = -2),
        aes(year, e0_diff),
        label = "Change in 2020", size = 3, family = font_rc,
        color = "#B5223BFF", fontface = 2
    )+
    # annotate sex in the free space
    # first, createe the white background
    geom_rect(
        data = tibble(row = 1, col = 1, year = 2000),
        aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf),
        color = NA, fill = "#ffffff"
    )+
    geom_text(
        data = tibble(row = 1, col = 1, year = 1990, e0_diff = 0),
        aes(label = "FEMALES", year, e0_diff),
        size = 5, family = font_rc, angle = 90,
        color = "#B5223BFF", fontface = 2
    )

diff_f <- last_plot()


ggsave(
    "{wd}/out/sens/fig-s4-hmd-yearly-e0-diff-f.pdf" %>% glue,
    diff_f, width = 7, height = 9, device = cairo_pdf
)
