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
ex_diff_2020 <- read_rds("{wd}/out/df_ex.rds" %>% glue) %>%
    filter(age == 0, sex == "Male") %>%
    transmute(name, country = code_hmd, sex, year, e0_diff = ex_diff, ex) %>%
    left_join(hmd_start)


# e0 yearly diff ----------------------------------------------------------

lt1x1 %>%
    right_join(ids, c("country" = "code_hmd")) %>%
    filter(
        age == 0,
        sex == "m"
    ) %>%
    drop_na() %>%
    mutate(e0_diff = ex - lag(ex)) %>%
    group_by(country) %>%
    filter(!year == min(year), year > 1899) %>%
    ungroup() %>%
    ggplot(aes(year, e0_diff))+
    geom_hline(yintercept = 0, color = 5, size = .2)+
    geom_ribbon(
        aes(xmin = -Inf, xmax = Inf, ymin = -1, ymax = 1),
        fill = 5, alpha = .1
    )+
    geom_col(fill = "#777777", width = 1)+
    geom_col(data = ex_diff_2020, fill = 2)+
    geom_segment(
        data = ex_diff_2020,
        aes(y = e0_diff, yend = e0_diff, x = year_first, xend = 2020),
        color = 2, size = .2
    )+
    # mark outlier years
    geom_point(
        data = . %>% filter(e0_diff > 5),
        aes(y = 4.96),
        shape = 45, size = 2, color = 5
    )+
    facet_wrap(~name, ncol = 4)+
    scale_x_continuous(
        breaks = c(1900, 1918, 1939, 1945, 1965, 1990, 2010),
        labels = c("1900", "'18", "", "'45", "'65", "'90", "2010")
    )+
    scale_y_continuous(breaks = seq(-4, 4, 2))+

    coord_cartesian(ylim = c(-5, 5), expand = F)+
    theme_minimal(base_family = font_rc)+
    theme(
        panel.grid.minor = element_blank(),
        legend.position = c(.88, .04),
        plot.title = element_text(family = "Roboto Slab")
    )+
    labs(
        x = NULL,
        y = "Yearly change in male life expectancy at birth",
        fill = "Change e0, years"
    )+
    geom_text(
        data = tibble(name = "Austria", year = 1990, e0_diff = -2),
        label = "Change in 2020", size = 3, family = font_rc,
        color = 2, fontface = 2
    )

p_diffs <- last_plot()


ggsave(
    "{wd}/out/sens/hmd-yearly-e0-diff.pdf", p_diffs,
    width = 8, height = 8, device = cairo_pdf
)
