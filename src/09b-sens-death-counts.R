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
library(patchwork)
library(hrbrthemes)
library(cowplot)

# identifiers helper table
ids <- read_rds("{wd}/out/ids.rds" %>% glue)

# HMD death counts
d1x1 <- read_rds(glue("{wd}/dat/hmdhfd/deaths-1x1.rds"))

# collapse age
hmd_d <- d1x1 %>%
    pivot_longer(f:b, names_to = "sex", values_to = "death") %>%
    group_by(code_hmd = country, year, sex) %>%
    summarise(d_hmd = death %>% sum(na.rm = T)) %>%
    ungroup() %>%
    mutate(sex = sex %>% as_factor %>% lvls_revalue(c("Both", "Female", "Male")))

# our death count after ungrouping
our_d <- readRDS(glue('{wd}/out/lt_input.rds')) %>%
    filter(!year==2020) %>%
    # filter()
    group_by(code = region_iso, year, sex) %>%
    summarise(d_our = death_total %>% sum(na.rm = T)) %>%
    ungroup() %>%
    filter(!d_our==0)

# join
df_d <- our_d %>%
    inner_join(ids) %>%
    left_join(hmd_d)


# visualize
df_d %>%
    ggplot(aes(year, color = sex))+
    geom_hline(yintercept = 1)+
    geom_point(aes(y = d_our/d_hmd, shape = sex), size = 2)+
    scale_color_manual(
        values = c("#B5223BFF", "#64B6EEFF"),
        guide  = guide_legend(ncol = 1)
    )+
    scale_shape_manual(values = c(1, 16), guide = NULL)+
    scale_x_continuous(labels = c("2015", "'16", "'17", "'18", "'19"))+
    scale_y_continuous(trans = "log")+
    coord_cartesian(ylim = c(.96, 1.05))+
    facet_wrap(~name, ncol = 7)+
    theme_minimal(base_family = font_rc)+
    theme(
        legend.position = c(.93, .07),
        panel.grid.minor = element_blank(),
        panel.spacing.x = unit(1, "lines")
    )+
    labs(
        color = NULL,
        y = "total deaths | Our divided by HMD"
    )

ggsave(
    "{wd}/out/sens/death-totals-our-hmd.pdf" %>% glue,
    width = 8, height = 6, device = cairo_pdf
)



# by age group ------------------------------------------------------------

# collapse age
hmd_d_a <- d1x1 %>%
    pivot_longer(f:b, names_to = "sex", values_to = "death") %>%
    mutate(agr = age %>% cut(c(0, 60, 80, Inf))) %>%
    group_by(code_hmd = country, year, sex, agr) %>%
    summarise(d_hmd = death %>% sum(na.rm = T)) %>%
    ungroup() %>%
    mutate(sex = sex %>% as_factor %>% lvls_revalue(c("Both", "Female", "Male")))

# our death count after ungrouping
our_d_a <- readRDS(glue('{wd}/out/lt_input.rds')) %>%
    filter(!year==2020) %>%
    mutate(agr = age_start %>% cut(c(0, 60, 80, Inf))) %>%
    group_by(code = region_iso, year, sex, agr) %>%
    summarise(d_our = death_total %>% sum(na.rm = T)) %>%
    ungroup() %>%
    filter(!d_our==0)

# join
df_d_a <- our_d_a %>%
    inner_join(ids) %>%
    left_join(hmd_d_a)


# visualize
df_d_a %>%
    filter(sex == "Female") %>%
    drop_na() %>%
    ggplot(aes(year, color = sex))+
    geom_hline(yintercept = 1, color = "#18ffff")+
    geom_point(aes(y = d_our/d_hmd, color = agr), size = 5, shape = 95)+
    scale_color_manual(
        values = c("#ff00ff", "#e91e63", "#38006b"),
        guide  = guide_legend(ncol = 1, reverse = TRUE)
    )+
    scale_x_continuous(labels = c("2015", "'16", "'17", "'18", "'19"))+
    scale_y_continuous(trans = "log")+
    coord_cartesian(ylim = c(.9, 1.12))+
    facet_wrap(~name, ncol = 7)+
    theme_minimal(base_family = font_rc)+
    theme(
        legend.position = c(.93, .07),
        panel.grid.minor = element_blank(),
        panel.spacing.x = unit(1, "lines")
    )+
    labs(
        color = "FEMALES",
        y = "Deaths by major age groups | Our divided by HMD"
    )

a <- last_plot()

df_d_a %>%
    filter(sex == "Male") %>%
    drop_na() %>%
    ggplot(aes(year, color = sex))+
    geom_hline(yintercept = 1, color = "#dfff00")+
    geom_point(aes(y = d_our/d_hmd, color = agr), size = 5, shape = 95)+
    scale_color_manual(
        values = c("#00ffff", "#3FB3F7FF", "#003737FF"),
        guide  = guide_legend(ncol = 1, reverse = TRUE)
    )+
    scale_x_continuous(labels = c("2015", "'16", "'17", "'18", "'19"))+
    scale_y_continuous(trans = "log")+
    coord_cartesian(ylim = c(.9, 1.12))+
    facet_wrap(~name, ncol = 7)+
    theme_minimal(base_family = font_rc)+
    theme(
        legend.position = c(.93, .07),
        panel.grid.minor = element_blank(),
        panel.spacing.x = unit(1, "lines")
    )+
    labs(
        color = "MALES",
        y = "Deaths by major age groups | Our divided by HMD"
    )

b <- last_plot()

out <- a / b

ggsave(
    "{wd}/out/sens/death-totals-our-hmd-by-age.pdf" %>% glue, out,
    width = 8, height = 9, device = cairo_pdf
)


