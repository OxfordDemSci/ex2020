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
    ungroup()

# join
df_d <- our_d %>%
    inner_join(ids) %>%
    left_join(hmd_d)


# visualize
df_d %>%
    ggplot(aes(year, color = sex))+
    geom_hline(yintercept = 1)+
    geom_point(aes(y = d_our/d_hmd, shape = sex), size = 2)+
    scale_color_manual(values = c("#B5223BFF", "#64B6EEFF"))+
    scale_shape_manual(values = c(1, 16), guide = NULL)+
    scale_x_continuous(labels = c("2015", "'16", "'17", "'18", "'19"))+
    coord_cartesian(ylim = c(.95, 1.03))+
    facet_wrap(~name, ncol = 4)+
    theme_minimal(base_family = font_rc)+
    theme(
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        panel.spacing.x = unit(1, "lines")
    )+
    labs(
        color = NULL,
        y = "total deaths | Our divided by HMD"
    )

ggsave(
    "{wd}/out/sens/death-totals-our-hmd.png" %>% glue,
    width = 8, height = 6, type = "cairo"
)
