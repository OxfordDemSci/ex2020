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

# # WPP
# ex_wpp <- read_rds("{wd}/dat/wpp/wpp_ex.rds" %>% glue)
#
# # HMD
# lt1x1 <- read_rds(glue("{wd}/dat/hmdhfd/lt-1x1.rds"))

# WPP and HMD ex estimates in lt_input
lt_input <- read_rds("{wd}/out/lt_input_85.rds" %>% glue)

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
        lt_input %>%
            transmute(
                code = region_iso,
                sex, year, age = age_start,
                ex_hmd = ex_hmd_estimate,
                ex_wpp = ex_wpp_estimate
            )
    )

# plot
ex_comp %>%
    filter(age == 0) %>%
    ggplot(aes(year))+
    geom_line(aes(y = ex_hmd, color = sex), alpha = .5, size = 1)+
    geom_segment(
        aes(y = ex_wpp, yend = ex_wpp, x = 2015, xend = 2019),
        size = .5, color = 8
    )+
    geom_point(aes(y = ex_85, color = sex), shape = 1, size = .7)+
    geom_point(aes(y = ex_100, color = sex), shape = 4, size = .5)+
    scale_color_manual(
        values = c("#B5223BFF", "#64B6EEFF"),
        guide  = guide_legend(ncol = 1)
    )+
    scale_x_continuous(labels = c("'15", "'16", "'17", "'18", "'19", "'20"))+
    # scale_y_continuous(breaks = 0:100)+
    facet_wrap(~name, ncol = 6)+
    theme_minimal(base_family = font_rc)+
    theme(
        legend.position = c(.93, .07),
        panel.grid.minor = element_blank(),
        panel.spacing.x = unit(1, "lines")
    )+
    labs(
        color = NULL,
        y = "Life expectancy estimates, years",
        title = "Estimated yearly life expectancy at age 0 with open age group 85+ (circle) and 100+ (cross) compared with HMD (colored line) and WPP (grey) 5 year average estimates" %>% str_wrap(97)
    )

ggsave(
    "{wd}/out/sens/fig-S2-ex-comp.pdf" %>% glue,
    width = 8, height = 7, device = cairo_pdf
)

# # separate for sexes
#
# ex_comp %>%
#     filter(age == 0, sex == "Female") %>%
#     ggplot(aes(y = year))+
#     geom_line(aes(x = ex_hmd, color = sex), alpha = .5, size = 1)+
#     geom_segment(
#         aes(x = ex_wpp, xend = ex_wpp, y = 2015, yend = 2019),
#         size = .5, color = 8
#     )+
#     geom_point(aes(x = ex_85, color = sex), shape = 124, size = 3)+
#     geom_point(aes(x = ex_100, color = sex), shape = 43, size = 2)+
#     scale_color_manual(
#         values = c("#B5223BFF"),
#         guide  = guide_legend(ncol = 1)
#     )+
#     # scale_y_continuous(labels = c("2015", "'16", "'17", "'18", "'19", "'20"))+
#     scale_x_continuous(breaks = 0:100)+
#     facet_wrap(~name, ncol = 7, scales = "free_x")+
#     theme_minimal(base_family = font_rc)+
#     theme(
#         legend.position = c(.9, .07),
#         panel.grid.minor = element_blank(),
#         panel.spacing.x = unit(1, "lines")
#     )+
#     labs(
#         color = NULL,
#         x = NULL,
#         y = "Life expectancy estimates, years"
#     )
#
# ex_comp %>%
#     filter(age == 0) %>%
#     ggplot(aes(year))+
#     geom_line(aes(y = ex_hmd, color = sex), alpha = .5, size = 1)+
#     # geom_segment(
#     #     aes(y = ex_wpp, yend = ex_wpp, x = 2015, xend = 2019, color = "WPP"),
#     #     alpha = .5
#     # )+
#     geom_point(aes(y = ex_85, color = sex), shape = 95, size = 4)+
#     geom_point(aes(y = ex_100, color = sex), shape = 43, size = 2)+
#     scale_color_manual(
#         values = c("#B5223BFF", "#64B6EEFF"),
#         guide  = guide_legend(ncol = 1)
#     )+
#     scale_x_continuous(labels = c("2015", "'16", "'17", "'18", "'19", "'20"))+
#     facet_wrap(~name, ncol = 7, scales = "free")+
#     theme_minimal(base_family = font_rc)+
#     theme(
#         legend.position = c(.9, .07),
#         panel.grid.minor = element_blank(),
#         panel.spacing.x = unit(1, "lines")
#     )+
#     labs(
#         color = NULL,
#         y = "Life expectancy estimates | Our divided by HMD"
#     )
#
