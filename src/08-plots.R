#===============================================================================
# 2021-02-02 -- ex2020
# Re-touch figures
#===============================================================================
# UPD  2021-06-09 ------------------------------
# UPD  2021-07-20 ------------------------------




library(tidyverse)
library(magrittr)
library(prismatic)
library(patchwork)
library(hrbrthemes)
library(cowplot)
library(here); wd <- here()
library(glue)

df_ex_ci <- read_rds("{wd}/out/df_ex_ci.rds" %>% glue)

# figrue 1 -- absolute levels of life expectancy --------------------------

df_ex_ci %>%
    mutate(
        name = name %>%
            fct_reorder(rank_e0f19)
    ) %>%
    filter(age %in% c(0, 60)) %>%
    drop_na(name) %>%
    transmute(
        name, sex, age,
        ex_2015, ex_2019, ex_2020 = ex
    ) %>%
    pivot_longer(
        cols = ex_2015:ex_2020,
        names_to = "year", values_to = "ex", names_prefix = "ex_"
    ) %>%
    mutate(age = age %>% as_factor()) %>%
    ggplot(aes(ex, name, color = sex, shape = year, size = year))+
    geom_hline(yintercept = seq(2, 28, 2), size = 5, color = "#eaeaea")+
    geom_point()+
    scale_shape_manual(values = c(124, 43, 16))+
    scale_size_manual(values = c(4, 4, 1.5))+
    scale_color_manual(values = c("#B5223BFF", "#64B6EEFF"))+
    scale_y_discrete(position = "right")+
    scale_x_continuous(position = "top")+
    facet_grid(~age, scales = "free_x")+
    theme_minimal(base_family = font_rc)+
    theme(
        legend.position = "none",
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        strip.text = element_blank(),
        panel.spacing.x = unit(2, "lines"),
        axis.text.y = element_text(face = 2)
    )+
    labs(
        x = "Life expectancy, years",
        y = NULL
    )

one <- last_plot()
ggsave("{wd}/out/fig-1.pdf" %>% glue, one,
       width = 6, height = 4.5, device = cairo_pdf)



# figure 2 -- changes in life expectancy ----------------------------------

df_ex_ci %>%
    filter(age %in% c(0, 60)) %>%
    drop_na(name) %>%
    ggplot(aes(y = name))+
    geom_hline(yintercept = seq(2, 28, 2), size = 5, color = "#eaeaea")+
    geom_vline(xintercept = 0, size = 2, color = "#bababa")+
    # geom_point(aes(x = ex_diff, color = sex, shape = sex), size = 2)+
    geom_pointrange(
        aes(
            color = sex,
            xmin = ex_diff_1920_q025,
            x = ex_diff,
            xmax = ex_diff_1920_q975
        ),
        fatten = .7, size = .3,
        position = position_dodge(width = 0.6)
    ) +
    geom_point(aes(x = ex_avgdiff_pre2020, color = sex), shape = 4, size = 1,
               position = position_dodge(width = 0.6))+
    geom_linerange(
        aes(
            color = sex,
            xmin = ex_avgdiff_pre2020_q025,
            xmax = ex_avgdiff_pre2020_q975
        ),
        size = 2, alpha = .35,
        position = position_dodge(width = 0.5)
    ) +
    scale_color_manual(values = c("#B5223BFF", "#64B6EEFF"))+
    scale_shape_manual(values = c(1, 16))+
    scale_y_discrete(position = "right")+
    scale_x_continuous(position = "top", breaks = seq(-2, .5, .5), expand = c(0,0))+
    coord_cartesian(xlim = c(-2.5, 1))+
    facet_wrap(~age, ncol = 3)+
    theme_minimal(base_family = font_rc)+
    theme(
        legend.position = "none",
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.text = element_blank(),
        panel.spacing.x = unit(2, "lines"),
        axis.text.y = element_text(face = 2)
    )+
    labs(
        x = "Difference in life expectancy, years",
        y = NULL
    )

two <- last_plot()
ggsave("{wd}/out/fig-2.pdf" %>% glue, two, width = 6, height = 4.5, device = cairo_pdf)


# figure 3 -- ex 2020 drop in historical context, promoted from fo --------
# see "src/09e-sens-e0-drop-in-context.R"



# plot decomposition ------------------------------------------------------

df_dec_age <- read_rds("{wd}/out/df_dec_age.rds" %>% glue)
df_dec_age_cause <- read_rds("{wd}/out/df_dec_age_cause.rds" %>% glue)


# figure 4 (previously Fig 3) -- contributions by age groups ---------------------------------

df_dec_age %>%
    filter(sex == "Female") %>%
    ggplot(aes(ctb, age3, fill = period))+
    geom_col()+
    geom_vline(xintercept = 0, size = .25, color = "#18ffff")+
    facet_wrap(~name, ncol = 7, dir = "v")+
    scale_fill_manual(
        values = c("#e91e63", "#38006b"),
        guide  = guide_legend(ncol = 1, reverse = TRUE)
    )+
    coord_cartesian(xlim = c(-1, 1.5), expand = F)+
    scale_x_continuous(
        # position = "top",
        breaks = seq(-1, 1, .5),
        labels = c("-1", "0.5", 0, "0.5", 1)
    )+
    theme_minimal(base_family = font_rc)+
    theme(
        panel.grid.minor.y = element_blank(),
        panel.spacing = unit(.5, "lines"),
        strip.text = element_text(face = 2),
        legend.position = c(.93, .1),
        axis.text.x = element_blank()
        # legend.background = element_rect(color = "#18ffff", size = 2),
        # plot.background = element_rect(color = "#eaeaea", size = 1)
    )+
    labs(
        fill = "FEMALES",
        # x = "Losses|Gains in life expectancy at birth, years",
        x = NULL,
        y = "Age groups"
    )

four_f <- last_plot()


df_dec_age %>%
    filter(sex == "Male") %>%
    ggplot(aes(ctb, age3, fill = period))+
    geom_col()+
    geom_vline(xintercept = 0, size = .25, color = "#dfff00")+
    facet_wrap(~name, ncol = 7, dir = "v")+
    scale_fill_manual(
        values = c("#3FB3F7FF", "#003737FF"),
        guide  = guide_legend(ncol = 1, reverse = TRUE)
    )+
    coord_cartesian(xlim = c(-1, 1.5), expand = F)+
    scale_x_continuous(
        # position = "top",
        breaks = seq(-1, 1, .5),
        labels = c("-1", "0.5", 0, "0.5", 1)
    )+
    theme_minimal(base_family = font_rc)+
    theme(
        panel.grid.minor.y = element_blank(),
        panel.spacing = unit(.5, "lines"),
        strip.text = element_text(face = 2),
        legend.position = c(.93, .07),
        # legend.background = element_rect(color = "#dfff00", size = 2),
        # plot.background = element_rect(color = "#eaeaea", size = 1)
    )+
    labs(
        fill = "MALES",
        x = "Losses|Gains in life expectancy at birth, years",
        y = "Age groups"
    )

four_m <- last_plot()

four <- four_f / four_m

ggsave("{wd}/out/fig-4.pdf" %>% glue, four, width = 10, height = 8, device = cairo_pdf)



# figure 5 (previously fig 4) -- split 2020 into covid and non-covid -------------------------

# df_dec_age_cause %>%
#     filter(sex == "Female") %>%
#     ggplot(aes(ctb, age3, fill = period_cause))+
#     geom_col()+
#     geom_vline(xintercept = 0, size = .25, color = "#18ffff")+
#     facet_wrap(~name, ncol = 5, dir = "v")+
#     scale_fill_manual(
#         values = c("#ff00ff", "#e91e63", "#38006b"),
#         guide  = guide_legend(ncol = 1, reverse = TRUE)
#     )+
#     coord_cartesian(xlim = c(-1, 1.5), expand = F)+
#     scale_x_continuous(
#         breaks = seq(-1, 1, .5),
#         labels = c("-1", ".5", 0, ".5", 1)
#     )+
#     theme_minimal(base_family = font_rc)+
#     theme(
#         panel.grid.minor.y = element_blank(),
#         panel.spacing = unit(.5, "lines"),
#         legend.position = c(.9, .1),
#         legend.background = element_rect(color = "#18ffff", size = 2),
#         plot.background = element_rect(color = "#18ffff", size = 1)
#     )+
#     labs(
#         fill = "FEMALES",
#         x = "Losses|Gains in life expectancy at birth, years",
#         y = "Age groups"
#     )
#
# four_f <- last_plot()
#
#
# df_dec_age_cause %>%
#     left_join(ids) %>%
#     left_join(rank_d0m20) %>%
#     drop_na(name) %>%
#     mutate(name = fct_reorder(name, rank_d0m20)) %>%
#     filter(sex == "Male") %>%
#     ggplot(aes(ctb, age3, fill = period_cause))+
#     geom_col()+
#     geom_vline(xintercept = 0, size = .25, color = "#dfff00")+
#     facet_wrap(~name, ncol = 6, dir = "v")+
#     scale_fill_manual(
#         values = c("#00ffff", "#3FB3F7FF", "#003737FF"),
#         guide  = guide_legend(ncol = 1, reverse = TRUE)
#     )+
#     coord_cartesian(xlim = c(-1, 1.5), expand = F)+
#     scale_x_continuous(
#         breaks = seq(-1, 1, .5),
#         labels = c("-1", ".5", 0, ".5", 1)
#     )+
#     theme_minimal(base_family = font_rc)+
#     theme(
#         panel.grid.minor.y = element_blank(),
#         panel.spacing = unit(.5, "lines"),
#         legend.position = c(.9, .1),
#         legend.background = element_rect(color = "#dfff00", size = 2),
#         plot.background = element_rect(color = "#dfff00", size = 1)
#     )+
#     labs(
#         fill = "MALES",
#         x = "Losses|Gains in life expectancy at birth, years",
#         y = "Age groups"
#     )
#
# four_m <- last_plot()
#
# four <- four_f / four_m

# UPD  2021-02-15 ------------------------------
# Only show 2020 covid contribution

# df_dec_age_cause %>%
#     left_join(ids) %>%
#     left_join(rank_d0m20) %>%
#     drop_na(name) %>%
#     mutate(name = fct_reorder(name, rank_d0m20)) %>%
#     filter(period_cause == "2019-20 COVID") %>%
#
#     ggplot(aes(ctb, age3, color = sex, shape = sex))+
#     geom_hline(yintercept = 2, size = 5, color = "#eaeaea")+
#     geom_vline(xintercept = 0, size = .25, color = "#dfff00")+
#     geom_point()+
#     scale_shape_manual(values = c(1, 16))+
#     scale_color_manual(values = c("#B5223BFF", "#64B6EEFF"))+
#     scale_y_discrete(position = "right")+
#     scale_x_continuous(breaks = c(-1, -.5, 0))+
#     facet_wrap(~name, ncol = 3, dir = "v")+
#     theme_minimal(base_family = font_rc)+
#     theme(
#         legend.position = "none",
#         panel.grid.major.y = element_blank(),
#         panel.grid.minor.y = element_blank(),
#         panel.spacing.x = unit(2, "lines")
#     )+
#     labs(
#         x = "Losses|Gains in life expectancy at birth, years",
#         y = NULL
#     )

# UPD  2021-02-22 ------------------------------
# only c19 and collapse age

df_dec_age_cause %>%
    left_join(ids) %>%
    drop_na(name) %>%
    filter(!period_cause == "2015-19") %>%
    group_by(name, sex, period_cause) %>%
    summarise(ctb = ctb %>% sum(na.rm = T)) %>%
    ungroup() %>%
    mutate(name = name %>% fct_reorder(ctb %>% desc)) %>%

    ggplot(aes(ctb, name, fill = sex, alpha = period_cause))+
    geom_col()+
    scale_fill_manual(values = c("#B5223BFF", "#64B6EEFF"))+
    scale_alpha_manual(values = c(1, .5))+
    scale_y_discrete(position = "right")+
    scale_x_continuous(position = "top", breaks = c(-2, -1.5, -1, -.5, 0))+
    facet_wrap(~sex, ncol = 2)+
    theme_minimal(base_family = font_rc)+
    theme(
        legend.position = "none",
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.spacing.x = unit(2, "lines"),
        strip.text = element_blank(),
        axis.text.y = element_text(face = 2),
    )+
    labs(
        x = "Losses|Gains in life expectancy at birth, years",
        y = NULL
    )

five <- last_plot()

ggsave("{wd}/out/fig-5.pdf" %>% glue, five, width = 6, height = 4, device = cairo_pdf)


# ggsave("tmp/fig-1.png", one, width = 6, height = 4.5, type = "cairo")
# ggsave("tmp/fig-2.png", two, width = 6, height = 4.5, type = "cairo")
# ggsave("tmp/fig-3.png", diff_m, width = 7, height = 9, type = "cairo")
# ggsave("tmp/fig-4.png", four, width = 10, height = 8, type = "cairo")
# ggsave("tmp/fig-5.png", five, width = 6, height = 4, type = "cairo")


# additional fig 1 without 2020 -------------------------------------------

df_ex %>%
    mutate(name = fct_reorder(name, rank_e0f19)) %>%
    filter(age %in% c(0, 60, 80)) %>%
    drop_na(name) %>%
    transmute(
        name, sex, age,
        ex_2015, ex_2019,
    ) %>%
    pivot_longer(
        cols = ex_2015:ex_2019,
        names_to = "year", values_to = "ex", names_prefix = "ex_"
    ) %>%
    mutate(age = age %>% as_factor()) %>%
    ggplot(aes(ex, name, color = sex, shape = year, size = year))+
    geom_hline(yintercept = seq(2, 22, 2), size = 5, color = "#eaeaea")+
    geom_point()+
    scale_shape_manual(values = c(124, 43))+
    scale_size_manual(values = c(4, 4))+
    scale_color_manual(values = c("#B5223BFF", "#64B6EEFF"))+
    scale_y_discrete(position = "right")+
    scale_x_continuous(position = "top")+
    facet_grid(~age, scales = "free_x")+
    theme_minimal(base_family = font_rc)+
    theme(
        legend.position = "none",
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        strip.text = element_blank(),
        panel.spacing.x = unit(2, "lines"),
        axis.text.y = element_text(face = 2)
    )+
    labs(
        x = "Life expectancy, years",
        y = NULL
    )

one_wo2020 <- last_plot()
ggsave("{wd}/out/fig-1-wo2020.pdf" %>% glue, one_wo2020,
       width = 9, height = 4, device = cairo_pdf)


