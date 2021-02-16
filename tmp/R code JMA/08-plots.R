#===============================================================================
# 2021-02-02 -- ex2020
# Re-touch figures
#===============================================================================
# UPD  2021-02-09 ------------------------------


library(tidyverse)
library(magrittr)
library(prismatic)
library(patchwork)
library(hrbrthemes)
library(cowplot)
library(here); wd <- here()
library(glue)

df_ex <- read_rds("{wd}/tmp/df_ex.rds" %>% glue)

# figrue 1 -- absolute levels of life expectancy --------------------------

df_ex %>%
    mutate(name = fct_reorder(name, rank_e0f19)) %>%
    filter(age %in% c(0, 60, 80)) %>%
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
    geom_hline(yintercept = seq(2, 22, 2), size = 5, color = "#eaeaea")+
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
ggsave("{wd}/tmp/fig-1.pdf" %>% glue, one,
       width = 9, height = 4, device = cairo_pdf)



# figure 2 -- changes in life expectancy ----------------------------------

df_ex %>%
    filter(age %in% c(0, 60, 80)) %>%
    drop_na(name) %>%
    mutate(name = fct_reorder(name, rank_d0m20)) %>%
    ggplot(aes(y = name))+
    geom_hline(yintercept = seq(2, 22, 2), size = 5, color = "#eaeaea")+
    geom_vline(xintercept = 0, size = 2, color = "#bababa")+
    geom_point(aes(x = ex_diff, color = sex, shape = sex), size = 2)+
    scale_color_manual(values = c("#B5223BFF", "#64B6EEFF"))+
    scale_shape_manual(values = c(1, 16))+
    geom_point(aes(x = ex_diff_1519/4, color = sex), shape = 124, size = 3)+
    scale_y_discrete(position = "right")+
    scale_x_continuous(breaks = seq(-1.5, 2.5, .5), expand = c(0,0))+
    coord_cartesian(xlim = c(-1.8, .999))+
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
    )+
    geom_text(
        data = . %>%
            mutate(
                hj = case_when(sex == "Female" ~ 1, TRUE ~ 0),
                nj = case_when(sex == "Female" ~ 1, TRUE ~ 1.05)
            ),
        aes(x = .77*nj, hjust = hj, color = sex,
            label = ex_diff_1519 %>% round(1)),
        size = 3, family = font_rc
    )

two <- last_plot()
ggsave("{wd}/tmp/fig-2.pdf" %>% glue, two, width = 9, height = 4, device = cairo_pdf)






# plot decomposition ------------------------------------------------------

df_dec_age <- read_rds("{wd}/tmp/df_dec_age.rds" %>% glue)
df_dec_age_cause <- read_rds("{wd}/tmp/df_dec_age_cause.rds" %>% glue)


# figure 3 -- contributions by age groups ---------------------------------

df_dec_age %>%
    filter(sex == "Female") %>%
    ggplot(aes(ctb, age3, fill = period))+
    geom_col()+
    geom_vline(xintercept = 0, size = .25, color = "#18ffff")+
    facet_wrap(~name, ncol = 6, dir = "v")+
    scale_fill_manual(
        values = c("#e91e63", "#38006b"),
        guide  = guide_legend(ncol = 1, reverse = TRUE)
    )+
    coord_cartesian(xlim = c(-1, 1.5), expand = F)+
    scale_x_continuous(
        breaks = seq(-1, 1, .5),
        labels = c("-1", ".5", 0, ".5", 1)
    )+
    theme_minimal(base_family = font_rc)+
    theme(
        panel.grid.minor.y = element_blank(),
        panel.spacing = unit(.5, "lines"),
        legend.position = c(.92, .06),
        legend.background = element_rect(color = "#18ffff", size = 2),
        plot.background = element_rect(color = "#18ffff", size = 1)
    )+
    labs(
        fill = "FEMALES",
        x = "Losses|Gains in life expectancy at birth, years",
        y = "Age groups"
    )

three_f <- last_plot()


df_dec_age %>%
    filter(sex == "Male") %>%
    ggplot(aes(ctb, age3, fill = period))+
    geom_col()+
    geom_vline(xintercept = 0, size = .25, color = "#dfff00")+
    facet_wrap(~name, ncol = 6, dir = "v")+
    scale_fill_manual(
        values = c("#3FB3F7FF", "#003737FF"),
        guide  = guide_legend(ncol = 1, reverse = TRUE)
    )+
    coord_cartesian(xlim = c(-1, 1.5), expand = F)+
    scale_x_continuous(
        breaks = seq(-1, 1, .5),
        labels = c("-1", ".5", 0, ".5", 1)
    )+
    theme_minimal(base_family = font_rc)+
    theme(
        panel.grid.minor.y = element_blank(),
        panel.spacing = unit(.5, "lines"),
        legend.position = c(.92, .06),
        legend.background = element_rect(color = "#dfff00", size = 2),
        plot.background = element_rect(color = "#dfff00", size = 1)
    )+
    labs(
        fill = "MALES",
        x = "Losses|Gains in life expectancy at birth, years",
        y = "Age groups"
    )

three_m <- last_plot()

three <- three_f / three_m

ggsave("{wd}/tmp/fig-3.pdf" %>% glue, three, width = 10, height = 8, device = cairo_pdf)



# figure 4 -- split 2020 into covid and non-covid -------------------------

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

df_dec_age_cause %>%
    left_join(ids) %>%
    left_join(rank_d0m20) %>%
    drop_na(name) %>%
    mutate(name = fct_reorder(name, rank_d0m20)) %>%
    filter(period_cause == "2019-20 COVID") %>%
    ggplot(aes(ctb, name, fill = sex))+
    #geom_hline(yintercept = 2, size = 5, color = "#eaeaea")+
    #geom_vline(xintercept = 0, size = .25, color = "#dfff00")+
    geom_bar(stat = 'identity')+
    #scale_shape_manual(values = c(1, 16))+
    scale_fill_manual(values = c("#B5223BFF", "#64B6EEFF"))+
    scale_y_discrete(position = "right")+
    #scale_x_continuous(breaks = c(-1, -.5, 0))+
    facet_wrap(~sex, ncol = 2, dir = "v")+
    theme_minimal(base_family = font_rc)+
    theme(
        legend.position = "none",
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.spacing.x = unit(2, "lines")
    )+
    labs(
        x = "Losses|Gains in life expectancy at birth, years",
        y = NULL
    )

four <- last_plot()

ggsave("{wd}/tmp/fig-4.pdf" %>% glue, four, width = 9, height = 4, device = cairo_pdf)


# ggsave("tmp/fig-1.png", one, width = 9, height = 4, type = "cairo")
# ggsave("tmp/fig-2.png", two, width = 9, height = 4, type = "cairo")
# ggsave("tmp/fig-3.png", three, width = 10, height = 8, type = "cairo")
# ggsave("tmp/fig-4.png", four, width = 10, height = 8, type = "cairo")


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
ggsave("{wd}/tmp/fig-1-wo2020.pdf" %>% glue, one_wo2020,
       width = 9, height = 4, device = cairo_pdf)


