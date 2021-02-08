#===============================================================================
# 2021-02-02 -- ex2020
# Re-touch figures
#===============================================================================
# UPD  2021-02-08 ------------------------------


library(tidyverse)
library(magrittr)
library(prismatic)
library(ggdark)
library(patchwork)
library(paletteer)
library(hrbrthemes)
library(cowplot)
library(geofacet)

library(countrycode)



# plot 1 ex levels and changes --------------------------------------------

# get the lt estimates
df_lt <- read_rds("out/it_output.rds")

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


df_plot <- left_join(ex20, ex1519) %>%
    left_join(ex1519asb) %>%
    left_join(ids) %>%
    left_join(rank_e0f19) %>%
    left_join(rank_d0m20)

# save the data frame for figures 1 and 2
write_rds(df_plot, "out/df_plot.csv")


# figrue 1 -- absolute levels of life expectancy --------------------------

df_plot %>%
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
    scale_color_manual(values = c(2, 4))+
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

p_1 <- last_plot()
ggsave("out/fig-1.pdf", p_1, width = 9, height = 4, device = cairo_pdf)



# figure 2 -- changes in life expectancy ----------------------------------


df_plot %>%
    filter(age %in% c(0, 60, 80)) %>%
    drop_na(name) %>%
    mutate(name = fct_reorder(name, rank_d0m20)) %>%
    ggplot(aes(y = name))+
    geom_hline(yintercept = seq(2, 22, 2), size = 5, color = "#eaeaea")+
    geom_vline(xintercept = 0, size = 2, color = "#bababa")+
    geom_point(aes(x = ex_diff, color = sex, shape = sex), size = 2)+
    scale_color_manual(values = c(2, 4))+
    scale_shape_manual(values = c(16, 1))+
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

p_2 <- last_plot()
ggsave("out/fig-2.pdf", p_2, width = 9, height = 4, device = cairo_pdf)






# plot decomposition ------------------------------------------------------

df_dec <- read_rds("out/decomposition_results.rds")

