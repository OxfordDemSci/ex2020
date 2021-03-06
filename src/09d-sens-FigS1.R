

library(tidyverse)
library(magrittr)
library(prismatic)
library(patchwork)
library(hrbrthemes)
library(cowplot)
library(here); wd <- here()
library(glue)
library(countrycode)


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
rank_d0m20 <- ex20 %>%
  filter(age == 0, sex == "Male") %>%
  arrange(ex_diff) %>%
  transmute(code, rank_d0m20 = seq_along(code))


df_dec <- read_rds("{wd}/out/decomposition_results.rds" %>% glue)


df_dec_age <- df_dec$decomposition_results_by_age %>%
  mutate(age = x %>% cut(c(seq(0,80,10), Inf), right = FALSE),
    period = year.final %>% cut(c(2015, 2019, 2020)) %>%
      lvls_revalue(c("2015-19", "2019-20"))
  ) %>%
  group_by(code = region_iso, sex,age,  period) %>%
  summarise(ctb = contribution %>% sum(na.rm = T)) %>%
  ungroup() %>%
  left_join(ids) %>%
  left_join(rank_d0m20) %>%
  drop_na(name) %>%
  mutate(
    name = fct_reorder(name, rank_d0m20),
    period = period %>% fct_rev
  )




df_dec_age %>%
  filter(sex == "Female") %>%
  ggplot(aes(ctb, age, fill = period))+
  geom_col()+
  geom_vline(xintercept = 0, size = .25, color = "#dfff00")+
  facet_wrap(~name, ncol = 7, dir = "v")+
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
    strip.text = element_text(face = 2),
    legend.position = c(.93, .1)
  )+
  labs(
    fill = "FEMALES",
    x = "",
    y = "Age groups"
  )

three_f <- last_plot()


df_dec_age %>%
  filter(sex == "Male") %>%
  ggplot(aes(ctb, age, fill = period))+
  geom_col()+
  geom_vline(xintercept = 0, size = .25, color = "#dfff00")+
  facet_wrap(~name, ncol = 7, dir = "v")+
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
    strip.text = element_text(face = 2),
    legend.position = c(.93, .1),
  )+
  labs(
    fill = "MALES",
    x = "Losses|Gains in life expectancy at birth, years",
    y = "Age groups"
  )

three_m <- last_plot()

three <- three_f / three_m

ggsave("{wd}/out/sens/fig-S1.pdf" %>% glue, three, width = 10, height = 11, device = cairo_pdf)

