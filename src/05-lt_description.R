# General life-table analysis

# Init ------------------------------------------------------------

library(here); library(glue)
library(tidyverse)
library(patchwork)

# Constants -------------------------------------------------------

wd <- here()
cnst <- list()
cnst <- within(cnst, {
  regions_for_analysis = c(
    'AT', 'BE', 'BG', 'CH', 'CL', 'CZ', 'DE', 'DK', 'EE', 'ES', 'FI', 'FR',
    'GB-EAW', 'GB-NIR', 'GB-SCT',
    'HU', 'IL', 'LT', 'NL', 'PL', 'PT', 'SE', 'SI'
  )
  path_out = glue('{wd}/out')
  path_tmp = glue('{wd}/tmp')
})

dat <- list()
fig <- list()

# Data ------------------------------------------------------------

dat$lt_input <- readRDS(glue('{cnst$path_out}/lt_input.rds'))

dat$lt_input_sub <-
  dat$lt_input %>%
  filter(region_iso %in% cnst$regions_for_analysis)

# Function --------------------------------------------------------

source(glue('{wd}/cfg/fig_specs.R'))

# simple piecewise-exponential life-table
CalculateLifeTable <- 
  function (df, x, nx, Dx, Ex) {
    
    require(dplyr)
    
    df %>%
    transmute(
      x = {{x}},
      nx = {{nx}},
      mx = {{Dx}}/{{Ex}},
      px = exp(-mx*{{nx}}),
      qx = 1-px,
      lx = head(cumprod(c(1, px)), -1),
      dx = c(-diff(lx), tail(lx, 1)),
      Lx = dx/mx,
      Tx = rev(cumsum(rev(Lx))),
      ex = Tx/lx
    )  
    
  }

# Calculate annual life tables ------------------------------------

# life-tables by year
dat$lt <-
  dat$lt_input_sub %>%
  arrange(region_iso, sex, year, age_start) %>%
  group_by(region_iso, sex, year) %>%
  group_modify(~{
    CalculateLifeTable(.x, age_start, age_width, death_total, population_midyear)
  }) %>%
  ungroup()

# Analyse ex and mx changes ---------------------------------------

# average yearly change in mx, ex 2015 to 2019
dat$lt_avg_annual_change_pre2020 <-
  dat$lt %>%
  arrange(region_iso, sex, x, year) %>%
  filter(year %in% 2015:2019) %>%
  group_by(region_iso, sex, x) %>%
  # the na.rm statements here mean that an average is
  # calculated even if data is missing for some years
  # in the 2015:2019 period
  summarise(
    # arithmetic mean of ex annual change
    ex_avgdiff_pre2020 = mean(diff(ex), na.rm = TRUE),
    # geometric mean of mx relative annual change
    mx_avgratio_pre2020 = prod(mx[-1]/head(mx, -1), na.rm = TRUE)^(1/(n()-1))
  ) %>%
  ungroup()
# change in mx, ex 2020 to 2019
dat$lt_annual_change_2020 <-
  dat$lt %>%
  filter(year %in% c(2019, 2020)) %>%
  select(region_iso, sex, year, x, mx, ex) %>%
  pivot_wider(names_from = year, values_from = c(mx, ex)) %>%
  mutate(
    ex_diff_2020 = ex_2020 - ex_2019,
    mx_ratio_2020 = mx_2020 / mx_2019
  )
# join average pre 2020 and 2020 changes
dat$lt_annual_change <- 
  full_join(
    dat$lt_avg_annual_change_pre2020,
    dat$lt_annual_change_2020,
    by  = c('region_iso', 'sex', 'x')
  )

# plot changes in remaining e0, e60, e80
# from 2019 to 2020 by sex and region and compare with average
# annual change over 2015 to 2019 period
walk(c(0, 60, 80), ~{
  fig[[glue('e{.x}_change')]] <<-
    dat$lt_annual_change %>%
    filter(x == .x) %>%
    mutate(
      x = as.factor(x),
      region_iso = fct_reorder(region_iso, -ex_diff_2020)
    ) %>%
    ggplot(aes(x = region_iso, color = sex, fill = sex, group = sex)) +
    geom_col(
      aes(y = ex_diff_2020),
      position = position_dodge(width = 0.6), width = 0.4
    ) +
    geom_point(
      aes(x = region_iso, color = sex, y = ex_avgdiff_pre2020),
      position = position_dodge(width = 0.6)
    ) +
    geom_hline(yintercept = 0) +
    coord_flip(ylim = c(-2, 1)) +
    scale_color_manual(values = fig_spec$sex_colors) +
    scale_fill_manual(values = fig_spec$sex_colors) +
    guides(
      color = guide_legend(reverse = TRUE),
      fill = guide_legend(reverse = TRUE)
    ) +
    labs(
      subtitle = glue('at age {.x}'),
      y = NULL,
      x = NULL
    ) +
    fig_spec$MyGGplotTheme(grid = 'xy', scaler = 0.8) +
    theme(legend.title = element_blank())
})

fig$ex_change <-
  fig$e0_change + fig$e60_change + fig$e80_change +
  plot_layout(guides = 'collect') +
  plot_annotation(
    title = 'Annual change in years of remaining life-expectancy 2019 to 2020',
    subtitle = 'Points mark the average annual change in life-expectancy 2015 to 2019'
  )
fig$ex_change
fig_spec$ExportFigure(fig$ex_change, path = cnst$path_out)

# compare hazards
dat$lt_annual_change %>%
  filter(x < 100) %>%
  ggplot(aes(x = x, color = sex)) +
  geom_line(aes(y = mx_2020)) +
  geom_line(aes(y = mx_2019), linetype = 2) +
  facet_wrap(~region_iso) +
  scale_y_log10() +
  scale_color_manual(values = fig_spec$sex_colors) +
  fig_spec$MyGGplotTheme(panel_border = TRUE) +
  labs(x = 'Age', y = 'Deaths per person-year of exposure',
       title = 'Hazard rates 2020 compared with 2019 (dashed)')

# compare densities
dat$lt %>%
  filter(x < 100, x > 70, sex == 'Male') %>%
  mutate(is2020 = ifelse(year == 2020, TRUE, FALSE)) %>%
  ggplot(aes(x = x, group = year, color = is2020)) +
  geom_line(aes(y = dx)) +
  facet_wrap(~region_iso) +
  scale_color_manual(values = c(`TRUE` = 'red', `FALSE` = 'grey')) +
  fig_spec$MyGGplotTheme(panel_border = TRUE) +
  labs(x = 'Age', y = 'density',
       title = 'Male densities of death 2020 (red) compared with previous years (grey)')

# Compare our ex estimates with wpp estimates ---------------------

walk(c(0, 60, 80), ~{
  fig[[glue('e{.x}_consistency_check')]] <<-
    dat$lt %>%
    filter(x == .x) %>%
    ggplot(aes(x = year, y = ex, color = sex)) +
    geom_point() +
    geom_segment(
      aes(x = 2015, xend = 2019, y = ex_wpp_estimate, yend = ex_wpp_estimate),
      data =
        dat$lt_input_sub %>%
        filter(age_start == .x, year == 2018)
    ) +
    facet_wrap(~region_iso, scales = 'free_y') +
    scale_x_continuous(
      breaks = 2015:2020,
      labels = c('', '2016', '', '2018', '', '2020')
    ) +
    scale_color_manual(values = fig_spec$sex_colors) +
    fig_spec$MyGGplotTheme(panel_border = TRUE, grid = 'xy', scaler = 0.8) +
    labs(
      title = glue('Estimated yearly life expectancy at age {.x} compared with WPP 5 year average estimates'),
      y = glue('e{.x}')
    )
})

fig_spec$ExportFigure(fig$e0_consistency_check, path = cnst$path_tmp)
fig_spec$ExportFigure(fig$e80_consistency_check, path = cnst$path_tmp)
