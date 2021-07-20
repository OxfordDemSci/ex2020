# General life-table analysis

# Init ------------------------------------------------------------

set.seed(1987)

library(here); library(glue)
library(tidyverse); library(yaml)
library(patchwork)
library(gt); library(openxlsx)

# Constants -------------------------------------------------------

wd <- here()
cnst <- list()
config <- read_yaml(glue('{wd}/cfg/config.yaml'))
region_meta <- read_csv(glue('{wd}/cfg/region_metadata.csv'), na = '.')
cnst <- within(cnst, {
  regions_for_analysis = config$regions_for_all_cause_analysis
  path_out = glue('{wd}/out')
  path_tmp = glue('{wd}/tmp')
  # number of Poisson life-table replicates
  n_sim = 500
})

dat <- list()
fig <- list()
tab <- list()

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
        Lx = ifelse(mx==0, lx*nx, dx/mx),
        Tx = rev(cumsum(rev(Lx))),
        ex = Tx/lx
      )

  }

# Data ------------------------------------------------------------

# input data for life-table calculation
# harmonized death counts and population exposures with open age group 100+
dat$lt_input_100 <- readRDS(glue('{cnst$path_out}/lt_input.rds'))

# Create open age group 85+ ---------------------------------------

dat$lt_input_85 <-
  dat$lt_input_100 %>%
  # for each life-table input stratum create age group 85+
  group_by(region_iso, sex, year) %>%
  group_modify(~{
    input_sorted <- arrange(.x, age_start)
    lt85 <- filter(input_sorted, age_start <= 85)
    lt85p <- filter(input_sorted, age_start > 85)

    lt85[nrow(lt85), 'age_width'] <- Inf
    lt85[nrow(lt85), 'death_total'] <- sum(lt85p$death_total)
    lt85[nrow(lt85), 'population_midyear'] <- sum(lt85p$population_midyear)
    lt85[nrow(lt85), 'population_py'] <- sum(lt85p$population_py)
    lt85[nrow(lt85), 'death_covid'] <- sum(lt85p$death_covid)

    return(lt85)
  }) %>%
  ungroup() %>%
  # harmonize order of variables
  select(all_of(names(dat$lt_input_100)))

# Subset to countries of interest ---------------------------------

dat$lt_input_85_sub <-
  dat$lt_input_85 %>%
  filter(region_iso %in% cnst$regions_for_analysis)

dat$lt_input_100_sub <-
  dat$lt_input_100 %>%
  filter(region_iso %in% cnst$regions_for_analysis)

# Calculate annual life tables ------------------------------------

# life-tables by region, sex, and year

# open age_group 100+
dat$lt_100 <-
  dat$lt_input_100_sub %>%
  arrange(region_iso, sex, year, age_start) %>%
  group_by(region_iso, sex, year) %>%
  group_modify(~{
    CalculateLifeTable(.x, age_start, age_width, death_total, population_py)
  }) %>%
  ungroup()

# open age group 85+
dat$lt_85 <-
  dat$lt_input_85_sub %>%
  arrange(region_iso, sex, year, age_start) %>%
  group_by(region_iso, sex, year) %>%
  group_modify(~{
    CalculateLifeTable(.x, age_start, age_width, death_total, population_py)
  }) %>%
  ungroup()

# Create Poisson life-table replicates ----------------------------

# create life table replicates by region, sex, and year
# based on repeatedly sampling death counts from a Poisson
# distribution with mean equal to estimated mean from PCLM
# decomposition
dat$lt_85_sim <-
  dat$lt_input_85_sub %>%
  expand_grid(id_sim = 1:cnst$n_sim) %>%
  group_by(region_iso, sex, year, age_start) %>%
  mutate(death_total_sim = rpois(cnst$n_sim, death_total)) %>%
  arrange(region_iso, sex, year, age_start) %>%
  group_by(id_sim, region_iso, sex, year) %>%
  group_modify(~{
    CalculateLifeTable(.x, age_start, age_width, death_total_sim, population_py)
  }) %>%
  ungroup()

# Assemble table with ex statistics -------------------------------

# central estimates of life-expectancy, annual life-expectancy difference,
# and average annual life-expectancy difference 2015 to 2019
dat$lt_ex_diff_mean <-
  dat$lt_85 %>%
  filter(year %in% c(2015:2020)) %>%
  select(region_iso, sex, year, x, mx, ex) %>%
  arrange(region_iso, sex, x, year) %>%
  group_by(region_iso, sex, x) %>%
  mutate(
    # annual ex difference (delta ex_y = ex_{y+1} - ex_y)
    ex_diff = c(diff(ex), NA),
    # average annual ex difference 2015 to 2019
    ex_avgdiff = mean(ifelse(year %in% 2015:2018, ex_diff, NA), na.rm = TRUE)
  ) %>%
  ungroup()

# 95% uncertainty intervals around the central estimates
dat$lt_ex_diff_ci <-
  dat$lt_85_sim %>%
  filter(year %in% c(2015:2020)) %>%
  select(id_sim, region_iso, sex, year, x, mx, ex) %>%
  arrange(id_sim, region_iso, sex, x, year) %>%
  group_by(id_sim, region_iso, sex, x) %>%
  mutate(
    ex_diff = c(diff(ex), NA),
    ex_avgdiff = mean(ifelse(year %in% 2015:2018, ex_diff, NA), na.rm = TRUE)
  ) %>%
  group_by(region_iso, sex, x, year) %>%
  summarise(
    ex_q025 = quantile(ex, 0.025, na.rm = TRUE),
    ex_q975 = quantile(ex, 0.975, na.rm = TRUE),
    ex_diff_q025 = quantile(ex_diff, 0.025, na.rm = TRUE),
    ex_diff_q975 = quantile(ex_diff, 0.975, na.rm = TRUE),
    ex_avgdiff_q025 = quantile(ex_avgdiff, 0.025, na.rm = TRUE),
    ex_avgdiff_q975 = quantile(ex_avgdiff, 0.975, na.rm = TRUE)
  ) %>%
  ungroup()

# assemble all the ex statistics in a single table
# for further computation
dat$lt_ex_diff_long <-
  left_join(
    dat$lt_ex_diff_mean,
    dat$lt_ex_diff_ci
  ) %>%
  mutate(
    region_name = factor(
      region_iso,
      region_meta$region_code_iso3166_2,
      region_meta$region_name
    )
  )

# this will be exported for use in further computation
dat$lt_ex_diff <-
  dat$lt_ex_diff_long %>%
  select(-mx, -ex_q025, -ex_q975) %>%
  pivot_wider(
    names_from = year,
    values_from = c(ex, ex_diff, ex_diff_q025, ex_diff_q975,
                    ex_avgdiff, ex_avgdiff_q025, ex_avgdiff_q975)
  ) %>%
  select(
    region_iso, region_name, sex, x,
    ex_2015:ex_2020,
    ex_diff_1920 = ex_diff_2019,
    ex_diff_1920_q025 = ex_diff_q025_2019,
    ex_diff_1920_q975 = ex_diff_q975_2019,
    ex_avgdiff_pre2020 = ex_avgdiff_2020,
    ex_avgdiff_pre2020_q025 = ex_avgdiff_q025_2020,
    ex_avgdiff_pre2020_q975 = ex_avgdiff_q975_2020
  )

# format ex statistics for export as excel table
walk(c(0, 60), ~{
  tab[[glue('tab_e{.x}_diff')]] <<-
    dat$lt_ex_diff_long %>%
    mutate(across(
      starts_with(c('ex_diff', 'ex_avgdiff')),
      ~formatC(., digits = 2, format = 'f', flag = '+')
    )) %>%
    mutate(across(
      c(ex, ex_q025, ex_q975),
      ~formatC(., digits = 2, format = 'f')
    )) %>%
    mutate(
      ex =
        paste0(ex, ' (', ex_q025, ',', ex_q975, ')'),
      ex_diff =
        paste0(ex_diff, ' (', ex_diff_q025, ',', ex_diff_q975, ')'),
      ex_avgdiff =
        paste0(ex_avgdiff, ' (', ex_avgdiff_q025, ',', ex_avgdiff_q975, ')')
    ) %>%
    select(region_iso, region_name, sex, year, x, ex, ex_diff, ex_avgdiff) %>%
    pivot_wider(names_from = year, values_from = c(ex, ex_diff, ex_avgdiff)) %>%
    select(region_iso, region_name, sex, x, ex_2015:ex_2020,
           ex_diff_1920 = ex_diff_2019,
           ex_avgdiff_pre2020 = ex_avgdiff_2020) %>%
    filter(x == .x) %>%
    select(-x) %>%
    pivot_wider(
      id_cols = c(region_iso, region_name, sex),
      names_from = sex,
      values_from = c(ex_2015:ex_2020, ex_diff_1920, ex_avgdiff_pre2020)
    )
})

# Plot ex changes -------------------------------------------------

# plot changes in remaining e0, e60
# from 2019 to 2020 by sex and region and compare with average
# annual change over 2015 to 2019 period
walk(c(0, 60), ~{
  fig[[glue('e{.x}_change')]] <<-
    dat$lt_ex_diff %>%
    filter(x == .x) %>%
    mutate(
      x = as.factor(x),
      region_name = fct_reorder(region_name, -ex_diff_1920)
    ) %>%
    ggplot(aes(x = region_name, color = sex, fill = sex, group = sex)) +
    geom_col(aes(y = 0)) + # workaround so tht geom_vline works with discrete scale
    geom_vline(
      xintercept = seq(2, length(cnst$regions_for_analysis), 2),
      size = 3, color = '#eaeaea'
    ) +
    geom_pointrange(
      aes(
        color = sex,
        ymin = ex_diff_1920_q025,
        y = ex_diff_1920,
        ymax = ex_diff_1920_q975
      ),
      fatten = 0.3, size = 0.2,
      position = position_dodge(width = 0.6)
    ) +
    geom_pointrange(
      aes(
        color = sex,
        ymin = ex_avgdiff_pre2020_q025,
        y = ex_avgdiff_pre2020,
        ymax = ex_avgdiff_pre2020_q975
      ),
      fatten = 0.3, size = 0.2, shape = 4,
      position = position_dodge(width = 0.6)
    ) +
    geom_hline(yintercept = 0) +
    coord_flip(ylim = c(-2.2, 1)) +
    scale_y_continuous(
      breaks = seq(-2, 2, 0.5),
      labels = c('-2 years', '', '-1', '', '0', '', '+1', '', '+2 years gained')
    ) +
    scale_fill_manual(values = fig_spec$sex_colors) +
    scale_color_manual(values = fig_spec$sex_colors) +
    guides(
      color = guide_legend(reverse = TRUE),
      fill = guide_legend(reverse = TRUE)
    ) +
    labs(
      subtitle = glue('at age {.x}'),
      y = NULL,
      x = NULL
    ) +
    fig_spec$MyGGplotTheme(grid = 'x', scaler = 0.8, show_legend = FALSE)
})

fig$ex_change <-
  fig$e0_change +
  annotate('text', x = 2, y = -1.5, label = 'Female', size = 3,
           color = fig_spec$sex_colors['Female'], hjust = 1) +
  annotate('text', x = 3, y = -1.5, label = 'Male', size = 3,
           color = fig_spec$sex_colors['Male'], hjust = 1) +
  fig$e60_change +
  plot_layout(guides = 'collect') +
  plot_annotation(
    title = 'Annual change in years of remaining life-expectancy 2019 to 2020',
    subtitle = '95% CIs via Poisson sampling of expected death counts\nCrosses mark the average annual change in life-expectancy 2015 to 2019'
  )
fig$ex_change

# Plot mx changes -------------------------------------------------

dat$mx_change <-
  dat$lt_85_sim %>%
  mutate(
    age_group = cut(x, c(0, 40, 60, 70, Inf), right = FALSE)
  ) %>%
  group_by(id_sim, region_iso, sex, year, age_group) %>%
  summarise(
    mx = sum(dx)/sum(Lx)
  ) %>%
  pivot_wider(names_from = year, values_from = mx) %>%
  mutate(diff = `2020`/`2019`) %>%
  group_by(region_iso, sex, age_group) %>%
  summarise(
    mean_diff = mean(diff),
    q025_diff = quantile(diff, 0.025),
    q975_diff = quantile(diff, 0.975),
    sig_elevated = ifelse(q025_diff > 1, TRUE, FALSE)
  )

fig$mx_change <-
  dat$mx_change %>%
  ggplot(aes(x = age_group, color = sex, group = sex)) +
  geom_hline(yintercept = 1, color = 'grey') +
  geom_pointrange(aes(
    y = mean_diff,ymin = q025_diff, ymax = q975_diff,
    alpha = sig_elevated
  ), fatten = 1, position = position_dodge(width = 0.5)) +
  scale_y_log10(breaks = c(0.8, 1, 1.2),
                labels = c('.8', '1', '1.2')) +
  scale_color_manual(values = fig_spec$sex_colors) +
  facet_wrap(~region_iso) +
  fig_spec$MyGGplotTheme(scaler = 0.8, panel_border = TRUE) +
  coord_flip() +
  scale_alpha_manual(values = c(0.2, 1)) +
  labs(
    title = 'Ratio of life-table death rates 2020 to 2019',
    subtitle = '95% CIs via Poisson simulation of raw death counts',
    x = 'Age group', y = 'Ratio 2020 to 2019'
  )
fig$mx_change

# Compare our ex estimates with wpp estimates ---------------------

walk(c(0, 60), ~{
  fig[[glue('e{.x}_consistency_check')]] <<-
    bind_rows(
      `85+` = filter(dat$lt_85, x == .x),
      `100+` = filter(dat$lt_100, x == .x),
      .id = 'open_age_group'
    ) %>%
    ggplot(aes(x = year, y = ex, color = sex)) +
    geom_segment(
      aes(x = 2015, xend = 2019, y = ex_wpp_estimate,
          yend = ex_wpp_estimate),
      size = 1, alpha = 0.5,
      data =
        dat$lt_input_85_sub %>%
        filter(age_start == .x, year == 2018)
    ) +
    geom_point(aes(shape = open_age_group)) +
    geom_line(
      aes(y = ex_hmd_estimate),
      data =
        dat$lt_input_85_sub %>%
        filter(age_start == .x)
    ) +
    facet_wrap(~region_iso, scales = 'free_y') +
    scale_x_continuous(
      breaks = 2015:2020,
      labels = c('', '2016', '', '2018', '', '2020')
    ) +
    scale_color_manual(values = fig_spec$sex_colors) +
    scale_shape_manual(values = c(`85+` = 1, `100+` = 3)) +
    fig_spec$MyGGplotTheme(panel_border = TRUE, grid = 'xy', scaler = 0.8) +
    labs(
      title = glue('Estimated yearly life expectancy at age {.x} compared with HMD (thin line) and WPP (bold line) 5 year average estimates'),
      y = glue('e{.x}'),
      shape = 'Open age group',
      color = 'Sex'
    )
})
fig$e0_consistency_check
fig$e60_consistency_check

# Export ----------------------------------------------------------

# save the regrouped life table input data
saveRDS(dat$lt_input_85, file = glue('{wd}/out/lt_input_85.rds'))

# save the all cause life tables
saveRDS(dat$lt_85, file = glue('{wd}/out/lt_output_85.rds'))
saveRDS(dat$lt_100, file = glue('{wd}/out/lt_output_100.rds'))

# save table of ex changes and CIs
saveRDS(dat$lt_ex_diff, file = glue('{wd}/out/lt_ex_diff.rds'))

# save formatted table of ex changes and CIs
tab$tab_ex_diff_wb <- createWorkbook()
addWorksheet(tab$tab_ex_diff_wb, sheetName = 'e0')
addWorksheet(tab$tab_ex_diff_wb, sheetName = 'e60')
writeData(tab$tab_ex_diff_wb, sheet = 'e0', tab$tab_e0_diff)
writeData(tab$tab_ex_diff_wb, sheet = 'e60', tab$tab_e60_diff)
saveWorkbook(
  tab$tab_ex_diff, glue('{wd}/out/tab_ex_diff.xlsx'),
  overwrite = TRUE
)

# save e0, e60 change figure
fig_spec$ExportFigure(fig$ex_change, path = cnst$path_out)

# save the life-table death rate change
fig_spec$ExportFigure(fig$mx_change, path = cnst$path_out)

# save the ex consistency checks
fig_spec$ExportFigure(fig$e0_consistency_check, path = cnst$path_out)
fig_spec$ExportFigure(fig$e60_consistency_check, path = cnst$path_out)
