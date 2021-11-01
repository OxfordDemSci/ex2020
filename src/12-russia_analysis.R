# General life-table analysis

# Init ------------------------------------------------------------

set.seed(1987)

library(here); library(glue)
library(tidyverse); library(yaml)
library(patchwork)
library(gt); library(openxlsx)
library(prismatic)

library(showtext)
font_add_google('Roboto', 'roboto')
font_add_google('Roboto Condensed', 'roboto_condensed')
showtext_auto()

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
  # prediction interval width
  ci_width = 0.95
  ci_lo = (1-ci_width)/2
  ci_hi = ci_lo+ci_width
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

# decomposition results
dat$decomp <- readRDS(glue('{wd}/out/decomposition_results.rds'))

# hmd data
dat$hmd_lifetab <- readRDS(glue('{wd}/dat/hmdhfd/lt-1x1.rds'))

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

# Calculate annual life tables ------------------------------------

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

# Assemble table with ex sex differences --------------------------

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

# central estimates of life-expectancy, annual life-expectancy difference,
# and average annual life-expectancy difference 2015 to 2019
dat$lt_ex_sexdiff_mean <-
  dat$lt_ex_diff_mean %>%
  pivot_wider(
    id_cols = c(region_iso, year, x),
    names_from = c(sex),
    values_from = c(ex, ex_diff, ex_avgdiff)
  ) %>%
  mutate(
    ex_sexdiff = ex_Female - ex_Male,
    ex_diff_sexdiff = ex_diff_Female - ex_diff_Male,
    ex_avgdiff_sexdiff = ex_avgdiff_Female - ex_avgdiff_Male
  )

# 95% uncertainty intervals around the central estimates
dat$lt_ex_sexdiff_ci <-
  dat$lt_85_sim %>%
  filter(year %in% c(2015:2020)) %>%
  select(id_sim, region_iso, sex, year, x, mx, ex) %>%
  arrange(id_sim, region_iso, sex, x, year) %>%
  group_by(id_sim, region_iso, sex, x) %>%
  mutate(
    ex_diff = c(diff(ex), NA),
    ex_avgdiff = mean(ifelse(year %in% 2015:2018, ex_diff, NA), na.rm = TRUE)
  ) %>%
  pivot_wider(
    id_cols = c(id_sim, region_iso, year, x),
    names_from = c(sex),
    values_from = c(ex, ex_diff, ex_avgdiff)
  ) %>%
  mutate(
    ex_sexdiff = ex_Female - ex_Male,
    ex_diff_sexdiff = ex_diff_Female - ex_diff_Male,
    ex_avgdiff_sexdiff = ex_avgdiff_Female - ex_avgdiff_Male
  ) %>%
  group_by(region_iso, x, year) %>%
  summarise(
    ex_Female_q025 = quantile(ex_Female, cnst$ci_lo, na.rm = TRUE),
    ex_Female_q975 = quantile(ex_Female, cnst$ci_hi, na.rm = TRUE),
    ex_diff_Female_q025 = quantile(ex_diff_Female, cnst$ci_lo, na.rm = TRUE),
    ex_diff_Female_q975 = quantile(ex_diff_Female, cnst$ci_hi, na.rm = TRUE),
    ex_diff_Male_q025 = quantile(ex_diff_Male, cnst$ci_lo, na.rm = TRUE),
    ex_diff_Male_q975 = quantile(ex_diff_Male, cnst$ci_hi, na.rm = TRUE),
    ex_sexdiff_q025 = quantile(ex_sexdiff, cnst$ci_lo, na.rm = TRUE),
    ex_sexdiff_q975 = quantile(ex_sexdiff, cnst$ci_hi, na.rm = TRUE),
    ex_diff_sexdiff_q025 = quantile(ex_diff_sexdiff, cnst$ci_lo, na.rm = TRUE),
    ex_diff_sexdiff_q975 = quantile(ex_diff_sexdiff, cnst$ci_hi, na.rm = TRUE),
    ex_avgdiff_sexdiff_q025 = quantile(ex_avgdiff_sexdiff, cnst$ci_lo, na.rm = TRUE),
    ex_avgdiff_sexdiff_q975 = quantile(ex_avgdiff_sexdiff, cnst$ci_hi, na.rm = TRUE),
  ) %>%
  ungroup()

dat$lt_ex_sexdiff <-
  left_join(
    dat$lt_ex_sexdiff_mean, dat$lt_ex_sexdiff_ci
  ) %>%
  left_join(region_meta, by = c('region_iso' = 'region_code_iso3166_2')) %>%
  mutate(
    region_name = ifelse(
      region_name == 'England and Wales',
      '\nEngland &\nWales',
      region_name
    )
  )

# Plot sexdiff ----------------------------------------------------

fig$sexdiff$cnst <- list(); fig$sexdiff$cnst <- within(fig$sexdiff$cnst, {

  font_size_annotation = 2
  font_size_annotation_numeric = font_size_annotation*0.8
  lighten_uncertainty = 0.8
  width_uncertainty = 2.5
  font_family_label = 'roboto'
  font_family_theme = 'roboto_condensed'
  font_size_theme = 6
  font_color_country = 'grey50'
  color_positive = '#005784'
  color_positive_light =
    clr_mix(color_positive, mix_in = '#FFFFFF', ratio = lighten_uncertainty)
  color_positive_light2 =
    clr_mix(color_positive, mix_in = '#FFFFFF', ratio = lighten_uncertainty*0.5)
  color_negative = '#B70D0D'
  color_negative_light =
    clr_mix(color_negative, mix_in = '#FFFFFF', ratio = lighten_uncertainty)
  color_negative_light2 =
    clr_mix(color_negative, mix_in = '#FFFFFF', ratio = lighten_uncertainty*0.5)

})

fig$sexdiff$func$FormatLabel <- function (x) {
  lab <- formatC(x, digits = 2, format = 'f', flag = '+')
  str_replace_all(lab, '(^[+-])(0)(.+)', '\\1\\3')
}

fig$sexdiff$data <-
  dat$lt_ex_sexdiff %>%
  filter(year == 2019, x == 0, ex_diff_Female < 0, ex_diff_Male < 0) %>%
  mutate(
    region_pos = as.integer(fct_reorder(region_iso, ex_diff_sexdiff))
  )

fig$sexdiff$plot <-
  fig$sexdiff$data %>%
  ggplot() +
  # grid
  geom_hline(yintercept = 0, color = 'grey', size = 1) +
  # central estimates & uncertainty
  geom_linerange(
    aes(
      x = region_pos,
      ymin = ex_diff_sexdiff_q025,
      ymax = ex_diff_sexdiff_q975,
      color = as.character(sign(ex_diff_sexdiff)+3)
    ),
    size = fig$sexdiff$cnst$width_uncertainty
  ) +
  geom_point(
    aes(
      x = region_pos,
      y = ex_diff_sexdiff,
      color = as.character(sign(ex_diff_sexdiff))
    )
  ) +
  # numeric labels
  # white offset
  geom_text(
    aes(
      x = region_pos,
      y = ex_diff_sexdiff_q025,
      label = fig$sexdiff$func$FormatLabel(ex_diff_sexdiff_q025),
    ),
    family = fig$sexdiff$cnst$font_family_label, color = 'white',
    position = position_nudge(y = -0.055, x = -0.005),
    size = fig$sexdiff$cnst$font_size_annotation_numeric
  ) +
  geom_text(
    aes(
      x = region_pos,
      y = ex_diff_sexdiff_q975,
      label = fig$sexdiff$func$FormatLabel(ex_diff_sexdiff_q975)
    ),
    color = 'white',
    family = fig$sexdiff$cnst$font_family_label,
    position = position_nudge(y = 0.055, x = -0.005),
    size = fig$sexdiff$cnst$font_size_annotation_numeric
  ) +
  geom_text(
    aes(
      x = region_pos,
      y = ex_diff_sexdiff,
      label = fig$sexdiff$func$FormatLabel(ex_diff_sexdiff)
    ),
    color = 'white',
    family = fig$sexdiff$cnst$font_family_label,
    position = position_nudge(x = 0.42, y = -0.005),
    size = fig$sexdiff$cnst$font_size_annotation_numeric
  ) +
  # colored text
  geom_text(
    aes(
      x = region_pos,
      y = ex_diff_sexdiff_q025,
      label = fig$sexdiff$func$FormatLabel(ex_diff_sexdiff_q025),
      color = as.character(sign(ex_diff_sexdiff)+6)
    ),
    family = fig$sexdiff$cnst$font_family_label,
    position = position_nudge(y = -0.05),
    size = fig$sexdiff$cnst$font_size_annotation_numeric
  ) +
  geom_text(
    aes(
      x = region_pos,
      y = ex_diff_sexdiff_q975,
      label = fig$sexdiff$func$FormatLabel(ex_diff_sexdiff_q975),
      color = as.character(sign(ex_diff_sexdiff)+6)
    ),
    family = fig$sexdiff$cnst$font_family_label,
    position = position_nudge(y = 0.05),
    size = fig$sexdiff$cnst$font_size_annotation_numeric
  ) +
  geom_text(
    aes(
      x = region_pos,
      y = ex_diff_sexdiff,
      label = fig$sexdiff$func$FormatLabel(ex_diff_sexdiff),
      color = as.character(sign(ex_diff_sexdiff)+6)
    ),
    family = fig$sexdiff$cnst$font_family_label,
    position = position_nudge(x = 0.45),
    size = fig$sexdiff$cnst$font_size_annotation_numeric
  ) +
  # country labels
  geom_text(
    aes(
      x = region_pos,
      y = ex_diff_sexdiff_q025,
      label = region_name,
    ),
    color = fig$sexdiff$cnst$font_color_country,
    family = fig$sexdiff$cnst$font_family_label,
    position = position_nudge(y = -0.15),
    data =
      fig$sexdiff$data %>% filter(region_pos%%2==1, region_name != 'Russia'),
    size = fig$sexdiff$cnst$font_size_annotation
  ) +
  geom_text(
    aes(
      x = region_pos,
      y = ex_diff_sexdiff_q975,
      label = region_name,
    ),
    color = fig$sexdiff$cnst$font_color_country,
    family = fig$sexdiff$cnst$font_family_label,
    position = position_nudge(y = 0.15),
    data =
      fig$sexdiff$data %>% filter(region_pos%%2==0, region_name != 'Russia'),
    size = fig$sexdiff$cnst$font_size_annotation
  ) +
  geom_text(
    aes(
      x = region_pos,
      y = ex_diff_sexdiff_q025,
      label = region_name,
    ),
    color = fig$sexdiff$cnst$font_color_country,
    family = fig$sexdiff$cnst$font_family_label,
    position = position_nudge(y = -0.15),
    data =
      fig$sexdiff$data %>% filter(region_name == 'Russia'),
    size = fig$sexdiff$cnst$font_size_annotation*1.2,
    fontface = 'bold'
  ) +
  # annotation
  annotate(
    'text', x = 28, y = 1.15, label = 'Larger life expectancy drop for men',
    color = fig$sexdiff$cnst$color_positive,
    hjust = 1, vjust = 0, alpha = 0.5,
    family = fig$sexdiff$cnst$font_family_label,
    size = fig$sexdiff$cnst$font_size_annotation
  ) +
  annotate(
    'text', x = 0, y = -0.9, label = 'Larger life expectancy drop for women',
    color = fig$sexdiff$cnst$color_negative,
    hjust = 0, vjust = 1, alpha = 0.5,
    family = fig$sexdiff$cnst$font_family_label,
    size = fig$sexdiff$cnst$font_size_annotation
  ) +
  annotate(
    'text', x = 0, y = 1,
    label = 'Sex difference in years of life expectancy drop',
    hjust = 0, vjust = -1, fontface = 'bold',
    size = fig$sexdiff$cnst$font_size_annotation*1.3
  ) +
  # meta plot
  scale_color_manual(
    values = c('1' = fig$sexdiff$cnst$color_positive,
               '-1' = fig$sexdiff$cnst$color_negative,
               '2' = fig$sexdiff$cnst$color_negative_light,
               '4' = fig$sexdiff$cnst$color_positive_light,
               '5' = fig$sexdiff$cnst$color_negative_light2,
               '7' = fig$sexdiff$cnst$color_positive_light2)
  ) +
  scale_x_continuous(breaks = 1:30, labels = NULL) +
  fig_spec$MyGGplotTheme(
    show_legend = FALSE, grid = 'y', axis = '',
    family = fig$sexdiff$cnst$font_family_theme,
    size = fig$sexdiff$cnst$font_size_theme
  ) +
  theme(panel.grid.major.y =
          element_line(color = 'grey70', size = 0.1, linetype = 1)) +
  coord_cartesian(ylim = c(-1.4, 1.4), expand = c(0,0)) +
  labs(x = NULL, y = NULL)
fig$sexdiff$plot

fig_spec$ExportFigure(
  fig$sexdiff$plot, cnst$path_out, filename = 'fig_sexdiff',
  width = 172, height = 70, device = 'png'
)

# Plot e0russia ---------------------------------------------------

fig$e0russia$cnst <- list(); fig$e0russia$cnst <- within(fig$e0russia$cnst, {

  font_size_annotation = 2.3
  font_size_annotation_numeric = font_size_annotation*0.8
  lighten_uncertainty = 0.8
  width_segment = 2.4
  size_line = 1
  font_family_label = 'Roboto Condensed'
  font_family_theme = 'Roboto'
  font_size_theme = 6
  color_male = '#005784'
  color_male_light =
    clr_mix(color_male, mix_in = '#FFFFFF', ratio = lighten_uncertainty)
  color_male_light2 =
    clr_mix(color_male, mix_in = '#FFFFFF', ratio = lighten_uncertainty*0.5)
  color_female = '#B70D0D'
  color_female_light =
    clr_mix(color_female, mix_in = '#FFFFFF', ratio = lighten_uncertainty)
  color_female_light2 =
    clr_mix(color_female, mix_in = '#FFFFFF', ratio = lighten_uncertainty*0.5)

})

fig$e0russia$func$FormatLabel <- function (x) {
  lab <- formatC(x, digits = 1, format = 'f')
}

fig$e0russia$func$FormatLabel2 <- function (x, y, z) {
  lab <- list()
  lab$x <- formatC(x, digits = 2, format = 'f', flag = '+')
  lab$y <- formatC(y, digits = 2, format = 'f', flag = '+')
  lab$z <- formatC(z, digits = 2, format = 'f', flag = '+')
  lab <- lapply(lab, function (x) str_replace_all(x, '(^[+-])(0)(.+)', '\\1\\3'))
  paste0(lab$x, ' (', lab$y, ', ', lab$z, ')')
}

fig$e0russia$data$timeseries <- list()
fig$e0russia$data$timeseries <-
  dat$lt_ex_sexdiff %>%
  filter(region_iso == 'RU', x == 0) %>%
  mutate(
    ex_Female_midpoint = (lead(ex_Female)+ex_Female)/2,
    ex_Male_midpoint = (lead(ex_Male)+ex_Male)/2,
    ex_diff_Female_label = fig$e0russia$func$FormatLabel2(
      ex_diff_Female, ex_diff_Female_q025, ex_diff_Female_q975
    ),
    ex_diff_Male_label = fig$e0russia$func$FormatLabel2(
      ex_diff_Male, ex_diff_Male_q025, ex_diff_Male_q975
    )
  )

# e0 timeseries
fig$e0russia$plot$timeseries <-
  fig$e0russia$data$timeseries %>%
  ggplot(aes(x = year)) +
  annotate(
    'polygon',
    x = c(2019, 2018.5, 2019.8, 2020),
    y = c(fig$e0russia$data$timeseries %>% filter(year == 2019) %>% pull(ex_Male),
          70, 70,
          fig$e0russia$data$timeseries %>% filter(year == 2020) %>% pull(ex_Male)),
    fill = 'grey70', color = NA, alpha = 0.2
  ) +
  annotate(
    'polygon',
    x = c(2019, 2018.5, 2019.8, 2020),
    y = c(fig$e0russia$data$timeseries %>% filter(year == 2019) %>% pull(ex_Female),
          76, 76,
          fig$e0russia$data$timeseries %>% filter(year == 2020) %>% pull(ex_Female)),
    fill = 'grey70', color = NA, alpha = 0.2
  ) +
  geom_line(
    aes(y = ex_Female),
    size = fig$e0russia$cnst$size_line,
    color = fig$e0russia$cnst$color_female_light2
  ) +
  geom_point(
    aes(y = ex_Female),
    size = fig$e0russia$cnst$size_line*2,
    color = fig$e0russia$cnst$color_female
  ) +
  geom_point(
    aes(y = ex_Female),
    shape = 1, size = fig$e0russia$cnst$size_line*3,
    color = fig$e0russia$cnst$color_female,
    data = fig$e0russia$data$timeseries %>% filter(year >= 2019)
  ) +

  geom_line(
    aes(y = ex_Male),
    size = fig$e0russia$cnst$size_line,
    color = fig$e0russia$cnst$color_male_light2
  ) +
  geom_point(
    aes(y = ex_Male),
    size = fig$e0russia$cnst$size_line*2,
    color = fig$e0russia$cnst$color_male
  ) +
  geom_point(
    aes(y = ex_Male),
    shape = 1, size = fig$e0russia$cnst$size_line*3,
    color = fig$e0russia$cnst$color_male,
    data = fig$e0russia$data$timeseries %>% filter(year >= 2019)
  ) +
  geom_text(
    aes(y = ex_Male+0.6, label = fig$e0russia$func$FormatLabel(ex_Male)),
    color = fig$e0russia$cnst$color_male_light2,
    size = fig$e0russia$cnst$font_size_annotation_numeric,
    fontface = 'bold'
  ) +
  geom_text(
    aes(y = ex_Female+0.6, label = fig$e0russia$func$FormatLabel(ex_Female)),
    color = fig$e0russia$cnst$color_female_light2,
    size = fig$e0russia$cnst$font_size_annotation_numeric,
    fontface = 'bold'
  ) +
  geom_text(
    aes(
      x = year+0.5,
      y = ex_Female_midpoint+0.6,
      label = ex_diff_Female_label
    ),
    color = fig$e0russia$cnst$color_female_light2,
    size = fig$e0russia$cnst$font_size_annotation_numeric
  ) +
  geom_text(
    aes(
      x = year+0.5,
      y = ex_Male_midpoint+0.6,
      label = ex_diff_Male_label
    ),
    color = fig$e0russia$cnst$color_male_light2,
    size = fig$e0russia$cnst$font_size_annotation_numeric
  ) +
  annotate(
    'text', x = 2015, y = 79,
    label = 'Life expectancy in years',
    hjust = 0, vjust = -0.3, fontface = 'bold',
    size = fig$e0russia$cnst$font_size_annotation*1.3
  ) +
  fig_spec$MyGGplotTheme(
    show_legend = FALSE, grid = 'y', axis = '',
    family = fig$e0russia$cnst$font_family_theme,
    size = fig$e0russia$cnst$font_size_theme
  ) +
  theme(
    panel.grid.major.y =
      element_line(color = 'grey90', size = 0.1, linetype = 1)
  ) +
  scale_y_continuous(breaks = c(65:80)) +
  coord_cartesian(expand = c(0.01,0.01), xlim = c(2014.9, 2020.1)) +
  labs(x = NULL, y = NULL)

fig$e0russia$plot$timeseries

# decomposition results
fig$e0russia$data$decomp <-
  dat$decomp$decomposition_results_by_age %>%
  filter(
    region_iso == 'RU',
    cause == 'All.cause',
    year.final == 2020
  ) %>%
  mutate(
    x_breaks =
      cut(x, breaks = c(0, 20, 60, 80, Inf), right = FALSE),
    x_pos =
      as.integer(x_breaks)
  ) %>%
  group_by(sex, x_pos, x_breaks) %>%
  summarise(contribution = sum(contribution)) %>%
  ungroup()

fig$e0russia$plot$decomp <-
  fig$e0russia$data$decomp %>%
  ggplot() +
  geom_segment(
    aes(
      x = 0, xend = contribution,
      y = x_pos, yend = x_pos
    ),
    size = fig$e0russia$cnst$width_segment,
    color = fig$e0russia$cnst$color_female,
    data = fig$e0russia$data$decomp %>% filter(sex == 'Female')
  ) +
  geom_segment(
    aes(
      x = 0, xend = contribution,
      y = x_pos-0.45, yend = x_pos-0.45
    ),
    size = fig$e0russia$cnst$width_segment,
    color = fig$e0russia$cnst$color_male,
    data = fig$e0russia$data$decomp %>% filter(sex == 'Male')
  ) +
  geom_vline(
    xintercept = 0
  ) +
  fig_spec$MyGGplotTheme(
    show_legend = FALSE, grid = '', axis = '',
    family = fig$e0russia$cnst$font_family_label,
    size = fig$e0russia$cnst$font_size_theme
  ) +
  scale_y_continuous(
    breaks = 1:4,
    labels = c('0 to 20', '20 to 60', '60 to 80', '80+ years')
  ) +
  scale_x_continuous(
    breaks = seq(0, -0.8, -0.2),
    label = c('0', '-.2', '-.4', '-.6', '-.8 yrs')
  ) +
  theme(
    plot.background = element_rect(colour = 'grey80', fill = 'white'),
    title = element_text(
      family = fig$e0russia$cnst$font_family_label,
      size = fig$e0russia$cnst$font_size_annotation
    )
  ) +
  labs(
    x = NULL, y = NULL, title = '\n'
  )

fig$e0russia$plot$final <-
  fig$e0russia$plot$timeseries +
  annotation_custom(
    ggplotGrob(fig$e0russia$plot$decomp),
    xmin = 2018.5, xmax = 2019.8, ymin = 70, ymax = 76
  ) +
  annotate(
    'text', x = 2018.52, y = 75.6,
    label = 'Age-wise contributions to 2020 life exp. drop',
    family = 'Roboto Condensed', size = fig$e0russia$cnst$font_size_annotation*0.9,
    hjust = 0, lineheight = 0.8
  ) +
  annotate(
    'text', x = 2015, y = 76.3,
    label = 'Female', color = fig$e0russia$cnst$color_female_light2,
    family = 'Roboto Condensed', size = fig$e0russia$cnst$font_size_annotation,
    hjust = 0
  ) +
  annotate(
    'text', x = 2015, y = 67.2,
    label = 'Male', color = fig$e0russia$cnst$color_male_light2,
    family = 'Roboto Condensed', size = fig$e0russia$cnst$font_size_annotation,
    hjust = 0
  )

fig$e0russia$plot$final

fig_spec$ExportFigure(
  fig$e0russia$plot$final, cnst$path_out, filename = 'fig_e0russia',
  width = 172, height = 70, device = 'png'
)

# Plot e0diff -----------------------------------------------------

dat$skeleton <- expand_grid(
  region_meta %>%
    select(region_code = region_code_iso3166_2, region_name),
  sex = c('Female', 'Male', 'Total'),
  year = 1945:2020
)

# sum female and male deaths and population exposures to a total column
dat$pclm_with_total <-
  dat$lt_input_85 %>%
  select(
    region_code = region_iso, sex, year, age_start, age_width,
    death_total, population_py
  ) %>%
  pivot_wider(
    id_cols = c(region_code, year, age_start, age_width),
    names_from = c(sex),
    values_from = c(death_total, population_py),
    names_sep = '.'
  ) %>%
  mutate(
    death_total.Total = death_total.Female + death_total.Male,
    population_py.Total = population_py.Female + population_py.Male,
  ) %>%
  pivot_longer(
    cols = starts_with(c('death_total', 'population_py')),
    names_to = c('.value', 'sex'), names_sep = '\\.'
  )

dat$pclm_lifetab <-
  dat$pclm_with_total %>%
  arrange(region_code, sex, year, age_start) %>%
  group_by(region_code, sex, year) %>%
  group_modify(~{
    CalculateLifeTable(.x, age_start, age_width, death_total, population_py)
  }) %>%
  ungroup()

dat$pclm_harmonized <-
  dat$pclm_lifetab %>%
  filter(x == 0) %>%
  group_by(region_code, sex) %>%
  ungroup() %>%
  select(region_code, sex, year, e0_pclm = ex)

dat$hmd_harmonized <-
  dat$hmd_lifetab %>%
  as_tibble() %>%
  filter(age == 0) %>%
  mutate(
    sex = case_when(
      sex == 'b' ~ 'Total', sex == 'f' ~ 'Female', sex == 'm' ~ 'Male')
  ) %>%
  right_join(region_meta, by = c('country' = 'region_code_hmd')) %>%
  select(region_code = region_code_iso3166_2, sex, year, e0_hmd = ex) %>%
  arrange(region_code, sex, year) %>%
  group_by(region_code, sex) %>%
  ungroup()

dat$de0 <-
  dat$skeleton %>%
  left_join(dat$pclm_harmonized) %>%
  left_join(dat$hmd_harmonized) %>%
  mutate(
    e0 = ifelse(is.na(e0_hmd), e0_pclm, e0_hmd)
  ) %>%
  group_by(region_code, sex) %>%
  mutate(de0 = e0 - lag(e0)) %>%
  ungroup()

fig$e0diff <- list()
fig$e0diff$cnst <- list(); fig$e0diff$cnst <- within(fig$e0diff$cnst, {

  font_size_annotation = 2.3
  lighten_uncertainty = 0.8
  width_uncertainty = 2.5
  font_family_label = 'roboto_condensed'
  font_family_theme = 'roboto'
  font_size_theme = 6
  font_color_country = 'grey50'
  color_20 = '#000000'
  color_20_light =
    clr_mix(color_20, mix_in = '#FFFFFF', ratio = lighten_uncertainty)
  color_15 = '#BD9900'
  color_15_light =
    clr_mix(color_15, mix_in = '#FFFFFF', ratio = lighten_uncertainty)
  color_90s = '#FF0000'
  color_90s_light =
    clr_mix(color_15, mix_in = '#FFFFFF', ratio = lighten_uncertainty)
  size_point_highlight = 1
  size_point_background = 1/3*size_point_highlight
  alpha_point_background = 0.1

})

fig$e0diff$data <-
  dat$de0 %>%
  filter(sex == 'Total', region_code %in% cnst$regions_for_analysis, year >= 1970) %>%
  mutate(
    region_name = case_when(
      region_name == 'Czech Republic' ~ 'Czech\nRepublic',
      region_name == 'England and Wales' ~ 'England &\nWales',
      region_name == 'Northern Ireland' ~ 'Northern\nIreland',
      TRUE ~ region_name
    ),
    region_code =
      fct_reorder(region_code, de0, last),
    region_pos = as.integer(region_code)
  )

fig$e0diff$plot <-
  ggplot(fig$e0diff$data) +
  # grid
  geom_hline(yintercept = 0, color = 'grey', size = 1) +
  # background
  geom_point(
    aes(
      x = region_pos,
      y = de0,
      label = region_name,
    ),
    size = fig$e0diff$cnst$size_point_background,
    color = fig$e0diff$cnst$font_color_country,
    alpha = fig$e0diff$cnst$alpha_point_background
  ) +
  # 2020
  geom_point(
    aes(
      x = region_pos,
      y = de0,
      label = region_name,
    ),
    color = fig$e0diff$cnst$color_20,
    size = fig$e0diff$cnst$size_point_highlight,
    data =
      fig$e0diff$data %>%
      filter(year == 2020)
  ) +
  # 2020 Russia
  geom_point(
    aes(
      x = region_pos,
      y = de0,
      label = region_name,
    ),
    color = fig$e0diff$cnst$color_20,
    size = fig$e0diff$cnst$size_point_highlight*2,
    shape = 1,
    data =
      fig$e0diff$data %>%
      filter(year == 2020, region_name == 'Russia')
  ) +
  # soviet dissolution
  geom_point(
    aes(
      x = region_pos,
      y = de0,
      label = region_name,
    ),
    color = fig$e0diff$cnst$color_90s,
    size = fig$e0diff$cnst$size_point_highlight,
    shape = 1,
    data =
      fig$e0diff$data %>%
      filter(
        year %in% 1991:1994 &
          region_code %in% c('BG', 'LT', 'PL', 'SI', 'HR', 'SK',
                             'HU', 'LU', 'EE', 'LV', 'RU') &
          de0 < 0
      )
  ) +
  # 2015
  geom_point(
    aes(
      x = region_pos,
      y = de0,
      label = region_name,
    ),
    color = fig$e0diff$cnst$color_15,
    size = fig$e0diff$cnst$size_point_highlight,
    shape = 1,
    data =
      fig$e0diff$data %>%
      filter(
        year %in% 2015 & de0 < 0
      )
  ) +
  # country labels
  geom_text(
    aes(
      x = region_pos,
      y = de0,
      label = region_name,
    ),
    color = fig$e0diff$cnst$font_color_country,
    family = fig$e0diff$cnst$font_family_label,
    position = position_nudge(y = -0.25),
    size = fig$e0diff$cnst$font_size_annotation,
    data =
      fig$e0diff$data %>%
      filter(region_pos%%2==1, region_name != 'Russia', year == 2020),
    lineheight = 0.8
  ) +
  geom_text(
    aes(
      x = region_pos,
      y = de0,
      label = region_name,
    ),
    color = fig$e0diff$cnst$font_color_country,
    family = fig$e0diff$cnst$font_family_label,
    position = position_nudge(y = 0.25),
    data =
      fig$e0diff$data %>%
      filter(region_pos%%2==0, region_name != 'Russia', year == 2020),
    size = fig$e0diff$cnst$font_size_annotation,
    lineheight = 0.8
  ) +
  geom_text(
    aes(
      x = region_pos,
      y = de0,
      label = region_name,
    ),
    color = fig$e0diff$cnst$font_color_country,
    family = fig$e0diff$cnst$font_family_label,
    position = position_nudge(y = 0.25),
    data =
      fig$e0diff$data %>% filter(region_name == 'Russia', year == 2020),
    size = fig$e0diff$cnst$font_size_annotation*1.2,
    fontface = 'bold'
  ) +
  scale_x_continuous(breaks = 1:30, labels = NULL) +
  scale_y_continuous(
    breaks = seq(-3, 2), labels = c('-3 years', '-2', '-1', '0', '+1', '+2 years')
  ) +
  fig_spec$MyGGplotTheme(
    show_legend = FALSE, grid = 'y', axis = '',
    family = fig$e0diff$cnst$font_family_theme,
    size = fig$e0diff$cnst$font_size_theme
  ) +
  theme(panel.grid.major.y =
          element_line(color = 'grey70', size = 0.1, linetype = 1)) +
  coord_cartesian(xlim = c(1,30)) +
  labs(x = NULL, y = NULL) +
  annotate(
    'text', x = 0, y = 2,
    label = 'Annual change in life expectancy',
    hjust = 0, vjust = -1, fontface = 'bold',
    size = fig$e0diff$cnst$font_size_annotation*1.3
  )
fig$e0diff$plot

fig_spec$ExportFigure(
  fig$e0diff$plot, cnst$path_out, filename = 'fig_e0diff',
  width = 172, height = 70, device = 'pdf'
)
