# Leslie-Matrix projection of midyear population 2018 for years
# 2019 and 2020 assuming stable population

# prepare a data set with required variables for
# Leslie matrix population projection for GB sub-regions
gb_pop_estimates <-
  # skeleton
  expand_grid(
    year = 2015:2018,
    region_code_hmd = c('GBRTENW', 'GBR_NIR', 'GBR_SCO'),
    sex = c('Female', 'Male'),
    age_start = 0:110
  ) %>%
  # midyear population
  left_join(
    dat$hmdhfd$hmd_pop %>%
      pivot_longer(c(Female, Male),
                   names_to = 'sex',
                   values_to = 'population_midyear') %>%
      select(region_code_hmd, year = Year, age_start = Age, sex, population_midyear)
  ) %>%
  # death rates
  left_join(
    dat$hmdhfd$hmd_mort %>%
      pivot_longer(c(Female, Male),
                   names_to = 'sex',
                   values_to = 'death_rate') %>%
      select(region_code_hmd, year = Year, age_start = Age, sex, death_rate)
  ) %>%
  # female fertility rates
  left_join(
    dat$hmdhfd$hfd_fert %>%
      select(region_code_hmd, year = Year, age_start = Age, female_fertility_rate = ASFR)
  ) %>%
  mutate(
    # fertility rates are NA outside the age range [12, 55],
    # replace with 0
    female_fertility_rate =
      ifelse(is.na(female_fertility_rate), 0, female_fertility_rate)
  )

.x <-
  gb_projection %>%
  filter(region_code_hmd == 'GBRTENW')

jump_off_year = 2018
n = 50
srb = 1.04
nage = 111

jump_off_pop <- filter(.x, year == jump_off_year)

# first project the female population for n steps
female <- filter(jump_off_pop, sex == 'Female')
# Leslie projection matrix
A_f <- matrix(0, nrow = nage, ncol = nage)
diag(A_f[-1,-nage]) <- head(exp(-female$death_rate), -1)
A_f[1,] <- female$female_fertility_rate * 1/(1+srb)
# population matrix, first column is jump off population
pop_f <- matrix(0, nrow = nage, ncol = n+1)
pop_f[,1] <- female$population_midyear
# project population
for (i in 1:n+1) {
  pop_f[,i] <- A_f%*%pop_f[,i-1]
}

# now project the male population with male births derived from
# projected female births via sex ratio
male <- filter(jump_off_pop, sex == 'Male')
A_m <- matrix(0, nage, ncol = nage)
diag(A_m[-1,-nage]) <- head(exp(-male$death_rate), -1)
pop_m <- matrix(0, nrow = nage, ncol = n+1)
pop_m[,1] <- male$population_midyear
for (i in 1:n+1) {
  pop_m[,i] <- A_m%*%pop_m[,i-1]
  pop_m[1,i] <- pop_f[1,i]*srb
}

library(ggplot2)

expand_grid(
  year = seq(jump_off_year, jump_off_year+n),
  region_code_hmd = '.y$region_code_hmd',
  age_start = 0:110
) %>%
  mutate(
    Female.population_midyear = c(pop_f),
    Male.population_midyear = c(pop_m)
  ) %>%
  filter(year %in% c(2018, 2020, 2050)) %>%
  ggplot(aes(x = age_start, group = year, color = year)) +
  geom_line(aes(y = Female.population_midyear)) +
  geom_line(aes(y = -Male.population_midyear)) +
  coord_flip()
