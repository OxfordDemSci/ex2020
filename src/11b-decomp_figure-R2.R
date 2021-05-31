
# Init ------------------------------------------------------------

library(here); library(glue)
library(ggplot2)
library(data.table)
library(patchwork)
library(tidyverse)

# global const
wd <- here()
cnst <- list()
dat <- list()
figs <- list()
text.results <- list()

# load data
decomp_data.full <- readRDS(glue('{wd}/out/decomposition_results.rds'))

source(glue('{wd}/cfg/fig_specs.R'))

# groups by age and year
cnst$length.grouping <- 10
cnst$analysis.breaks.age   <- c(0,60,80)
cnst$analysis.breaks.years <- c(2015,2019,2020)
cnst$path_tmp <- glue('{wd}/tmp')


# Analysis by age ---------------------------------------------------------
# create variable for new age grouping
decomp_data.full$decomposition_results_by_age_cause[, x.new := cut(x, breaks=c(seq(min(x),max(x),cnst$length.grouping),Inf),ordered_result = T,include.lowest = T,right = F)]

# create variable for more aggregate analysis
decomp_data.full$decomposition_results_by_age_cause[, x.aggregated := cut(x, breaks=c(cnst$analysis.breaks.age ,Inf),ordered_result = T,include.lowest = T,right = F)]

# create variable for new year grouping
decomp_data.full$decomposition_results_by_age_cause[, year.new := cut(year.initial, breaks=c(cnst$analysis.breaks.years),ordered_result = T,include.lowest = T,right = F)]

# Prepare datasets for prelim figures  --------------------------------------------------------

#aggregate over the new grouping

dat$cause.group <- decomp_data.full$decomposition_results_by_age_cause[!(is.na(contribution)),
                                                                                         .(contribution = sum(contribution)), 
                                                                                         by = .(region_iso,sex,year.new,cause)]

# Age decomp figures --------------------------------------------------------
figs$fig.cause.covid_Referee2<-
  ggplot(dat$cause.group[year.new %in% '[2019,2020]' ] , aes(x = region_iso, y = contribution, fill = cause)) +
  ggtitle('Contribution to changes in life expectancy by COVID-19', 
          subtitle = 'Negative (positive) values decrease (increase) life expectancy')+
  facet_wrap(~sex)+
  geom_bar(stat = "identity",position = "stack",show.legend = T)+
  theme_light()+
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle=0, hjust=1),panel.grid.minor.x = element_blank())+
  labs(x = NULL, y = "Losses|Gains in life expectancy at birth in years",size=10)+
  theme(text = element_text(size=10),
        strip.text.x = element_text(size = 10, colour = "black"))+
  theme(legend.text=element_text(size=10), legend.title = element_text(size=10),
        legend.position = 'bottom')+
  geom_hline(yintercept = 0)+
  coord_flip()
figs$fig.cause.covid_Referee2

