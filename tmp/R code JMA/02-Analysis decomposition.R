
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
decomp_data.full$decomposition_results_by_age[, x.new := cut(x, breaks=c(seq(min(x),max(x),cnst$length.grouping),Inf),ordered_result = T,include.lowest = T,right = F)]
decomp_data.full$decomposition_results_by_age_cause[, x.new := cut(x, breaks=c(seq(min(x),max(x),cnst$length.grouping),Inf),ordered_result = T,include.lowest = T,right = F)]
decomp_data.full$decomposition_results_by_age_stepwise[, x.new := cut(x, breaks=c(seq(min(x),max(x),cnst$length.grouping),Inf),ordered_result = T,include.lowest = T,right = F)]

# create variable for more aggregate analysis
decomp_data.full$decomposition_results_by_age[, x.aggregated := cut(x, breaks=c(cnst$analysis.breaks.age ,Inf),ordered_result = T,include.lowest = T,right = F)]
decomp_data.full$decomposition_results_by_age_cause[, x.aggregated := cut(x, breaks=c(cnst$analysis.breaks.age ,Inf),ordered_result = T,include.lowest = T,right = F)]
decomp_data.full$decomposition_results_by_age_stepwise[, x.aggregated := cut(x, breaks=c(cnst$analysis.breaks.age ,Inf),ordered_result = T,include.lowest = T,right = F)]

# create variable for new year grouping
decomp_data.full$decomposition_results_by_age[, year.new := cut(year.initial, breaks=c(cnst$analysis.breaks.years),ordered_result = T,include.lowest = T,right = F)]
decomp_data.full$decomposition_results_by_age_cause[, year.new := cut(year.initial, breaks=c(cnst$analysis.breaks.years),ordered_result = T,include.lowest = T,right = F)]
decomp_data.full$decomposition_results_by_age_stepwise[, year.new := cut(year.initial, breaks=c(cnst$analysis.breaks.years),ordered_result = T,include.lowest = T,right = F)]


# Prepare datasets for prelim figures  --------------------------------------------------------

#aggregate over the new grouping
dat$decomp_new_age_groups_all_cause <- decomp_data.full$decomposition_results_by_age[!(is.na(contribution)),
                                         .(contribution = sum(contribution)), 
                                         by = .(region_iso,sex,x.new,year.new)]

dat$decomp_large_age_groups <- decomp_data.full$decomposition_results_by_age[!(is.na(contribution)),
                                                                             .(contribution = sum(contribution)), 
                                                                             by = .(region_iso,sex,cause,x.aggregated,year.new)]


dat$decomp_new_age_groups_covid <- decomp_data.full$decomposition_results_by_age_cause[!(is.na(contribution)),
                                                                                 .(contribution = sum(contribution)), 
                                                                                 by = .(region_iso,sex,x.new,year.new,cause)]

dat$decomp_large_age_groups_covid <- decomp_data.full$decomposition_results_by_age_cause[!(is.na(contribution)),
                                                                                       .(contribution = sum(contribution)), 
                                                                                       by = .(region_iso,sex,x.aggregated,year.new,cause)]

dat$cause.group <- decomp_data.full$decomposition_results_by_age_cause[!(is.na(contribution)),
                                                                                         .(contribution = sum(contribution)), 
                                                                                         by = .(region_iso,sex,year.new,cause)]

dat$decomp_large_age_groups_stepwise <- decomp_data.full$decomposition_results_by_age_stepwise[!(is.na(contribution)), .(contribution = sum(contribution,na.rm = T)), 
                                                                                         by = .(region_iso,sex,x.aggregated,year.new)]


# Description in paper --------------------------------------------------------

text.results$above.80. <-  dat$decomp_large_age_groups[x.aggregated %in% '[80,Inf]']
text.results$above.80[,months := contribution*12]

text.results$above.80[order(year.new,sex,months)] %>% filter(sex %in% 'Female' & year.new %in% '[2019,2020]')

text.results$above.80[order(year.new,sex,months)] %>% filter(sex %in% 'Male' & year.new %in% '[2019,2020]')


text.results$above.60 <-  dat$decomp_large_age_groups[x.aggregated %in% '[60,80)']
text.results$above.60[,months := contribution*12]

text.results$above.60[order(year.new,sex,months)] %>% filter(sex %in% 'Female' & year.new %in% '[2019,2020]')

text.results$above.60[order(year.new,sex,months)] %>% filter(sex %in% 'Male' & year.new %in% '[2019,2020]')


text.results$below.60 <-  dat$decomp_large_age_groups[x.aggregated %in% '[0,60)']
text.results$below.60[,months := contribution*12]

text.results$below.60[order(year.new,sex,months)] %>% filter(sex %in% 'Female' & year.new %in% '[2019,2020]')

text.results$below.60[order(year.new,sex,months)] %>% filter(sex %in% 'Male' & year.new %in% '[2019,2020]')


text.results$covid <- dat$cause.group[cause %in% 'covid' & year.new %in% '[2019,2020]']
text.results$covid[order(sex,contribution)]

# Age decomp figures --------------------------------------------------------

#Figure age decomposition
figs$fig.10.age.group.female <-
    ggplot(dat$decomp_new_age_groups_all_cause[sex == 'Female'] , aes(x = x.new, y = contribution, fill = year.new)) +
  ggtitle('Decomposition of life expectancy', subtitle = 'Females. Negative (positive) values decrease (increase) life expectancy')+
  facet_wrap(~region_iso)+
  scale_fill_manual('Period', values = c('#517AC9','#C05D5D')) +
  geom_bar(stat = "identity",position = "stack")+
  theme_light()+
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle=0, hjust=1),panel.grid.minor.x = element_blank())+
  labs(x = "Age group", y = "Losses|Gains in life expectancy at birth in years",size=10)+
  theme(text = element_text(size=10),
        strip.text.x = element_text(size = 10, colour = "black"))+
  theme(legend.text=element_text(size=10), legend.title = element_text(size=10),
        legend.position = c(0.9, 0.18))+
  geom_hline(yintercept = 0)+
  coord_flip()
figs$fig.10.age.group.female

figs$fig.10.age.group.male <-
  ggplot(dat$decomp_new_age_groups_all_cause[sex == 'Male'] , aes(x = x.new, y = contribution, fill = year.new)) +
  ggtitle('Decomposition of life expectancy', subtitle = 'Males. Negative (positive) values decrease (increase) life expectancy')+
  facet_wrap(~region_iso)+
  scale_fill_manual('Period', values = c('#8892C8','#72A162')) +
  geom_bar(stat = "identity",position = "stack")+
  theme_light()+
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle=0, hjust=1),panel.grid.minor.x = element_blank())+
  labs(x = "Age group", y = "Losses|Gains in life expectancy at birth in years",size=10)+
  theme(text = element_text(size=10),
        strip.text.x = element_text(size = 10, colour = "black"))+
  theme(legend.text=element_text(size=10), legend.title = element_text(size=10),
        legend.position = c(0.9, 0.18))+
  geom_hline(yintercept = 0)+
  coord_flip()
figs$fig.10.age.group.male



# Large age group figure --------------------------------------------------------

figs$fig.large.age.group.female <-
  ggplot(dat$decomp_large_age_groups[sex == 'Female'] , aes(x = x.aggregated, y = contribution, fill = year.new)) +
  ggtitle('Contribution to changes in life expectancy by age group', 
          subtitle = 'Females, Negative (positive) values decrease (increase) life expectancy')+
  facet_wrap(~region_iso)+
  scale_fill_manual('Period', values = c('#517AC9','#C05D5D')) +
  geom_bar(stat = "identity",position = "stack")+
  theme_light()+
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle=0, hjust=1),panel.grid.minor.x = element_blank())+
  labs(x = "Age group", y = "Losses|Gains in life expectancy at birth in years",size=10)+
  theme(text = element_text(size=10),
        strip.text.x = element_text(size = 10, colour = "black"))+
  theme(legend.text=element_text(size=10), legend.title = element_text(size=10),
        legend.position = c(0.9, 0.18))+
  geom_hline(yintercept = 0)+
  ylim(-1,1.7)+
  coord_flip()
figs$fig.large.age.group.female

figs$fig.large.age.group.male <-
  ggplot(dat$decomp_large_age_groups[sex == 'Male'] , aes(x = x.aggregated, y = contribution, fill = year.new)) +
  ggtitle('Contribution to changes in life expectancy by age group', 
          subtitle = 'Males, Negative (positive) values decrease (increase) life expectancy')+
  facet_wrap(~region_iso)+
  scale_fill_manual('Period', values = c('#8892C8','#72A162')) +
  geom_bar(stat = "identity",position = "stack")+
  theme_light()+
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle=0, hjust=1),panel.grid.minor.x = element_blank())+
  labs(x = "Age group", y = "Losses|Gains in life expectancy at birth in years",size=10)+
  theme(text = element_text(size=10),
        strip.text.x = element_text(size = 10, colour = "black"))+
  theme(legend.text=element_text(size=10), legend.title = element_text(size=10),
        legend.position = c(0.9, 0.18))+
  geom_hline(yintercept = 0)+
  ylim(-1,1.7)+
  coord_flip()
figs$fig.large.age.group.male




# Age and cause decomp figures --------------------------------------------------------

#Figure age decomposition
figs$fig.10.age.group.female.cause <-
  ggplot(dat$decomp_new_age_groups_covid[sex == 'Female'] , aes(x = x.new, y = contribution, fill = interaction(year.new,cause))) +
  ggtitle('Decomposition of life expectancy', subtitle = 'Females. Negative (positive) values decrease (increase) life expectancy')+
  facet_wrap(~region_iso)+
  geom_bar(stat = "identity",position = "stack")+
  theme_light()+
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle=0, hjust=1),panel.grid.minor.x = element_blank())+
  labs(x = "Age group", y = "Losses|Gains in life expectancy at birth in years",size=10)+
  theme(text = element_text(size=10),
        strip.text.x = element_text(size = 10, colour = "black"))+
  theme(legend.text=element_text(size=10), legend.title = element_text(size=10),
        legend.position = c(0.9, 0.18))+
  geom_hline(yintercept = 0)+
  coord_flip()
figs$fig.10.age.group.female.cause

figs$fig.10.age.group.male.cause <-
  ggplot(dat$decomp_new_age_groups_covid[sex == 'Male'] , aes(x = x.new, y = contribution, fill = interaction(year.new,cause))) +
  ggtitle('Decomposition of life expectancy', subtitle = 'Males. Negative (positive) values decrease (increase) life expectancy')+
  facet_wrap(~region_iso)+
  geom_bar(stat = "identity",position = "stack")+
  theme_light()+
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle=0, hjust=1),panel.grid.minor.x = element_blank())+
  labs(x = "Age group", y = "Losses|Gains in life expectancy at birth in years",size=10)+
  theme(text = element_text(size=10),
        strip.text.x = element_text(size = 10, colour = "black"))+
  theme(legend.text=element_text(size=10), legend.title = element_text(size=10),
        legend.position = c(0.9, 0.18))+
  geom_hline(yintercept = 0)+
  coord_flip()
figs$fig.10.age.group.male.cause



# Large age group figure --------------------------------------------------------

figs$fig.large.age.group.female.cause <-
  ggplot(dat$decomp_large_age_groups_covid[sex == 'Female'] , aes(x = x.aggregated, y = contribution, fill = interaction(year.new,cause))) +
  ggtitle('Contribution to changes in life expectancy by age group', 
          subtitle = 'Females, Negative (positive) values decrease (increase) life expectancy')+
  facet_wrap(~region_iso)+
  geom_bar(stat = "identity",position = "stack")+
  theme_light()+
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle=0, hjust=1),panel.grid.minor.x = element_blank())+
  labs(x = "Age group", y = "Losses|Gains in life expectancy at birth in years",size=10)+
  theme(text = element_text(size=10),
        strip.text.x = element_text(size = 10, colour = "black"))+
  theme(legend.text=element_text(size=10), legend.title = element_text(size=10),
        legend.position = c(0.9, 0.18))+
  geom_hline(yintercept = 0)+
  ylim(-1,1.7)+
  coord_flip()
figs$fig.large.age.group.female.cause

figs$fig.large.age.group.male.cause <-
  ggplot(dat$decomp_large_age_groups_covid[sex == 'Female'] , aes(x = x.aggregated, y = contribution, fill = interaction(year.new,cause))) +
  ggtitle('Contribution to changes in life expectancy by age group', 
          subtitle = 'Males, Negative (positive) values decrease (increase) life expectancy')+
  facet_wrap(~region_iso)+
  geom_bar(stat = "identity",position = "stack")+
  theme_light()+
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle=0, hjust=1),panel.grid.minor.x = element_blank())+
  labs(x = "Age group", y = "Losses|Gains in life expectancy at birth in years",size=10)+
  theme(text = element_text(size=10),
        strip.text.x = element_text(size = 10, colour = "black"))+
  theme(legend.text=element_text(size=10), legend.title = element_text(size=10),
        legend.position = c(0.9, 0.18))+
  geom_hline(yintercept = 0)+
  ylim(-1,1.7)+
  coord_flip()
figs$fig.large.age.group.male.cause



# Total cause of death figure --------------------------------------------------------

figs$fig.cause.covid<-
  ggplot(dat$cause.group[year.new %in% '[2019,2020]' & cause %in% 'covid' ] , aes(x = region_iso, y = contribution, fill = interaction(year.new,cause))) +
  ggtitle('Contribution to changes in life expectancy by COVID-19', 
          subtitle = 'Negative (positive) values decrease (increase) life expectancy')+
  facet_wrap(~sex)+
  geom_bar(stat = "identity",position = "stack",show.legend = F)+
  scale_fill_manual(values ='red')+
  theme_light()+
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle=0, hjust=1),panel.grid.minor.x = element_blank())+
  labs(x = "Age group", y = "Losses|Gains in life expectancy at birth in years",size=10)+
  theme(text = element_text(size=10),
        strip.text.x = element_text(size = 10, colour = "black"))+
  theme(legend.text=element_text(size=10), legend.title = element_text(size=10),
        legend.position = 'bottom')+
  geom_hline(yintercept = 0)+
  coord_flip()
figs$fig.cause.covid


figs$fig.cause.non.covid<-
  ggplot(dat$cause.group[year.new %in% '[2019,2020]' & cause %in% 'non.covid' ] , aes(x = region_iso, y = contribution, fill = interaction(year.new,cause))) +
  ggtitle('Contribution to changes in life expectancy by non COVID-19', 
          subtitle = 'Negative (positive) values decrease (increase) life expectancy')+
  facet_wrap(~sex)+
  geom_bar(stat = "identity",position = "stack",show.legend = F)+
  scale_fill_manual(values ='green')+
  theme_light()+
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle=0, hjust=1),panel.grid.minor.x = element_blank())+
  labs(x = "Age group", y = "Losses|Gains in life expectancy at birth in years",size=10)+
  theme(text = element_text(size=10),
        strip.text.x = element_text(size = 10, colour = "black"))+
  theme(legend.text=element_text(size=10), legend.title = element_text(size=10),
        legend.position = 'bottom')+
  geom_hline(yintercept = 0)+
  coord_flip()
figs$fig.cause.non.covid

# Figures for sensitivity analysis --------------------------------------------------------

figs$fig.large.age.group.female.stepwise <-
  ggplot(dat$decomp_large_age_groups_stepwise[sex == 'Female'] , aes(x = x.aggregated, 
                                                                           y = contribution, 
                                                                           fill = interaction(year.new))) +
  ggtitle('Contribution to changes in life expectancy by age group using stepwise decomposition', 
          subtitle = 'Females, Negative (positive) values decrease (increase) life expectancy')+
  facet_wrap(~region_iso)+
  geom_bar(stat = "identity",position = "stack")+
  theme_light()+
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle=0, hjust=1),panel.grid.minor.x = element_blank())+
  labs(x = "Age group", y = "Losses|Gains in life expectancy at birth in years",size=10)+
  theme(text = element_text(size=10),
        strip.text.x = element_text(size = 10, colour = "black"))+
  theme(legend.text=element_text(size=10), legend.title = element_text(size=10),
        legend.position = c(0.9, 0.18))+
  geom_hline(yintercept = 0)+
  ylim(-1,1.7)+
  coord_flip()
figs$fig.large.age.group.female.stepwise

figs$fig.large.age.group.male.stepwise <-
  ggplot(dat$decomp_large_age_groups_stepwise[sex == 'Female'] , aes(x = x.aggregated, y = contribution, 
                                                                           fill = interaction(year.new))) +
  ggtitle('Contribution to changes in life expectancy by age group using stepwise decomposition', 
          subtitle = 'Males, Negative (positive) values decrease (increase) life expectancy')+
  facet_wrap(~region_iso)+
  geom_bar(stat = "identity",position = "stack")+
  theme_light()+
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle=0, hjust=1),panel.grid.minor.x = element_blank())+
  labs(x = "Age group", y = "Losses|Gains in life expectancy at birth in years",size=10)+
  theme(text = element_text(size=10),
        strip.text.x = element_text(size = 10, colour = "black"))+
  theme(legend.text=element_text(size=10), legend.title = element_text(size=10),
        legend.position = c(0.9, 0.18))+
  geom_hline(yintercept = 0)+
  ylim(-1,1.7)+
  coord_flip()
figs$fig.large.age.group.male.stepwise


# Figures for sensitivity analysis --------------------------------------------------------

fig_spec$ExportFigure(figs$fig.10.age.group.female, path = cnst$path_tmp)
fig_spec$ExportFigure(figs$fig.10.age.group.male, path = cnst$path_tmp)
fig_spec$ExportFigure(figs$fig.large.age.group.female, path = cnst$path_tmp)
fig_spec$ExportFigure(figs$fig.large.age.group.male, path = cnst$path_tmp)
fig_spec$ExportFigure(figs$fig.10.age.group.female.cause, path = cnst$path_tmp)
fig_spec$ExportFigure(figs$fig.10.age.group.male.cause, path = cnst$path_tmp)
fig_spec$ExportFigure(figs$fig.large.age.group.female.cause, path = cnst$path_tmp)
fig_spec$ExportFigure(figs$fig.large.age.group.female.cause, path = cnst$path_tmp)
fig_spec$ExportFigure(figs$fig.cause.covid, path = cnst$path_tmp)
fig_spec$ExportFigure(figs$fig.cause.non.covid, path = cnst$path_tmp)
fig_spec$ExportFigure(figs$fig.large.age.group.female.stepwise, path = cnst$path_tmp)
fig_spec$ExportFigure(figs$fig.large.age.group.male.stepwise, path = cnst$path_tmp)



