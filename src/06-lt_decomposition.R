
# Init ------------------------------------------------------------

library(here); library(glue)
library(tidyverse)
library(data.table)
library(DemoDecomp)
library(reshape2)
library(doParallel)

# Constants -------------------------------------------------------
wd <- here()
config <- yaml::read_yaml(glue::glue('{wd}/cfg/config.yaml'))
cnst <- list()
cnst <- within(cnst, {
  regions_for_analysis = config$regions_for_all_cause_analysis
  regions_for_cause_of_death_analysis = config$regions_for_covid_cause_analysis
  path_out = glue('{wd}/out')
  path_tmp = glue('{wd}/tmp')
})

cnst$initial.year <- 2015
cnst$final.year <- 2020

dat <- list()
fig <- list()

# Data ------------------------------------------------------------
source(glue('{wd}/cfg/fig_specs.R'))

#dat$lt_input <- readRDS(glue('{cnst$path_out}/lt_input.rds'))

dat$lt_input <- readRDS(glue('{cnst$path_out}/lt_input_85.rds'))

dat$lt_input_sub <-
  dat$lt_input %>%
  filter(region_iso %in% cnst$regions_for_analysis)

#create dataset needed for decomposition, I'm using data.table here but you can edit
dat$mx_input_decomp <- data.table(dat$lt_input_sub)
#add all.cause mx
dat$mx_input_decom[,mx := death_total/population_py]
#add covid19 mx
dat$mx_input_decom[,mx.covid := ifelse(year %in% 2020,
                                       mx*(death_covid/death_total),0)]
#check  dat$mx_input_decom[is.na(mx.covid)]
dat$mx_input_decom[,mx.non.covid := ifelse(year %in% 2020,
                                           mx - mx.covid,mx)]

# Functions -------------------------------------------------------

#General Life expectancy function
life.expectancy.fun  <-
  function (mx, x, nx, cond_age = 0) {

    px = exp(-mx*{{nx}})
    qx = 1-px
    lx = head(cumprod(c(1, px)), -1)
    dx = c(-diff(lx), tail(lx, 1))
    Lx = ifelse(mx==0, lx*nx, dx/mx)
    Tx = rev(cumsum(rev(Lx)))
    ex = Tx/lx

    ex[cond_age + 1]

  }

#Life expectancy from a vector of mx by causes of death stacked
#c(mx.all.except.covid[from age 0 to 100], mx.covid19[from age 0 to 100])

life.expectancy.cod.fun <-
  function(mx.cod, x, nx,cond_age = 0){
    dim(mx.cod) <- c(length(x),length(mx.cod)/length(x))
    mx          <- rowSums(mx.cod)
    life.expectancy.fun(mx,x,nx,cond_age)
  }


#DT <- dat$mx_input_decomp[region_iso %in% 'BE' & sex == 'Female']
#y <- 2016
# create a function to decompose by age every yearly change
Decomp_fun <-
  function(DT = .SD, covid.included = 1, cond_age = 0){
  x          <- sort(unique(DT$age_start))
  years      <- sort(unique(DT$year))
  nx         <- DT$age_width[1:length(x)]


  decomp <- mclapply(years[-1],function(y,x =x, nx = nx , m = m, N = N, cond_age = cond_age){

    if(covid.included == 1){
    mx2  <- c(m[m$year == y,]$mx.covid,m[m$year == y,]$mx.non.covid)
    mx1  <- c(m[m$year == y-1,]$mx.covid,m[m$year == y-1,]$mx.non.covid)}

    if(covid.included != 1){
      mx2  <- m[m$year == y,]$mx
      mx1  <- m[m$year == y - 1,]$mx}

    hor  <- horiuchi(func = life.expectancy.cod.fun,pars1 = mx1,pars2 = mx2,N = N,x =x, nx = nx, cond_age)

    dim(hor) <- c(length(x), length(hor)/length(x))

    colnames(hor) <- if(covid.included != 1){'All.cause'}

    rownames(hor) <- x

    hor <- data.table(melt(hor))

    names(hor) <- c('x','cause','contribution')
    hor$year.final <- as.numeric(y)
    hor
  }, m = DT, x = x, nx = nx, N = 50, mc.cores = 1, cond_age)

  decomp.results <- data.table(do.call(rbind,decomp))

  decomp.results

  }


Decomp_fun_stepwise <-
  function(DT = .SD, covid.included = 1,cond_age = 0){
    x          <- sort(unique(DT$age_start))
    years      <- sort(unique(DT$year))
    nx         <- DT$age_width[1:length(x)]


    decomp <- mclapply(years[-1],function(y,x =x, nx = nx , m = m, cond_age){

      if(covid.included == 1){
        mx2  <- c(m[m$year == y,]$mx.covid,m[m$year == y,]$mx.non.covid)
        mx1  <- c(m[m$year == y-1,]$mx.covid,m[m$year == y-1,]$mx.non.covid)}

      if(covid.included != 1){
        mx2  <- m[m$year == y,]$mx
        mx1  <- m[m$year == y - 1,]$mx}



      hor  <- stepwise_replacement(func = life.expectancy.cod.fun,pars1 = mx1,pars2 = mx2,x =x, nx = nx, cond_age = cond_age)

      dim(hor) <- c(length(x), length(hor)/length(x))

      colnames(hor) <- if(covid.included != 1){'All.cause'}

      rownames(hor) <- x

      hor <- data.table(melt(hor))

      names(hor) <- c('x','cause','contribution')
      hor$year.final <- as.numeric(y)
      hor
    }, m = DT, x = x, nx = nx, cond_age = cond_age, mc.cores = 1)

    decomp.results <- data.table(do.call(rbind,decomp))

    decomp.results

  }

# Decomposition calculations -----------------------------------------

##### Decomposition for life expectancy at birth
#decomp by age
dat$decomposition_results_by_age <- dat$mx_input_decomp[, Decomp_fun(DT = .SD,covid.included = 0,cond_age = 0), by = .(region_iso,sex)]


#decomp by age and cause, fewer countries
dat$decomposition_results_by_age_cause <- dat$mx_input_decomp[region_iso %in% cnst$regions_for_cause_of_death_analysis,
                                                        Decomp_fun(DT = .SD,covid.included = 1,cond_age = 0), by = .(region_iso,sex)]

#double check new countries
#check.1 <- dat$mx_input_decomp[region_iso %in% c('US', 'NO', 'HU') & is.na(mx.covid)]
#dat$decomposition_results_by_age_cause[is.na(contribution) & year.final %in% 2020]

#decomp by age and cause, fewer countries sensitivity check with stepwise
dat$decomposition_results_by_age_stepwise <- dat$mx_input_decomp[,Decomp_fun_stepwise(DT = .SD,covid.included = 0,cond_age = 0),
                                                                 by = .(region_iso,sex)]


##### Decomposition for life expectancy at age 60
#decomp by age
dat$decomposition_results_by_age_60 <- dat$mx_input_decomp[, Decomp_fun(DT = .SD,covid.included = 0,cond_age = 60), by = .(region_iso,sex)]


#decomp by age and cause, fewer countries
dat$decomposition_results_by_age_cause_60 <- dat$mx_input_decomp[region_iso %in% cnst$regions_for_cause_of_death_analysis,
                                                              Decomp_fun(DT = .SD,covid.included = 1,cond_age = 60), by = .(region_iso,sex)]

#decomp by age and cause, fewer countries sensitivity check with stepwise
dat$decomposition_results_by_age_stepwise_60 <- dat$mx_input_decomp[,Decomp_fun_stepwise(DT = .SD,covid.included = 0,cond_age = 60),
                                                                 by = .(region_iso,sex)]


##### Decomposition for life expectancy at age 80
#decomp by age
#dat$decomposition_results_by_age_80 <- dat$mx_input_decomp[, Decomp_fun(DT = .SD,covid.included = 0,cond_age = 80), by = .(region_iso,sex)]


#decomp by age and cause, fewer countries
#dat$decomposition_results_by_age_cause_80 <- dat$mx_input_decomp[region_iso %in% cnst$regions_for_cause_of_death_analysis,Decomp_fun(DT = .SD,covid.included = 1,cond_age = 80), by = .(region_iso,sex)]

#decomp by age and cause, fewer countries sensitivity check with stepwise
#dat$decomposition_results_by_age_stepwise_80 <- dat$mx_input_decomp[,Decomp_fun_stepwise(DT = .SD,covid.included = 0,cond_age = 80),by = .(region_iso,sex)]

#add some variables and save

dat$decomposition_results_by_age$year.initial <-  dat$decomposition_results_by_age$year.final-1
dat$decomposition_results_by_age_cause$year.initial <-  dat$decomposition_results_by_age_cause$year.final-1
dat$decomposition_results_by_age_stepwise$year.initial <-  dat$decomposition_results_by_age_stepwise$year.final-1


dat$decomposition_results_by_age_cause$cause <- factor(dat$decomposition_results_by_age_cause$cause,labels = c('covid','non.covid'))


dat$decomposition_results_by_age_60$year.initial <-  dat$decomposition_results_by_age_60$year.final-1
dat$decomposition_results_by_age_cause_60$year.initial <-  dat$decomposition_results_by_age_cause_60$year.final-1
dat$decomposition_results_by_age_stepwise_60$year.initial <-  dat$decomposition_results_by_age_stepwise_60$year.final-1


dat$decomposition_results_by_age_cause_60$cause <- factor(dat$decomposition_results_by_age_cause_60$cause,labels = c('covid','non.covid'))


#dat$decomposition_results_by_age_80$year.initial <-  dat$decomposition_results_by_age_80$year.final-1
#dat$decomposition_results_by_age_cause_80$year.initial <-  dat$decomposition_results_by_age_cause_80$year.final-1
#dat$decomposition_results_by_age_stepwise_80$year.initial <-  dat$decomposition_results_by_age_stepwise_80$year.final-1


#dat$decomposition_results_by_age_cause_80$cause <- factor(dat$decomposition_results_by_age_cause_80$cause,labels = c('covid','non.covid'))



saveRDS(dat, file = glue('{wd}/out/decomposition_results.rds'))

