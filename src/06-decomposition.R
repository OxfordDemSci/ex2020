
# Init ------------------------------------------------------------

library(here); library(glue)
library(data.table)
library(DemoDecomp)
library(reshape2)
library(doParallel)

# Get life tables  -------------------------------------------------
wd <- here()

source(glue('{wd}/src/05-calculate_lt.R'))

# Years we include in analysis  ------------------------------------
cnst$initial.year <- 2015
cnst$final.year <- 2020

# Functions used in the decomp   -----------------------------------

#General Life expectancy function 
life.expectancy.fun  <- 
  function (mx, x, nx) {
    
    px = exp(-mx*{{nx}})
    qx = 1-px
    lx = head(cumprod(c(1, px)), -1)
    dx = c(-diff(lx), tail(lx, 1))
    Lx = dx/mx
    Tx = rev(cumsum(rev(Lx)))
    ex = Tx/lx
    
    ex[1L]
    
  }

#Life expectancy from a vector of mx by causes of death stacked 
#c(mx.all.except.covid[from age 0 to 100], mx.covid19[from age 0 to 100])

life.expectancy.cod.fun <- 
  function(mx.cod, x, nx){
    dim(mx.cod) <- c(length(x),length(mx.cod)/length(x))
    mx          <- rowSums(mx.cod)
    life.expectancy.fun(mx,x,nx)
  }

# create a function to decompose by age every yearly change
Decomp_fun <- function(DT = .SD ){
  x          <- sort(unique(DT$x))
  years      <- sort(unique(DT$year))
  nx         <- DT$nx[1:length(x)]
  
  decomp <- mclapply(years[-1],function(y,x =x, nx = nx , m = m, N = N){
    mx2  <- m[m$year == y,]$mx
    mx1  <- m[m$year == y-1,]$mx
    
    hor  <- horiuchi(func = life.expectancy.cod.fun,pars1 = mx1,pars2 = mx2,N = N,x =x, nx = nx)
    
    dim(hor) <- c(length(x), length(unique(DT$Cause)))
    
    colnames(hor) <- unique(DT$Cause)
    rownames(hor) <- x
    
    hor <- data.table(melt(hor))
    
    names(hor) <- c('x','cause','contribution')
    hor$year.final <- as.numeric(y)
    hor
  }, m = DT, x = x, nx = nx, N = 50, mc.cores = 1)
  
  decomp.results <- data.table(do.call(rbind,decomp))
  
  decomp.results
  
}

# Subset life tables for only those to be decomposed------------------

#select data 
dat$lt_to_decompose <- data.table(dat$lt %>% filter(year %in% cnst$initial.year:cnst$final.year))

##This will have 2 causes of death once we include COVID-19 specific mx vs the rest mx
dat$lt_to_decompose$Cause <- 1

#calculate decomp by country and sex
dat$decomposition_results <- dat$lt_to_decompose[, Decomp_fun(DT = .SD), by = .(region_iso,sex)]

dat$decomposition_results$year.initial <-  dat$decomposition_results$year.final-1

saveRDS(dat$decomposition_results, file = glue('{wd}/out/decomposition_results.rds'))
