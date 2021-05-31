
# Init ------------------------------------------------------------
library(tidyverse)
library(magrittr)
library(prismatic)
library(patchwork)
library(hrbrthemes)
library(cowplot)
library(here); library(glue)
library(data.table)
library(reshape2)


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

ids <- read_rds("{wd}/out/ids.rds" %>% glue)

dat$lt_input <- readRDS(glue('{cnst$path_out}/lt_input_85.rds'))

dat$lt_input_sub <-
  dat$lt_input %>%
  filter(region_iso %in% cnst$regions_for_cause_of_death_analysis)%>%
  filter(year %in%cnst$final.year)

#view(dat$lt_input_sub)

#create dataset needed for decomposition, I'm using data.table here but you can edit
dat$mx_input_decomp <- data.table(dat$lt_input_sub)
#add all.cause mx
dat$mx_input_decom[,mx := death_total/population_py]
#add covid19 mx
dat$mx_input_decom[,Ri := death_covid/death_total]

#cheack
#dat$mx_input_decom[is.na(Ri)]

#view(dat$mx_input_decom)
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

#source Preston et al 2002
#mx = dat$mx_input_decom[sex %in% 'Female' & region_iso %in% 'BE']$mx
#x = dat$mx_input_decom[sex %in% 'Female' & region_iso %in% 'BE']$age_start
#nx = dat$mx_input_decom[sex %in% 'Female' & region_iso %in% 'BE']$age_width
#Ri = dat$mx_input_decom[sex %in% 'Female' & region_iso %in% 'BE']$Ri

cause.deleted.life.expectancy.fun  <-
  function (mx, x, nx, Ri, cond_age = 0) {
    
    #functions from master life table
    px = exp(-mx*{{nx}})
    lx = head(cumprod(c(1, px)), -1)
    dx = c(-diff(lx), tail(lx, 1))
    Lx = nx[-length(nx)] *lx[-1]+.5*dx[-length(nx)]
    Lx = c(Lx,(dx/mx)[length(nx)])
    #Lx = ifelse(mx==0, lx*nx, dx/mx)
    Tx = rev(cumsum(rev(Lx)))
    ex = Tx/lx
    
    #ASDLT 
    new.px = px^(1-Ri)
    new.lx = lx/lx
    new.lx = head(cumprod(c(1, new.px)), -1)
    new.dx = c(-diff(new.lx), tail(new.lx, 1))
    new.Lx = nx[-length(nx)] *new.lx[-1]+.5*new.dx[-length(nx)]
    new.Lx = c(new.Lx,new.lx[length(new.lx)]/(mx[length(mx)]*(1-Ri[length(Ri)])))
    new.Tx = rev(cumsum(rev(new.Lx)))
    new.ex = new.Tx/new.lx
    
    -(new.ex - ex)[1]
    
  }


# calculate life expectancy and cause deleted life expectancy ------------------------------------------


life_expectancy_results_R1 <- dat$mx_input_decomp[, .(e0 = life.expectancy.fun(mx = mx,x = age_start,nx = age_width,cond_age = 0),
                                                          ctb = cause.deleted.life.expectancy.fun(mx = mx,
                                                                                                               x = age_start,
                                                                                                               nx = age_width,
                                                                                                               Ri=Ri,
                                                                                                               cond_age = 0)), 
                                                      by = .(region_iso,sex,year)]

life_expectancy_results_R1
names(life_expectancy_results_R1)[1] <- 'code'


# Make a plot -------------------------------------------------------------

#load data from paper's decomposition
df_dec_age_cause <- read_rds("{wd}/out/df_dec_age_cause.rds" %>% glue)

original.paper <- df_dec_age_cause %>%
  left_join(ids) %>%
  drop_na(name) %>%
  filter(period_cause == "2019-20 COVID") %>%
  group_by(name,code, sex) %>%
  summarise(ctb = ctb %>% sum(na.rm = T)) %>%
  ungroup() %>%
  mutate(name = name %>% fct_reorder(ctb %>% desc)) 
original.paper$method <- 'decomposition'


cause.deleted.plot.data <-   life_expectancy_results_R1 %>%
  left_join(ids)

cause.deleted.plot.data <- cause.deleted.plot.data[,c('name','code','sex','ctb')]
cause.deleted.plot.data$method <- 'cause deleted'


### make plot


fig.R1.data <- rbind(cause.deleted.plot.data,original.paper)  

  ggplot(data = fig.R1.data, aes(ctb, name, fill = method))+
  geom_bar(stat = 'identity',position = position_dodge())+
  scale_fill_manual(values = c("#B5223BFF", "#64B6EEFF"))+
  scale_y_discrete(position = "right")+
  scale_x_continuous(position = "top", breaks = c(-1.5, -1, -.5, 0))+
  facet_wrap(~sex, ncol = 2)+
  theme_minimal(base_family = font_rc)+
  theme(
    legend.position = "bottom",
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.spacing.x = unit(2, "lines"),
    strip.text = element_blank(),
    axis.text.y = element_text(face = 2),
  )+
  labs(
    x = "Losses in life expectancy at birth by method",
    y = NULL
  )

  fig.R1 <- last_plot()

ggsave("{wd}/out/fig-R1.pdf" %>% glue, fig.R1, width = 6, height = 3, device = cairo_pdf)
