library(here); library(glue)
library(data.table)

wd <- here()

source(glue('{wd}/src/05-lt_description.R'))

## load lifetable data
unique(dat$lt_input_85$region_iso)

## number of countries in the anlaysis
cnst$regions_for_analysis
length(cnst$regions_for_analysis)

LT <- data.table(dat$lt_85)

#subset to ages 0,60,80
LT <- LT[x %in% c(0,60), c('region_iso','sex','x','year','ex')]

###### ugly table 1 for levels of life expectancy
LT[,ex:= round(ex,2)]

table.1.ugly <- dcast.data.table(data = LT[year %in% c(2015,2019,2020)], region_iso + sex ~ x + year ,value.var = 'ex')
names(table.1.ugly)[3:8] <- paste0('e',names(table.1.ugly)[3:8] )

table.1.ugly <- table.1.ugly[!(is.na(e0_2019)),]

table.1.ugly <- table.1.ugly[order(sex,e0_2019),]

table.1.ugly[sex == 'Female']

table.1.ugly[sex == 'Male']

#######################

table.1.ugly <- table.1.ugly[order(sex,e60_2019),]

table.1.ugly[sex == 'Female']

table.1.ugly[sex == 'Male']

#################################

###### changes in life expectancy at birth

# change in e0 between 2015 and 2019
LT.0 <- LT[x == 0 & year %in% 2015:2019]
dif.LT0 <-LT.0[, .(dif.e0 = ex[length(ex)] - ex[1], dif.avg = (ex[length(ex)] - ex[1])/5 ), by = .(region_iso,sex)]
dif.LT0[order(sex,dif.e0)]

# change in e60 between 2015 and 2019
LT.60 <- LT[x == 60 & year %in% 2015:2019]
dif.LT60 <-LT.60[, .(dif.e60 = ex[length(ex)] - ex[1], dif.avg = (ex[length(ex)] - ex[1])/5 ), by = .(region_iso,sex)]
dif.LT60[order(sex,dif.e60)]

#changes in e0 between 2019 and 2020
LT.03 <- LT[x == 0 & year %in% c(2019,2020)]
dif.LT03 <-LT.03[, .(dif.e03 = ex[length(ex)] - ex[1]), by = .(region_iso,sex)]
dif.LT03[order(sex,dif.e03)]

#changes in e60 between 2019 and 2020
LT.04 <- LT[x == 60 & year %in% c(2019,2020)]
dif.LT04 <-LT.04[, .(dif.e03 = ex[length(ex)] - ex[1]), by = .(region_iso,sex)]
dif.LT04[order(sex,dif.e03)]


# change in e0 between 2015 and 2020
LT.02 <- LT[x == 0 & year %in% c(2015,2020)]
dif.LT02 <-LT.02[, .(dif.e02 = ex[length(ex)] - ex[1]), by = .(region_iso,sex)]
dif.LT02[order(sex,dif.e02)]


