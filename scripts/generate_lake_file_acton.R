library(tidyverse)
library(lubridate)
library(LakeMetabolizer)
library(patchwork)
library(gridExtra)
library(nrlmetab)
library(readxl)
library(zoo)
library(plotly)

rm(list=ls())
source("scripts/helper_functions.r")
lake <- "acton"
lake_id <- get.abrev(lake)[1]
max_d <- as.numeric(get.abrev(lake)[2])
year <- 2014


######
######Process water profile data
######
dt1 <- read_excel("data/Acton_Lake_high_freq_temp_for_Noah_Lottig.xlsx")
names(dt1) <- c("datetime","wtr_0","wtr_0.5","wtr_1","wtr_1.5","wtr_2","wtr_2.5","wtr_3","wtr_4","wtr_5","wtr_6","wtr_7")

dt1 <- dt1 %>% filter(year(datetime)==year) %>% 
    select(-wtr_2.5)

dt1 <- dt1 %>% pivot_longer(cols = c(-datetime)) %>% 
    rename(depth = name) %>% 
    mutate(depth = str_replace(depth,"wtr_","")) %>% 
    mutate(depth = as.numeric(depth)) %>% 
    rename(wtemp = value) %>% 
    drop_na(wtemp) %>% 
    filter(wtemp >0)

dt1 <- dt1 %>% 
    mutate(daynum = yday(datetime)) %>% 
    arrange(datetime) %>% 
    filter(!(daynum %in% range(daynum))) #filters out first and last day 
    
#examine data and filter to stratified time period
p1 <- ggplot(dt1,aes(x=datetime,y=wtemp,color=factor(depth))) + geom_line()
p1
ggplotly(p1)
dt1 <- dt1 %>% filter(daynum > 129) %>% filter(daynum < 253) %>% select(-daynum) #filter to stratified time period

dt1 <- dt1 %>% pivot_wider(values_from = wtemp,names_from=depth,names_prefix="wtr_")

wtr.heat.map(as.data.frame(dt1))

zmix <- ts.meta.depths(dt1,seasonal=TRUE,na.rm=TRUE) %>% 
    mutate(year = year(datetime),yday=yday(datetime))

date_matrix <- zmix %>% expand(year, datetime=full_seq(datetime,15*60))
zmix <- date_matrix %>% left_join(zmix) %>%  #These lines are the one that do the linear extrapolation
    mutate(top = na.approx(top)) %>% 
    rename(z = top)

zmix.clean <- tibble(datetime=seq(from=floor_date(min(zmix$datetime), unit = "hour"),
                                           to=ceiling_date(max(zmix$datetime),"hour"),
                                           by="1 min")) %>% left_join(zmix) %>% 
    mutate(top.clean = rollapply(data = z, 
                                 width = 1441, 
                                 FUN = mean, 
                                 align = "center",
                                 fill= NA,
                                 na.rm = T,
                                 partial = TRUE)) %>% 
    right_join(tibble(datetime=seq(from=floor_date(min(zmix$datetime), unit = "hour"),
                                   to=ceiling_date(max(zmix$datetime),"hour"),
                                   by="60 min")))

ggplot(data = zmix.clean,aes(x=datetime,y=z)) + geom_line(color="grey") + 
    geom_line(data = zmix.clean,aes(x=datetime,y=top.clean),color="red") + scale_y_reverse()



######
######Process sensor data
######
dt1 <- readxl::read_excel("data/acton_raw_data.xlsx") %>% 
    filter(year(datetime)==year) %>% rename(do_raw = do.obs) %>% 
    rename(do_wtemp = wtemp) %>% 
    rename(wind_speed_2m = wspeed)


##### DO DATA PREP
do.ts <- dt1 %>% select(datetime,do_raw)
ggplot(data = do.ts,aes(x=datetime,y=do_raw)) + geom_line()

describe.ts(do.ts)
do.ts.clean <- trim.outliers(data = do.ts,width = 7,sd.dev = 3)
ggplot(data = do.ts.clean,aes(x=datetime,y=do_raw)) +
    geom_line(data = do.ts,aes(x=datetime,y=do_raw)) +
    geom_line(col="red")
do.ts.avg <- aggregate.data(data = do.ts.clean,time.step = 60)
ggplot(data = do.ts.avg,aes(x=datetime,y=do_raw)) + 
    geom_line(data=do.ts.clean,aes(x=datetime,y=do_raw)) +
    geom_line(color="red")

do.ts.drift <- do.ts.avg

rm(list=c("do.ts.avg","do.ts.clean","do.ts"))

##### WTEMP DATA PREP
do.wtr <- dt1 %>% select(datetime,do_wtemp)
ggplot(data = do.wtr,aes(x=datetime,y=do_wtemp)) + geom_line()

describe.ts(do.wtr)
do.wtr.clean <- trim.outliers(data = do.wtr,width = 7,sd.dev = 5)
ggplot(data = do.wtr.clean,aes(x=datetime,y=do_wtemp)) +
    geom_line(data = do.wtr,aes(x=datetime,y=do_wtemp)) +
    geom_line(col="red")
do.wtr.avg <- aggregate.data(data = do.wtr.clean,time.step = 60)
ggplot(data = do.wtr.avg,aes(x=datetime,y=do_wtemp)) + 
    geom_line(data=do.wtr.clean,aes(x=datetime,y=do_wtemp)) +
    geom_line(color="red")
do.wtr.drift <- do.wtr.avg
rm(list=c("do.wtr.avg","do.wtr.clean","do.wtr"))

##### PAR DATA PREP
do.par <- dt1 %>% select(datetime,par)
ggplot(do.par,aes(x=datetime,y=par)) + geom_line()
do.par.avg <- aggregate.data(data = do.par,time.step = 60) %>% 
    mutate(par = ifelse(par <=0,0.00001,par))
ggplot(do.par.avg,aes(x=datetime,y=par)) + geom_line()
rm(do.par)

##### WIND DATA PREP

do.wind <- dt1 %>% select(datetime,wind_speed_2m)
ggplot(do.wind,aes(x=datetime,y=wind_speed_2m)) + geom_line()
do.wind.avg <- aggregate.data(data = do.wind,time.step = 60)
ggplot(data = do.wind.avg,aes(x=datetime,y=wind_speed_2m)) + geom_line()
rm(do.wind)

#### JOIN BOUY DATA

dat_aggregated <- do.ts.drift %>% full_join(do.wtr.drift) %>% full_join(do.par.avg) %>% full_join(do.wind.avg) %>% 
    rename(do = do_raw,wtemp = do_wtemp, wspeed = wind_speed_2m) 

create.plots(dat_aggregated,c("do","wtemp","par","wspeed"))
rm(list=c("do.par.avg","do.ts.drift","do.wtr.drift","do.wind.avg"))


#check to make sure there are no duplicated rows...unique values should be equal to number of rows
nrow(dat_aggregated) == length(unique(dat_aggregated$datetime))
#check to make sure there are no missing time stamps
length(seq(min(dat_aggregated$datetime),max(dat_aggregated$datetime),"60 min")) == nrow(dat_aggregated)

#Get individual data files
do <- dat_aggregated %>% select(datetime,do)
wtemp <- dat_aggregated %>% select(datetime,wtemp)
par <- dat_aggregated %>% select(datetime,par)
wspeed <- dat_aggregated %>% select(datetime,wspeed) 
zmix <- zmix.clean %>% select(datetime,top.clean) %>% rename(z=top.clean)

pressure <- readxl::read_excel("data/acton_raw_data.xlsx") %>% 
    select(datetime,pressure_mbar) %>% 
    filter(year(datetime)==year) %>% 
    rename(barom_pres=pressure_mbar)

wspeed <- wspeed %>% rename(wnd=wspeed)

U10 <- wind.scale(ts.data = wspeed,wnd.z = 2) %>% 
    rename(wspeed = wnd_10)


dat <- do  %>% 
    left_join(wtemp) %>%
    left_join(par) %>% 
    left_join(U10) %>% 
    left_join(pressure) %>% 
    left_join(zmix)

create.plots(dat,c("do","wtemp","par","wspeed","barom_pres","z"))
summary(dat)
#=========================================== 
# Generate calculated values
#===========================================
dat <- dat %>% 
    rename(wtr = wtemp)
    
dat <- dat %>% 
    mutate(do_eq = o2.at.sat(ts.data = dat[,c("datetime","wtr")],baro = dat$barom_pres)[,2]) %>% 
    rename(wtemp = wtr) %>% 
    select(-barom_pres) %>% 
    mutate(o2_sat = do/do_eq)

#estimate light extinction coefficents
ext.coef <- readxl::read_excel("data/acton_raw_data.xlsx", 
                               sheet = "zmix and ext_coef") %>% 
    select(datetime,ext_coef) %>% 
    filter(year(datetime)==year) %>% drop_na() %>% 
    mutate(year = year(datetime),yday=yday(datetime)) %>% 
    rename(extcoef = ext_coef) %>% 
    mutate(extcoef=as.numeric(extcoef))
ext.coef <- ext.coef %>% 
    expand(year, yday=full_seq(yday,1)) %>% 
    left_join(ext.coef) %>% 
    select(-datetime) %>% 
    mutate(extcoef=na.approx(extcoef))


ggplot(data = ext.coef,aes(x=yday,y=extcoef)) + geom_point() + geom_line()

#get missing times and add to dataframe
datetime_seq <- data.frame(datetime = seq(from=min(dat$datetime),to=max(dat$datetime),by="60 min")) %>% 
    mutate(year = year(datetime),
           yday = yday(datetime),
           hour = hour(datetime) + minute(datetime)/60 + 1)

dat <- datetime_seq %>% 
    left_join(dat) %>% 
    left_join(ext.coef) %>% 
    mutate(par_int = (par - par*exp(-extcoef*z))/(extcoef*z)) %>% 
    select(-extcoef)

create.plots(dat,c("do","wtemp","par_int","wspeed","o2_sat","z"))


write_csv(dat,paste("data/sonde_raw/clean_data/",lake,"/data_",year,".csv",sep=""))
ß