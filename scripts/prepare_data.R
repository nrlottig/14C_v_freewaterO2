#========== 
#========== Preliminaries
rm(list=ls())
# load packages
library(tidyverse)
library(lubridate)
library(LakeMetabolizer)
library(rstan)
library(patchwork)
library(plotly)
source("scripts/helper_functions.R")

#=========================================== 
# Get and process high frequency sensor data
#===========================================
    #Lakes are processed one lake at a time
lake <- "acton"
lake_id <- get.abrev(lake)[1]
max_d <- as.numeric(get.abrev(lake)[2])
lake.area <- as.numeric(get.abrev(lake)[3])
out.time.period <- "60 min"
tz <-  "US/Eastern"#"US/Central"#"US/Pacific"#"US/Central"

sonde = list.files(paste("data/sonde_raw/clean_data/",lake,sep=""), full.names = T) %>%
    lapply(read_csv) %>%
    bind_rows()
if(lake == "castle") sonde <- sonde %>% drop_na(datetime)

#examine years in raw data
unique(sonde$year)
years = c(2010:2012,2014) #select years for which to generate model input files
sonde <- sonde %>% filter(year %in% years) #trim dataset to years


#process lake files and drop data that is bad or outside of summer stratified
#time period
if(lake == "sparkling"){
    data <- sonde %>% 
      mutate(datetime = ymd_hms(datetime,tz=tz)) %>% 
    filter((year != 2010 & yday >= 152 & yday <= 245) |
    (year == 2010 & ((yday >=152 & yday <= 189) | (yday >=202 & yday <= 245)))) %>%
      drop_na()  
}

if(lake == "trout") {
  data <- sonde %>% select(-par_sp) %>% 
    mutate(datetime = ymd_hms(datetime,tz=tz)) %>% 
    # filter(yday >=152 & yday <= 245) %>% 
    filter((year == 2007 & yday >=166 & yday <= 245) |
             (year == 2008 & yday >=170 & yday <=245) |
             (year == 2009 & yday >=152 & yday <=245) |
             (year == 2010 & ((yday >=152 & yday <= 195) | (yday >=205 & yday <= 245))) |
             (year == 2012 & yday >=152 & yday <= 245)) %>% 
    #          (year == 2009 & ((yday >=152 & yday <= 180) | (yday >=190 & yday <= 245))) |
    #          (year == 2010 & ((yday >=152 & yday <= 197) | (yday >=205 & yday <= 245))) |
    #          (year == 2012 & yday >=152 & yday <= 245)) %>% 
    drop_na()
}
if(lake == "castle"){
  data <- sonde %>% 
    mutate(datetime = ymd_hms(datetime,tz=tz)) %>%
    filter((year == 2014 & yday >= 152 & yday <=245) |
             (year==2015 & yday >=172 & yday <=245) |
             (year == 2016 & yday >= 188 & yday <=245) |
             (year == 2017 & yday >=212 & yday <= 245)) %>% 
    drop_na()  
}

if(lake == "acton") {
  data <- sonde %>% 
    mutate(datetime = ymd_hms(datetime,tz=tz)) %>%
    filter(yday >=152 & yday <= 245) %>% 
    drop_na()
}

data <- data %>% 
  group_by(year,yday) %>%
  mutate(do = ifelse(z<0.5,NA,do)) %>%  #exclude observations below DO sensor
  #this needs to change for Castle lake which was 3m
  mutate(obs = sum(!is.na(do))) %>%       #identify and filter records that have < 23 hrs of data 
  ungroup()

freq <- nrlmetab::calc.freq(data$datetime) # determine data frequency obs/day
    
data <- data %>% filter(obs>=(freq-(freq/24*2))) %>% #allow for 2 hours
    mutate(k600 = k.vachon.base(wnd = wspeed,lake.area = lake.area)) %>% #estimate K in m/day
    mutate(kgas = k600.2.kGAS.base(k600 = k600,temperature = wtemp,gas = "O2")) %>%  #m/d
    mutate(k = (kgas/freq)/z) %>% #convert gas to T^-1
    select(-kgas,-k600,-obs)

#setting exchange to 0 in castle if sensor was shallower than z
#these data weren't used so not relavent.
if(lake == "castle") { 
  data <- data %>% 
    mutate(k = ifelse(z<3,0,k))
}
#
#==========
#========== Prepare for data analysis
#==========

# prepare data (derived from Phillips 2020)
sonde_prep = data %>%
    arrange(year, yday, hour) %>%
    # for each year, create identifier for uninterrupted stretches of observations
    group_by(year) %>%
    mutate(i = ifelse(is.na(do)==T, 1, 0), 
           j = c(1,abs(diff(i)))) %>% 
    filter(is.na(do)==F) %>%
    mutate(series = cumsum(j)) %>% 
    ungroup() %>%
    # create unique index for each series
    # remove series with fewer than 24 observations
    mutate(unique_series = year + series/length(unique(series))) %>%
    group_by(unique_series) %>%
    mutate(series_length = length(unique_series)) %>%
    ungroup() %>%
    # recreate series index and make unique index for days
    # create index for observations (for joining later)
    # replace 0 par_int with smallest non-zero value
    mutate(unique_series = as.factor(unique_series) %>% as.numeric(),
           unique_day = paste(year, yday) %>% as.factor() %>% as.numeric(),
           index = 1:length(do),
           par_int = ifelse(par_int==0,0.00001, par_int)) %>%
    select(-i, -j) 

# return missing observations for check
sonde_check = data %>% 
    expand(year,yday,hour) %>%
    full_join(sonde_prep) %>%
    arrange(year,yday)

ggplot(sonde_check,aes(x=datetime,y=do)) + geom_point(size=0.2) + geom_line() + facet_wrap(vars(year),scales="free_x")

# export prepared data
if(length(years) == 1) {
  sonde_check %>%
  write_csv(paste("analyses/int_par/model_fit/input/sonde_prep_",lake,"_",years,".csv",sep =""))
} else {
  sonde_check %>%
    write_csv(paste("data/model_input/sonde_prep_",lake,"_",min(years),"_",max(years),".csv",sep =""))
}

#==========
#========== Package data 
#==========

# define variables in evnironment 
o2_freq = freq;
o2_obs = 1000*sonde_prep$do # convert to mg m^-3
o2_eq = 1000*sonde_prep$do_eq # convert to mg m^-3
light = sonde_prep$par_int
temp = sonde_prep$wtemp
wspeed = sonde_prep$wspeed
# sch_conv = sonde_prep$sch_conv
map_days = sonde_prep$unique_day
k = sonde_prep$k
if(length(years) == 1) {
days_per_year = array(c({sonde_prep %>%
        group_by(year) %>%
        summarize(value = length(unique(unique_day)))}$value), dim = 1) #,dim = 1
} else {
  days_per_year = array(c({sonde_prep %>%
      group_by(year) %>%
      summarize(value = length(unique(unique_day)))}$value)) #,dim = 1 
}
obs_per_series = c({sonde_prep %>%
        group_by(unique_series) %>%
        summarize(value = length(unique_series))}$value) 
obs_per_day = c({sonde_prep %>%
        group_by(unique_day) %>%
        summarize(value = length(unique_day))}$value) 
z = sonde_prep$z
n_obs = length(o2_obs)
n_series = length(obs_per_series) 
n_days = sum(days_per_year)
n_years = length(days_per_year)

# export as .R
if(length(years)>1) {
  stan_rdump(c("o2_freq","o2_obs","o2_eq","light","temp","wspeed","map_days","obs_per_series","days_per_year",
               "obs_per_day", "z","k","n_obs","n_series","n_days","n_years"),
             file=paste("model/input/",lake,"_",min(years),"_",max(years),"_sonde_list.R",sep=""))
} else {
  stan_rdump(c("o2_freq","o2_obs","o2_eq","light","temp","wspeed","map_days","obs_per_series","days_per_year",
               "obs_per_day", "z","k","n_obs","n_series","n_days","n_years"),
             file=paste("model/input/",lake,"_",years,"_sonde_list.R",sep=""))
}

