#========== 
#========== Preliminaries
#=
rm(list=ls())
# load packages
library(tidyverse)
library(lubridate)
library(LakeMetabolizer)
library(rstan)
library(patchwork)
library(plotly)
source("helper_functions.R")

#=========================================== 
# Get and process high frequency sensor data
#===========================================
lake <- "trout"
lake_id <- get.abrev(lake)[1]
max_d <- as.numeric(get.abrev(lake)[2])
lake.area <- as.numeric(get.abrev(lake)[3])
out.time.period <- "60 min"
tz <-  "US/Central"#"US/Pacific"#"US/Central"


sonde = list.files(paste("data/sonde_raw/clean_data/",lake,sep=""), full.names = T) %>%
    lapply(read_csv) %>%
    bind_rows()
if(lake == "castle") sonde <- sonde %>% drop_na(datetime)
unique(sonde$year)



years = c(2007:2010,2012)

sonde <- sonde %>% filter(year %in% years)

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

data <- data %>% 
  group_by(year,yday) %>%
  mutate(obs = sum(!is.na(do))) %>%       #identify and filter records that have < 23 hrs of data 
  ungroup() %>%
  mutate(z = ifelse(z<=0.5,.5,z)) #can't have zero depth zmix

freq <- nrlmetab::calc.freq(data$datetime) # determine data frequency obs/day
    
data <- data %>% filter(obs>=(freq-(freq/24*2))) %>% #allow for 2 hours
    mutate(k600 = k.vachon.base(wnd = wspeed,lake.area = lake.area)) %>% #estimate K in m/day
    mutate(kgas = k600.2.kGAS.base(k600 = k600,temperature = wtemp,gas = "O2")) %>%  #m/d
    mutate(k = (kgas/freq)/z) %>% #convert gas to T^-1
    select(-kgas,-k600,-obs)

if(lake == "castle") { 
  data <- data %>% 
    mutate(k = ifelse(z<3,0,k))
}


ggplot(data=data,aes(x=yday,y=do)) + geom_line() + facet_wrap(vars(year),scales="free_x") +
  geom_point(aes(x=yday,y=z),col="blue",size=0.2)
ggplotly()

#If only a single year of data, trim data to data extent
# if(length(years) == 1) {
# # datetime_seq <- data.frame(datetime = seq(from=min(data$datetime),to=max(data$datetime),by=out.time.period)) %>% 
# #   mutate(year = year(datetime),
# #          yday = yday(datetime),
# #          hour = hour(datetime) + minute(datetime)/60 + 1)
# # 
# # data <- datetime_seq %>% left_join(data)
# # rm(datetime_seq)
# } else {data <- datetime_matrix %>% left_join(data)}

if(length(years) == 1) {
#===========================================
# Generate overview plots of data
#===========================================
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/31/29/5a5a5606737d760b61c43bc59460ccc9"
infile1 <- tempfile()
download.file(inUrl1,infile1,method="curl")
lter.events <-read.csv(infile1,header=F,skip=1,sep=","  ,quot='"',col.names=c(
                 "lakeid",
                 "year4",
                 "daynum",
                 "sampledate",
                 "sta",
                 "secview",
                 "secnview",
                 "timeon",
                 "timeoff",
                 "airtemp",
                 "windir",
                 "windspd",
                 "waveht",
                 "cloud",
                 "ice"), check.names=TRUE) %>%
  select(lakeid,sampledate,timeon) %>%
  filter(lakeid == lake_id) %>% #need to work on making this automatic
  drop_na() %>%
  mutate(timeon = sprintf("%04d", as.integer(timeon))) %>%
  mutate(timeon = sub("(.{2})(.*)","\\1:\\2",timeon)) %>%
  mutate(timeon = paste(timeon ,":00",sep="")) %>%
  mutate(datetime = round_date(ymd_hms(paste(sampledate,timeon,sep=" ")),out.time.period)) # Need to make this automatic with freq

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/29/27/03e232a1b362900e0f059859abe8eb97"
infile1 <- tempfile()
download.file(inUrl1,infile1,method="curl")

diss_o_measurements <-read.csv(infile1,header=F,skip=1,sep=",",quot='"',col.names=c(
                 "lakeid",
                 "year4",
                 "daynum",
                 "sampledate",
                 "depth",
                 "rep",
                 "sta",
                 "event",
                 "wtemp",
                 "o2",
                 "o2sat",
                 "deck",
                 "light",
                 "frlight",
                 "flagdepth",
                 "flagwtemp",
                 "flago2",
                 "flago2sat",
                 "flagdeck",
                 "flaglight",
                 "flagfrlight"    ), check.names=TRUE) %>%
  select(lakeid,sampledate,depth,o2,flago2,wtemp) %>%
  filter(lakeid == lake_id) %>% 
  drop_na(o2) %>%
  filter(flago2 == "") %>%
  filter(depth <=0.5) %>%
  group_by(sampledate) %>%
  summarize(o2 = mean(o2),wtemp = mean(wtemp)) %>%
  ungroup()

lter_do <- lter.events %>% left_join(diss_o_measurements) %>% drop_na(o2)

p1 <- data %>%
    mutate(time = yday + hour/24) %>%
    {ggplot(.,aes(datetime, do))+
            geom_line(data = . %>% filter(is.na(do)==F), size = 0.3)+
            geom_point(size = 0.5)+
            geom_point(data = lter_do %>% filter(datetime > min(data$datetime)) %>%
                         filter(datetime < max(data$datetime)),aes(x=datetime,y=o2),color="red") +
            theme_bw()}
p2 <- data %>%
    {ggplot(.,aes(datetime, wtemp))+
            geom_line(data = . %>% filter(is.na(do)==F), size = 0.3)+
            geom_point(size = 0.5)+
          geom_point(data = lter_do %>% filter(datetime > min(data$datetime)) %>%
                     filter(datetime < max(data$datetime)),aes(x=datetime,y=wtemp),color="blue") +
            theme_bw()}
p3 <- data %>%
    {ggplot(.,aes(datetime, z))+
            geom_line(data = . %>% filter(is.na(do)==F), size = 0.3)+
            geom_point(size = 0.5)+
            scale_y_reverse() +
            labs(y = "Epi Depth")+
            theme_bw()}
p4 <- data %>%
  {ggplot(.,aes(datetime, wspeed))+
      geom_line(data = . %>% filter(is.na(do)==F), size = 0.3)+
      geom_point(size = 0.5)+
      theme_bw()}
p5 <- data %>%
  {ggplot(.,aes(datetime, k))+
      geom_line(data = . %>% filter(is.na(do)==F), size = 0.3)+
      geom_point(size = 0.5)+
      theme_bw()}

p6 <- data %>%
  {ggplot(.,aes(datetime, par_int))+
      geom_line(data = . %>% filter(is.na(do)==F), size = 0.3)+
      geom_point(size = 0.5)+
      theme_bw()}

(p1 + p2) / (p3 + p4) / (p5 + p6)

ggsave(paste("graphics/lake_data/",lake,"_",years,"_inputs.png",sep=""),dpi=300,width=6.5,height=9.5,units="in")
}
#==========
#========== Prepare for data analysis
#==========

# prepare data
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

# ggsave("castle_do.png")
# # check unique_series
# sonde_check %>%
#     mutate(time = yday + hour/24) %>%
#     ggplot(aes(time, do, color=factor(unique_series)))+
#     facet_wrap(~year)+
#     geom_line()+
#     scale_color_discrete(guide = F)+
#     theme_bw()
# 
# # check unique_days
# sonde_check %>%
#     mutate(time = yday + hour/24) %>%
#     filter(unique_day %in% (50 + c(1:10))) %>%
#     ggplot(aes(time, do, color=factor(unique_day)))+
#     facet_wrap(~year, scale = "free_x", nrow=2)+
#     geom_line()+
#     theme_bw()

# export prepared data
if(length(years) == 1) {
  sonde_check %>%
  write_csv(paste("analyses/int_par/model_fit/input/sonde_prep_",lake,"_",years,".csv",sep =""))
} else {
  sonde_check %>%
    write_csv(paste("analyses/int_par/model_fit/input/sonde_prep_",lake,"_",min(years),"_",max(years),".csv",sep =""))
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
             file=paste("model/input/alt_model/",lake,"_",min(years),"_",max(years),"_sonde_list.R",sep=""))
} else {
  stan_rdump(c("o2_freq","o2_obs","o2_eq","light","temp","wspeed","map_days","obs_per_series","days_per_year",
               "obs_per_day", "z","k","n_obs","n_series","n_days","n_years"),
             file=paste("model/input/alt_model/",lake,"_",years,"_sonde_list.R",sep=""))
}

