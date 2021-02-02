rm(list=ls())
library(plyr)
library(tidyverse)
library(lubridate)
library(zoo)

castle_c14 <- read_csv("data/castleppr.csv") 
mixed_depth <- read_csv("data/model_input/sonde_prep_castle_2014_2017.csv") %>% 
    select(year,yday,z) %>% 
    dplyr:::group_by(year,yday) %>% 
    dplyr:::summarize(zmin=mean(z,na.rm=TRUE),zavg=mean(z,na.rm=TRUE)) %>% 
    mutate(zrd=round_any(zmin,0.5,)) %>%
    drop_na() %>% 
    select(year,yday,zrd)

detach(package:plyr)
mixed_depth <- mixed_depth %>% rename(z=zrd)

dates <- unique(castle_c14$Date)
for(i in 1:length(dates)) {
    temp <- data.frame(Date = dates[i],Depth =seq(0,10,0.5))
    if(i ==1) depth_matrix = temp else depth_matrix = rbind(depth_matrix,temp)
}

light <- read_csv("data/LightFraction.csv") %>% 
    mutate(Date = dmy(Date))

temp <- castle_c14 %>% full_join(depth_matrix) %>% arrange(Date,Depth) %>% 
    mutate(Date = mdy(Date)) %>% 
    group_by(Date) %>% 
    mutate(`PPR(mgC*m3*h)`=na.approx(`PPR(mgC*m3*h)`)) %>% 
    ungroup() %>% 
    rename(p80 = `PPR(mgC*m3*h)`) %>% 
    mutate(year = year(Date), yday=yday(Date)) %>% 
    mutate(p80 = p80*4) %>% 
    left_join(light) %>% 
    mutate(ppr = p80/Fraction) %>% 
    left_join(mixed_depth) %>% 
    drop_na(z) %>% 
    group_by(Date) %>%
    filter(Depth <= z[1]) %>%
    summarize(ppr = mean(ppr,na.rm=TRUE))

write_csv(temp,"data/c14/ca_daily14c_prod.csv")           

