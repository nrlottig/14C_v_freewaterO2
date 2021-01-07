rm(list=ls())
library(tidyverse)
library(lubridate)
library(zoo)
library(plyr)
acton_c14 <- read_csv("data/acton_c14.csv") 
mixed_depth <- read_csv("data/model_input/sonde_prep_acton_2010_2014.csv") %>% 
    select(year,yday,z) %>% 
    dplyr:::group_by(year,yday) %>% 
    dplyr:::summarize(z=mean(z,na.rm=TRUE)) %>% 
    mutate(z=round_any(z,0.5)) %>% 
    drop_na()
detach(package:plyr)

data <- acton_c14 %>% mutate(date = dmy(Date)) %>% 
    select(-X3,-Date)
ac_c14 <- data %>% select(date,mgCm2d)

data <- data %>% select(-mgCm2d) %>% 
    pivot_longer(cols = c(-date), names_to = "depth", values_to="value") %>% 
    mutate(depth = as.numeric(depth)) %>% 
    drop_na() %>% 
    mutate(year=year(date),yday=yday(date))

date_depth_matrix <- data %>% select(date,depth) %>% 
    group_by(date) %>% 
    mutate(depth = c(0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,NA)) %>% 
    drop_na()

temp <- data %>% full_join(date_depth_matrix) %>% arrange(date,depth) %>% 
    group_by(date) %>% 
    mutate(year=year[1],yday=yday[1]) %>% 
    mutate(value=na.approx(value)) %>% 
    ungroup() %>% 
    left_join(mixed_depth) %>% 
    drop_na(z) %>% 
    group_by(date) %>% 
    filter(depth <= z[1]) %>% 
    summarize(ppr = mean(value))

write_csv(temp,"data/ac_daily14c_prod.csv")           
