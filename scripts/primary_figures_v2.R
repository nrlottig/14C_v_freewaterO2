rm(list=ls())
library(lubridate)
library(patchwork)
library(tidyverse)
library(mcr)
library(MethComp)
library(zoo)

date_matrix = tibble(date=seq(from=as_date("2007-01-01"),to=as_date("2017-12-31"),by="1 day")) %>% 
    mutate(yday = yday(date),year=year(date)) %>% 
    filter(yday>=152 & yday <= 245)

sp_metab <- read_csv("model/output/sparkling_varying_daily_full.csv") %>% 
    filter(name=="GPP" | name=="NEP") %>% 
    drop_na(index) 
sp_metab <- sp_metab %>% expand(yday=full_seq(yday,1),name,year) %>% left_join(sp_metab) %>% arrange(name,year,yday) %>% left_join(date_matrix) %>% 
    select(name,date,middle,upper,lower) %>% 
    mutate(lake="Sparkling")

sp_c14 <- read_tsv("data/sp_daily14c_prod.txt") %>% 
    mutate(year = year(date),yday=yday(date)) %>% filter(type=="m3")
sp_c14 <- sp_metab %>% left_join(sp_c14) %>% 
    drop_na(middle) %>% 
    filter(name=="GPP") %>% 
    mutate(delta_p = abs(middle-s62)) %>% 
    mutate(event=as_date(event)) %>% 
    mutate(date_diff = abs(event-date)) %>% 
    group_by(event) %>% 
    filter(date_diff == min(date_diff)) %>% 
    filter(delta_p ==min(delta_p)) %>% 
    ungroup() %>% 
    select(date,p80,s62) %>% 
    mutate(lake="Sparkling")

tr_metab <- read_csv("model/output/trout_varying_daily_full.csv") %>% 
    filter(name=="GPP" | name=="NEP") %>% 
    drop_na(index) 
tr_metab <- tr_metab %>% expand(yday=full_seq(yday,1),name,year) %>% left_join(tr_metab) %>% arrange(name,year,yday) %>% left_join(date_matrix) %>% 
    select(name,date,middle,upper,lower) %>% 
    mutate(lake="Trout")

tr_c14 <- read_tsv("data/tr_daily14c_prod.txt") %>% 
    mutate(year = year(date),yday=yday(date)) %>% filter(type=="m3") %>% 
    filter(yday(date)>=152)
tr_c14 <- tr_metab %>% left_join(tr_c14) %>% 
    drop_na(middle) %>% 
    filter(name=="GPP") %>% 
    mutate(delta_p = abs(middle-s62)) %>% 
    mutate(event=as_date(event)) %>% 
    mutate(date_diff = abs(event-date)) %>% 
    group_by(event) %>% 
    filter(date_diff == min(date_diff)) %>% 
    filter(delta_p ==min(delta_p)) %>% 
    ungroup() %>% 
    select(date,p80,s62) %>% 
    mutate(lake="Trout")

ca_metab <- read_csv("model/output/castle_varying_daily_full.csv") %>% 
    filter(name=="GPP" | name=="NEP") %>% 
    drop_na(index)
ca_metab <- ca_metab %>% expand(yday=full_seq(yday,1),name,year) %>% left_join(ca_metab) %>% arrange(name,year,yday) %>% left_join(date_matrix) %>% 
    select(name,date,middle,upper,lower) %>% 
    mutate(lake="Castle")

ca_c14 <- read_csv("data/ca_daily14c_prod.csv") %>% 
    rename(date = Date) %>% 
    mutate(date = mdy(date)) %>% 
    mutate(yday=yday(date),year=year(date)) %>% 
    rename(p80 = mgc_l_d) %>% 
    mutate(p80 = p80*1000)
ca_c14 <- ca_metab %>% filter(name=="GPP") %>% left_join(ca_c14) %>%
    drop_na() %>% 
    select(date,lake,p80) %>% 
    mutate(lake="Castle")

ac_metab <- read_csv("model/output/acton_daily_full.csv") %>% 
    filter(name=="GPP" | name=="NEP") %>% 
    drop_na(index)

ac_metab <- ac_metab %>% expand(yday=full_seq(yday,1),name,year) %>% left_join(ac_metab) %>% arrange(name,year,yday) %>% left_join(date_matrix) %>% 
    select(name,date,middle,upper,lower,year,yday) %>% 
    mutate(lake="Acton")
ac_metab <- ac_metab %>% filter(!(yday %in% c(194:196) & year == 2011)) %>% 
    select(-yday,-year)

ac_c14 <- read_csv("data/ac_daily14c_prod.csv") %>% 
    mutate(yday=yday(date),year=year(date)) %>% 
    rename(p80 = ppr)
ac_c14 <- ac_metab %>% filter(name=="GPP") %>% left_join(ac_c14) %>%
    drop_na() %>% 
    select(date,lake,p80) %>% 
    mutate(lake="Acton")

ntl <- rbind(sp_c14,tr_c14)

####Primary Data Frames
dat_c14 <- ca_c14 %>% rbind(ntl %>% select(lake,date,p80)) %>%
    rbind(ac_c14) %>% 
    mutate(p80=p80/12.011) %>% 
    mutate(year=year(date),yday=yday(date))
dat_metab <- rbind(sp_metab,tr_metab,ca_metab,ac_metab) %>% 
    mutate(year = year(date),yday=yday(date))

#7-day Average Data
dat_avg <- dat_metab %>% 
    arrange(lake,date) %>% 
    filter(name=="GPP") %>% 
    group_by(lake,year) %>% 
    mutate(avg = rollapply(middle,width=7,median,align="center",fill=NA)) %>% 
    ungroup()

#biplot data for discreate days
biplot <- dat_c14 %>% left_join(dat_metab %>% filter(name=="GPP")) %>% select(lake,year,middle,p80,upper,lower) %>% drop_na()

#biplot data for average dayes
biplot_avg <- dat_c14 %>% left_join(dat_avg %>% filter(name=="GPP")) %>% select(lake,year,middle,p80,upper,lower,avg) %>% drop_na()

#Overview Plot of Time Series (Figure 1)
p1 <- ggplot(data = dat_metab %>% filter(name=="GPP") ,aes(yday, middle/1.25, color = name))+
    geom_hline(yintercept = 0, size = 0.3, color = "gray50")+
    geom_ribbon(aes(ymin = lower/1.25, ymax = upper/1.25, fill = name),
                linetype = 0, alpha = 0.2)+
    geom_line() +
    geom_point(data = dat_c14,aes(x=yday,y=p80,color="C14")) +
    scale_color_manual(values = c("black","dodgerblue","firebrick")) +
    scale_fill_manual(values = c("dodgerblue","firebrick"),guide=FALSE) +
    theme_bw() +
    labs(y=expression(mmol~C~m^-3~d^-1),color="",x="Day of Year") +
    facet_wrap(vars(lake,year),scales = "free_y",ncol=4) +
    theme(strip.text.x = element_text(size = 8))
p1 
ggsave(plot = p1,"graphics/metabolism.pdf",width=6.5,height=7.5,dpi=300)

#Biplot of discreate Days
p2 <- ggplot(data = biplot,aes(x=p80,y=middle/1.25)) + 
    geom_errorbar(aes(ymin=lower/1.25, ymax=upper/1.25),col="lightgrey") +
    geom_point() +
    geom_abline(slope = 1,intercept = 0) +
    theme_bw()+
    coord_fixed(xlim = c(1,600),ylim=c(1,600)) +
    theme(aspect.ratio=1) +
    labs(y =  expression(GPP~"("*mmol~O[2]~m^{-3}~day^{-1}*")"),
         x = expression(C[14]~"("*mmol~C~m^{-3}~day^{-1}*")")) +
    scale_x_log10() +
    scale_y_log10()
p2

#biplot of 7 day median GPP
p3 <- ggplot(data = biplot_avg,aes(x=p80,y=avg/1.25)) + 
    geom_point() +
    geom_abline(slope = 1,intercept = 0) +
    theme_bw()+
    coord_fixed(xlim = c(1,600),ylim=c(1,600)) +
    theme(aspect.ratio=1) +
    labs(y =  expression(GPP~"("*mmol~O[2]~m^{-3}~day^{-1}*")"),
         x = expression(C[14]~"("*mmol~C~m^{-3}~day^{-1}*")")) +
    scale_x_log10() +
    scale_y_log10()
p3

# Distribution Comparision
p4 <-ggplot(data=biplot %>% 
               select(middle,p80) %>% 
               mutate(middle=middle/1.25) %>% 
               rename(GPP = middle,C14=p80) %>% 
               gather(value=value,key=model)) + 
    geom_density(aes(x=value,col=model,fill=model),alpha=0.5) +
    scale_color_manual(values = c("black","dodgerblue"),guide=FALSE)+
    scale_fill_manual(values = c("black","dodgerblue"),guide=FALSE) +
    theme_bw() +
    theme(aspect.ratio=1) +
    labs(x = expression(Production~"("*mmol~m^{-3}~day^{-1}*")")) +
    scale_x_log10()
p4

combined_plots <- (p2/p4)
combined_plots
combined_plots + plot_annotation(tag_levels = "A", theme = theme(plot.caption = element_text(size = 10, hjust = 0)))

ggsave("graphics/point_estimates.png",width=3,height=5.5,dpi=300)


biplot_by_lake <- biplot %>% 
    mutate(upper = upper/1.25,lower=lower/1.25, middle=middle/1.25) %>% 
    mutate(one_to_one = ifelse((p80 < (lower) | p80 >(upper)),1,0)) 
    
point_summary <- biplot_by_lake %>% group_by(lake) %>% 
    summarize(n=n(),fraction = sum(one_to_one)/n())

p6 <- ggplot(data = biplot_by_lake,aes(x=p80,y=middle,color=factor(one_to_one))) + 
    geom_errorbar(aes(ymin=lower, ymax=upper),col="lightgrey") +
    geom_point() +
    geom_abline(slope = 1,intercept = 0) +
    theme_bw()+
    coord_fixed(xlim = c(1,600),ylim=c(1,600)) +
    theme(aspect.ratio=1) +
    labs(y =  expression(GPP~"("*mmol~C~m^{-3}~day^{-1}*")"),
         x = expression(C[14]~"("*mmol~C~m^{-3}~day^{-1}*")")) +
    scale_x_log10() +
    scale_y_log10() +
    facet_wrap(vars(lake))
p6


m2 <- mcreg(log10(biplot$p80+1),log10(biplot$middle/1.25+1),method.reg="PaBa",method.ci = "nestedbootstrap",method.bootstrap.ci = "tBoot")
MCResult.plot(x=m2, add.legend=TRUE,equal.axis=TRUE,xn=50,ci.area = TRUE,x.lab="14 C",y.lab = "Free-water")
getCoefficients(m2)
m3 <- mcreg(log10(biplot_avg$p80+1),log10(biplot_avg$middle/1.25+1),method.reg="PaBa",method.ci = "nestedbootstrap",method.bootstrap.ci = "tBoot")
MCResult.plot(x=m3, add.legend=TRUE,equal.axis=TRUE,xn=50,ci.area = TRUE,x.lab="14 C",y.lab = "Free-water")
getCoefficients(m3)

out <- PBreg(log10(biplot$p80+1), y = log10(biplot$middle/1.25+1))
plot(out)
