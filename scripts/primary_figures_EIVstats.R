rm(list=ls())
library(lubridate)
library(patchwork)
library(tidyverse)
library(zoo)
library(mcr)
library(MethComp)
library(ggExtra)

#Get raw data files
date_matrix = tibble(date=seq(from=as_date("2007-01-01"),to=as_date("2017-12-31"),by="1 day")) %>% 
    mutate(yday = yday(date),year=year(date)) %>% 
    filter(yday>=152 & yday <= 245)

sp_metab <- read_csv("model/output/sparkling_varying_daily_full.csv") %>% 
    filter(name=="GPP" | name=="NEP") %>% 
    drop_na(index) 
sp_metab <- sp_metab %>% expand(yday=full_seq(yday,1),name,year) %>% left_join(sp_metab) %>% arrange(name,year,yday) %>% left_join(date_matrix) %>% 
    select(name,date,middle,upper,lower) %>% 
    mutate(lake="Sparkling")

sp_c14 <- read_tsv("data/c14/sp_daily14c_prod.txt") %>% 
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

tr_c14 <- read_tsv("data/c14/tr_daily14c_prod.txt") %>% 
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

ca_c14 <- read_csv("data/c14/ca_daily14c_prod.csv") %>% 
    rename(date = Date) %>% 
    mutate(yday=yday(date),year=year(date)) %>% 
    rename(p80 = ppr)
ca_c14 <- ca_metab %>% filter(name=="GPP") %>% left_join(ca_c14) %>%
    drop_na() %>% 
    select(date,lake,p80) %>% 
    mutate(lake="Castle")

ac_metab <- read_csv("model/output/acton_varying_daily_full.csv") %>% 
    filter(name=="GPP" | name=="NEP") %>% 
    drop_na(index)

ac_metab <- ac_metab %>% expand(yday=full_seq(yday,1),name,year) %>% left_join(ac_metab) %>% arrange(name,year,yday) %>% left_join(date_matrix) %>% 
    select(name,date,middle,upper,lower,year,yday) %>% 
    mutate(lake="Acton")
ac_metab <- ac_metab %>% filter(!(yday %in% c(194:196) & year == 2011)) %>% 
    select(-yday,-year)

ac_c14 <- read_csv("data/c14/ac_daily14c_prod.csv") %>% 
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
    mutate(year = year(date),yday=yday(date)) %>% 
    filter(name=="GPP") %>% 
    arrange(lake,date)

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
biplot_avg <- dat_c14 %>% left_join(dat_avg %>% filter(name=="GPP")) %>% select(lake,year,date,middle,p80,upper,lower,avg) %>% drop_na()

#Overview Plot of Time Series (Figure 1)
#mutate(date = as.Date(strptime(paste0(year, yday), format=“%Y %j”)))
p1 <- ggplot(data = dat_metab %>% filter(name=="GPP") ,aes(as.Date(yday, origin = as.Date("2019-01-01")), middle/1.25, color = name))+
    geom_hline(yintercept = 0, size = 0.3, color = "gray50")+
    geom_ribbon(aes(ymin = lower/1.25, ymax = upper/1.25, fill = name),
                linetype = 0, alpha = 0.2)+
    geom_line() +
    geom_point(data = dat_c14,aes(x=as.Date(yday, origin = as.Date("2019-01-01")),y=p80,color="C14")) +
    scale_color_manual(values = c("black","dodgerblue"),labels = c(expression(""^14*C),expression(O[2]))) +
    scale_fill_manual(values = c("dodgerblue","firebrick"),guide=FALSE) +
    theme_bw() +
    labs(y=expression(mmol~C~m^-3~d^-1),color="",x="Day of Year") +
    facet_wrap(vars(lake,year),scales = "free_y",ncol=4) +
    theme(strip.text.x = element_text(size = 8))+
    theme(legend.position = "none")
p1 
ggsave(plot = p1,"graphics/metabolism.pdf",width=7,height=7.9,dpi=300,units="in")

#Biplot of discreate Days
p2 <- ggplot(data = biplot,aes(x=p80,y=middle/1.25,color=lake)) + 
    geom_errorbar(aes(ymin=lower/1.25, ymax=upper/1.25),col="lightgrey") +
    geom_point() +
    geom_abline(slope = 1,intercept = 0) +
    theme_bw()+
    theme(aspect.ratio=1) +
    labs(y =  expression(O[2]~(mmol~C~m^-3~d^-1)),
         x = expression(""^14*C~(mmol~C~m^-3~d^-1))) +
    scale_x_log10(limits=c(.5,1000)) +
    scale_y_log10(limits=c(0.5,1000)) +
    theme(legend.position = "bottom") +
    theme(legend.title = element_blank())
p2

p4 <- ggMarginal(p2,type="density")
ggsave(plot = p4,"graphics/point_estimates.pdf",width=5,height=5,units="in",dpi=300)

#biplot of 7 day median GPP
# p3 <- ggplot(data = biplot_avg,aes(x=p80,y=avg/1.25)) + 
#     geom_point(aes(x=p80,y=middle/1.25),color="lightgrey") +
#     # geom_errorbar(aes(ymin=lower/1.25, ymax=upper/1.25),col="lightgrey") +
#     geom_point() +
#     geom_abline(slope = 1,intercept = 0) +
#     theme_bw()+
#     coord_fixed(xlim = c(1,600),ylim=c(1,600)) +
#     theme(aspect.ratio=1) +
#     labs(y =  expression(O[2]~(mmol~C~m^-3~d^-1)),
#          x = expression(""^14*C~(mmol~C~m^-3~d^-1))) +
#     scale_x_log10() +
#     scale_y_log10()
# p3

# ggsave(plot = p3,"graphics/median_point.pdf",width=3,height=3,units="in",dpi=300)
p5 <- ggplot(data = biplot,aes(x=p80,y=middle/1.25,color=as.factor(lake))) + 
    geom_errorbar(aes(ymin=lower/1.25, ymax=upper/1.25),col="lightgrey") +
    geom_point() +
    geom_abline(slope = 1,intercept = 0) +
    theme_bw()+
    coord_fixed(xlim = c(1,600),ylim=c(1,600)) +
    theme(aspect.ratio=1) +
    labs(y =  expression(O[2]~(mmol~C~m^-3~d^-1)),
         x = expression(""^14*C~(mmol~C~m^-3~d^-1))) +
    scale_x_log10() +
    scale_y_log10() +
    facet_wrap(vars(lake))+
    theme(legend.position = "none")
p5


ggsave("graphics/lake_values.pdf",width=5,height=5,units="in",dpi=300)

##############
#Credible Interval Overlap
##############
temp <- biplot %>% mutate(CI = ifelse(p80>=lower/1.25 & p80 <= upper/1.25,1,0))
sum(temp$CI)/101
#exclude castle
temp <- biplot %>% filter(lake != "Castle") %>% mutate(CI = ifelse(p80>=lower/1.25 & p80 <= upper/1.25,1,0))
sum(temp$CI)/nrow(temp)
##############
#EIV Regression
##############

#discreate daily
#log transformed
m2 <- mcreg(log10(biplot$p80+1),log10(biplot$middle/1.25+1),method.reg="PaBa",method.ci = "nestedbootstrap",method.bootstrap.ci = "tBoot")
MCResult.plot(x=m2, add.legend=TRUE,equal.axis=TRUE,xn=50,ci.area = TRUE,x.lab="14 C",y.lab = "Free-water")
getCoefficients(m2)

biplot_acton <- biplot %>% filter(lake=="Acton")
m2c <- mcreg((biplot_acton$p80),(biplot_acton$middle/1.25),method.reg="PaBa",method.ci = "nestedbootstrap",method.bootstrap.ci = "tBoot")
getCoefficients(m2c)

biplot_low <- biplot %>% filter(lake!="Acton")
m2d <- mcreg((biplot_low$p80),(biplot_low$middle/1.25),method.reg="PaBa",method.ci = "nestedbootstrap",method.bootstrap.ci = "tBoot")
getCoefficients(m2d)

#7 day average
m3 <- mcreg(log10(biplot_avg$p80+1),log10(biplot_avg$middle/1.25+1),method.reg="PaBa",method.ci = "nestedbootstrap",method.bootstrap.ci = "tBoot")
MCResult.plot(x=m3, add.legend=TRUE,equal.axis=TRUE,xn=50,ci.area = TRUE,x.lab="14 C",y.lab = "Free-water")
getCoefficients(m3)

#lake specific regressions
temp <- biplot %>% filter(lake=="Acton")
acton_reg <- mcreg(temp$p80,(temp$middle/1.25),method.reg="PaBa",method.ci = "nestedbootstrap",method.bootstrap.ci = "tBoot")
getCoefficients(acton_reg)
temp <- biplot %>% filter(lake=="Castle")
castle_reg <- mcreg(temp$p80,(temp$middle/1.25),method.reg="PaBa",method.ci = "nestedbootstrap",method.bootstrap.ci = "tBoot")
getCoefficients(castle_reg)
temp <- biplot %>% filter(lake=="Sparkling")
sparkling_reg <- mcreg(temp$p80,(temp$middle/1.25),method.reg="PaBa",method.ci = "nestedbootstrap",method.bootstrap.ci = "tBoot")
getCoefficients(sparkling_reg)
temp <- biplot %>% filter(lake=="Trout")
trout_reg <- mcreg(temp$p80,(temp$middle/1.25),method.reg="PaBa",method.ci = "nestedbootstrap",method.bootstrap.ci = "tBoot")
getCoefficients(trout_reg)
temp <- biplot %>% filter(lake!="Acton")
lowp_reg <- mcreg(temp$p80,(temp$middle/1.25),method.reg="PaBa",method.ci = "nestedbootstrap",method.bootstrap.ci = "tBoot")
getCoefficients(lowp_reg)

pdf(file = "graphics/pb_regressions.pdf",width=6.5,height=6.5)
par(mfrow=c(2,2))
plot(m2,sub="")
plot(m2b,sub="")
plot(m3,sub="")
MCResult.plot(m3b,sub="")
dev.off()

#export final analysis and figure tables
out <- dat_metab %>% filter(name=="GPP") %>% 
    select(-name) %>% 
    rename(o2_pp_mmolcm3d = middle) %>% 
    mutate(o2_pp_mmolcm3d = o2_pp_mmolcm3d/1.25) %>% 
    rename(o2_pp_975_ci = upper) %>% 
    mutate(o2_pp_975_ci = o2_pp_975_ci/1.25) %>% 
    rename(o2_pp_025_ci = lower) %>% 
    mutate(o2_pp_025_ci = o2_pp_025_ci/1.25) %>% 
    left_join(dat_c14) %>% 
    rename(c14_pp_mmolcm3d = p80) %>% 
    select(lake,year,yday,date, o2_pp_mmolcm3d,o2_pp_025_ci,o2_pp_975_ci,c14_pp_mmolcm3d)
write_csv(out,"data/final/daily_pp_data.csv")

out2 <- out %>% drop_na() %>% left_join(biplot_avg %>% select(lake,date,avg)) %>% 
    rename(avg_o2_pp_mmolcm3d=avg)
write_csv(out2,"data/final/discreate_pp_data.csv")

out3 <- out %>% left_join(biplot_avg %>% select(lake,date,avg)) %>% 
    rename(avg_o2_pp_mmolcm3d=avg) %>% 
    mutate(avg_o2_pp_mmolcm3d = avg_o2_pp_mmolcm3d/1.25)
write_csv(out3,"data/final/lottig_etal_data.csv")
