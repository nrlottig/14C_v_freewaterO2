rm(list=ls())
library(lubridate)
library(patchwork)
library(tidyverse)
library(zoo)
library(mcr)
library(MethComp)
library(ggExtra)
library(ggtext)
library(cowplot)

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
    # filter(delta_p ==min(delta_p)) %>% 
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
    # filter(delta_p ==min(delta_p)) %>% 
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


#biplot data for discreate days
biplot <- dat_c14 %>% left_join(dat_metab %>% filter(name=="GPP")) %>% select(lake,year,middle,p80,upper,lower) %>% drop_na()

dummy <- data.frame(yday=rep(170,20),
                    lake = c(rep("Acton",4),rep("Castle",4),rep("Sparkling",7),rep("Trout",5)),
                    middle=c(rep(1100,4),rep(10,4),rep(19,7),rep(17,5)),
                    year = c(2010:2012,2014,2014:2017,2007:2013,2007:2010,2012))

p1 <- ggplot(data = dat_metab %>% filter(name=="GPP") ,aes(as.Date(yday, origin = as.Date("2019-01-01")), middle/1.25))+
    geom_hline(yintercept = 0, size = 0, color = "gray50",alpha=0)+
    geom_ribbon(aes(ymin = lower/1.25, ymax = upper/1.25, fill = as.factor(lake)),
    linetype = 0, alpha = 0.2)+
    geom_line(aes(color=as.factor(lake))) +
    geom_point(data = dat_c14,aes(x=as.Date(yday, origin = as.Date("2019-01-01")),y=p80),color="black") +
    geom_blank(data=dummy)+
    # scale_color_manual(values = c("black","dodgerblue"),labels = c(expression(""^14*C),expression(O[2]))) +
    # scale_fill_manual(values = c("dodgerblue","firebrick"),guide=FALSE) +
    theme_bw() +
    labs(y=expression(Pelagic~Primary~Production~(mmol~C~m^-3~d^-1)),color="",x="Day of Year") +
    facet_wrap(vars(lake,year),scales = "free_y",ncol=4) +
    theme(strip.text.x = element_text(size = 8))+
    theme(legend.position = "none")
p1 
ggsave(plot = p1,"graphics/metabolism.pdf",width=7,height=7.9,dpi=1200,units="in")


#Biplot of discreate Days
p2 <- ggplot(data = biplot,aes(x=log10(p80),y=log10(middle/1.25),color=lake)) + 
    geom_abline(slope = 1,intercept = 0) +
    geom_errorbar(aes(ymin=log10(lower/1.25), ymax=log10(upper/1.25)),col="lightgrey") +
    geom_point() +
    theme_bw()+
    xlab(expression(""^14*C~(mmol~C~m^-3~d^-1))) +
    ylab("\n") +
    scale_x_continuous(breaks =c(0,1,2,3),labels=c("1","10","100","1000"),limits=c(-.3,3)) +
    scale_y_continuous(breaks =c(0,1,2,3),labels=c("1","10","100","1000"),limits=c(-.3,3)) +
    theme(legend.position = "bottom") +
    theme(legend.title = element_blank())

p2
line1 <- expression(Free-Water)
line2 <- expression(O[2]~(mmol~C~m^-3~d^-1))
p3<- p2 +  coord_cartesian(clip = "off") +
    draw_label(label = line1,x = -1.5,y=1.2,angle = 90,size = 11) +
    draw_label(label = line2,x = -1.20,y=1.2,angle = 90,size = 11)


p4 <- ggMarginal(p3,type="density")
p4
ggsave(plot = p4,"graphics/point_estimates.pdf",width=3.5,height=3.7,units="in",dpi=1200)

p5 <- ggplot(data = biplot,aes(x=p80,y=middle/1.25,color=as.factor(lake))) + 
    geom_abline(slope = 1,intercept = 0) +
    geom_errorbar(aes(ymin=lower/1.25, ymax=upper/1.25),col="lightgrey") +
    geom_point() +
    theme_bw()+
    # theme(aspect.ratio = 1) +
    labs(y =  expression(Free-water~(mmol~C~m^-3~d^-1)),
         x = expression(""^14*C~(mmol~C~m^-3~d^-1))) +
    scale_x_log10(limits=c(1,1000)) +
    scale_y_log10(limits=c(1,1000)) +
    facet_wrap(vars(lake))+
    theme(legend.position = "none")
p5

ggsave("graphics/lake_values.pdf",width=3.5,height=3.6,units="in",dpi=1200)

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

out2 <- out %>% drop_na() 
write_csv(out2,"data/final/discreate_pp_data.csv")

out3 <- out 
write_csv(out3,"data/final/lottig_etal_data.csv")
