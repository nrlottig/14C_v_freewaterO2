rm(list=ls())
library(lubridate)
library(patchwork)
date_matrix = tibble(date=seq(from=as_date("2007-01-01"),to=as_date("2017-12-31"),by="1 day")) %>% 
    mutate(yday = yday(date),year=year(date)) %>% 
    filter(yday>=152 & yday <= 245)

sp_metab <- read_csv("model/output/alt_model/sparkling_varying_daily_full.csv") %>% 
    filter(name=="GPP" | name=="NEP") %>% 
    drop_na(index) 
sp_metab <- sp_metab %>% expand(yday=full_seq(yday,1),name,year) %>% left_join(sp_metab) %>% arrange(name,year,yday) %>% left_join(date_matrix) %>% 
    select(name,date,middle,upper,lower) %>% 
    mutate(lake="Sparkling")

sp_c14 <- read_tsv("C14_data/SP_Daily14CProd_new.txt") %>% 
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

tr_metab <- read_csv("model/output/alt_model/trout_varying_daily_full.csv") %>% 
    filter(name=="GPP" | name=="NEP") %>% 
    drop_na(index) 
tr_metab <- tr_metab %>% expand(yday=full_seq(yday,1),name,year) %>% left_join(tr_metab) %>% arrange(name,year,yday) %>% left_join(date_matrix) %>% 
    select(name,date,middle,upper,lower) %>% 
    mutate(lake="Trout")

tr_c14 <- read_tsv("C14_data/TR_Daily14CProd_new.txt") %>% 
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

ca_metab <- read_csv("model/output/alt_model/castle_varying_daily_full.csv") %>% 
    filter(name=="GPP" | name=="NEP") %>% 
    drop_na(index)
ca_metab <- ca_metab %>% expand(yday=full_seq(yday,1),name,year) %>% left_join(ca_metab) %>% arrange(name,year,yday) %>% left_join(date_matrix) %>% 
    select(name,date,middle,upper,lower) %>% 
    mutate(lake="Castle")

ca_c14 <- read_csv("C14_data/castle_c14.csv") %>% 
    rename(date = Date) %>% 
    mutate(date = mdy(date)) %>% 
    mutate(yday=yday(date),year=year(date)) %>% 
    rename(p80 = mgc_l_d) %>% 
    mutate(p80 = p80*1000)
ca_c14 <- ca_metab %>% filter(name=="GPP") %>% left_join(ca_c14) %>%
    drop_na() %>% 
    select(date,lake,p80) %>% 
    mutate(lake="Castle")


ntl <- rbind(sp_c14,tr_c14)
dat_c14 <- ca_c14 %>% rbind(ntl %>% select(lake,date,p80)) %>% 
    mutate(p80=p80/12.011) %>% 
    mutate(year=year(date),yday=yday(date))
dat_metab <- rbind(sp_metab,tr_metab,ca_metab) %>% 
    mutate(year = year(date),yday=yday(date))



p1 <- ggplot(data = dat_metab ,aes(yday, middle, color = name))+
    geom_hline(yintercept = 0, size = 0.3, color = "gray50")+
    geom_ribbon(aes(ymin = lower, ymax = upper, fill = name),
                linetype = 0, alpha = 0.2)+
    geom_line() +
    geom_point(data = dat_c14,aes(x=yday,y=p80,color="C14")) +
    scale_color_manual(values = c("black","dodgerblue","firebrick")) +
    scale_fill_manual(values = c("dodgerblue","firebrick"),guide=FALSE) +
    theme_bw() +
    labs(y=expression(mmol~O[2]~m^-3~d^-1),color="",x="Day of Year") +
    facet_wrap(vars(lake,year)) +
    theme(strip.text.x = element_text(size = 8))
p1 
ggsave(plot = p1,"graphics/metabolism.png",width=10,height=7.5,dpi=300)

biplot <- dat_c14 %>% left_join(dat_metab %>% filter(name=="GPP")) %>% select(middle,p80,upper,lower) %>% drop_na()

p2 <- ggplot(data = biplot,aes(x=p80,y=middle)) + 
    geom_errorbar(aes(ymin=lower, ymax=upper),col="lightgrey") +
    geom_point() +
    geom_abline(slope = 1,intercept = 0) +
    theme_bw()+
    coord_fixed(xlim = c(0,11),ylim=c(0,11)) +
    theme(aspect.ratio=1) +
    labs(y =  expression(GPP~"("*mmol~O[2]~m^{-3}~day^{-1}*")"),
         x = expression(C[14]~"("*mmol~C~m^{-3}~day^{-1}*")"))
p2

p3 <- ggplot(data = biplot,aes(x=p80,y=middle/1.25)) + 
    geom_errorbar(aes(ymin=lower/1.25, ymax=upper/1.25),col="lightgrey") +
    geom_point() +
    geom_abline(slope = 1,intercept = 0) +
    theme_bw()+
    coord_fixed(xlim = c(0,11),ylim=c(0,11)) +
    theme(aspect.ratio=1) +
    labs(y =  expression(GPP~"("*mmol~O[2]~m^{-3}~day^{-1}*")"),
         x = expression(C[14]~"("*mmol~C~m^{-3}~day^{-1}*")"))
p3

p4<-ggplot(data=biplot %>% 
               select(middle,p80) %>% 
               mutate(middle=middle) %>% 
               rename(GPP = middle,C14=p80) %>% 
               gather(value=value,key=model)) + 
    geom_density(aes(x=value,col=model,fill=model),alpha=0.5) +
    scale_color_manual(values = c("black","dodgerblue"),guide=FALSE)+
    scale_fill_manual(values = c("black","dodgerblue"),guide=FALSE) +
    lims(x=c(0,10)) + 
    theme_bw() +
    theme(aspect.ratio=1) +
    labs(x = expression(Production~"("*mmol~m^{-3}~day^{-1}*")"))
p4
p5 <-ggplot(data=biplot %>% 
               select(middle,p80) %>% 
               mutate(middle=middle/1.25) %>% 
               rename(GPP = middle,C14=p80) %>% 
               gather(value=value,key=model)) + 
    geom_density(aes(x=value,col=model,fill=model),alpha=0.5) +
    scale_color_manual(values = c("black","dodgerblue"),guide=FALSE)+
    scale_fill_manual(values = c("black","dodgerblue"),guide=FALSE) +
    lims(x=c(0,10)) + 
    theme_bw() +
    theme(aspect.ratio=1) +
    labs(x = expression(Production~"("*mmol~m^{-3}~day^{-1}*")"))
p5

combined_plots <- (p2 + p3)/(p4+p5)
combined_plots + plot_annotation(tag_levels = "A",
                                 caption = "Comparision of primary production estimates using free-water oxygen metabolism and 14C incubations. 
Grey error bars in A & B are the 95% credible intervals of the free-water oxygen production estimate.
Free-water production estimates of plots on the right (B & D) are scaled assuming a photosynthetic
quotient of 1.25 (Hanson et al., others). Free-water (blue) and 14C (black) distribution of estimtes
shown in bottom row.",
                                 theme = theme(plot.caption = element_text(size = 10, hjust = 0)))

ggsave("graphics/point_estimates.png",width=6.5,height=6.5,dpi=300)
