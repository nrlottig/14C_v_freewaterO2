rm(list=ls())

# load packages
library(tidyverse)
library(rstan)
library(loo)
library(patchwork)
library(lubridate)
source("model/stan_utility.R")

lake <- "acton" #c("sparkling","trout")
year <- c(2010,2014)
# stan settings
rstan_options(auto_write = TRUE)
# read data
data <- read_rdump(paste("model/input/",lake,"_",min(year),"_",max(year),"_sonde_list.R",sep=""))

# set reference temperature
data$temp_ref <- 20

model <- "o2_model_inhibition.stan" #Steele 2 param inhibition
model_path <- paste0("model/stan/",model)

chains <- 3
iter <-6000 
warmup <- 1000
adapt_delta <- 0.80
max_treedepth <- 15
thin <- 1
data$sig_b0 <- 0.01 #pmax smoothing parameter #0.1
data$sig_r <- 0.01  #respiration smoothing parameter
data$sig_i0 <- 0.2  #light saturation smoothing parameter #1

sm <- stan_model(file = model_path)
fit <- sampling(sm, data = data, chains = chains, cores = chains, iter = iter, warmup = min(iter*0.5,warmup),
                control = list(adapt_delta = adapt_delta, max_treedepth = max_treedepth), 
                seed=194838,thin = thin,save_warmup=FALSE)


fit_summary <- summary(fit, probs=c(0.025,0.5,0.975))$summary %>% 
  {as_tibble(.) %>%
      mutate(var = rownames(summary(fit)$summary))}


  check_n_eff(fit)
  check_rhat(fit)
  check_div(fit)
  check_treedepth(fit,max_treedepth)
  check_energy(fit)

# export path
output_path <- paste0("model/output/")
# save model full output
saveRDS(fit, paste0(output_path,"/",lake,"_fit.rds"))

fit_clean <- fit_summary %>%
  rename(lower = '2.5%', middle = `50%`,upper = '97.5%')  %>%
  mutate(name = strsplit(var, "\\[|\\]|,") %>% map_chr(~.x[1]),
         index = strsplit(var, "\\[|\\]|,") %>% map_int(~as.integer(.x[2])),
         day = ifelse(name %in% c("GPP","ER","NEP","AIR","Flux","GPP_m2","ER_m2","NEP_m2","b0","r","i0","b"), 
                      index, 
                      data$map_days[index])) %>%
  select(name, index, day, middle,lower,upper)

 
  # data
sonde_data <- read_csv(paste0("data/model_input/sonde_prep_",lake,"_",year[1],"_",year[2],".csv"))

out <- fit_clean %>%
  filter(name %in% c("GPP","ER","NEP")) %>%
  rename(unique_day = day) %>% 
  left_join(sonde_data %>% select(unique_day,yday,year) %>% distinct()) %>% 
  full_join(sonde_data %>% expand(year,yday,name=c("GPP","ER","NEP"))) %>% 
  mutate(middle = ifelse(name=="ER",-middle,middle),
         lower = ifelse(name=="ER",-lower,lower),
         upper = ifelse(name=="ER",-upper,upper),
         name = factor(name, levels=c("GPP","NEP","ER")))
out2 <- fit_clean %>%
  filter(name %in% c("GPP_m2","ER_m2","NEP_m2"))%>%
  rename(unique_day = day) %>% 
  left_join(sonde_data %>% select(unique_day,yday,year) %>% distinct()) %>% 
  full_join(sonde_data %>% expand(year,yday,name=c("GPP_m2","ER_m2","NEP_m2"))) %>% 
  mutate(middle = ifelse(name=="ER_m2",-middle,middle),
         lower = ifelse(name=="ER_m2",-lower,lower),
         upper = ifelse(name=="ER_m2",-upper,upper),
         name = factor(name, levels=c("GPP_m2","NEP_m2","ER_m2")))

out3 <- rbind(out,out2)

#==========
#==========  Export
#==========

# export
write_csv(out3, paste0("model/output/",lake,"_daily_full.csv"))
write_csv(fit_clean, paste0("model/output/",lake,"_summary_clean.csv"))

if(lake=="sparkling") c14 <- read_tsv("data/sp_daily14c_prod.txt") %>% 
  mutate(year = year(date),yday=yday(date)) %>% filter(type=="m3")
if(lake =="trout") c14 <- read_tsv("data/tr_daily14c_Prod.txt") %>% 
  mutate(year = year(date),yday=yday(date)) %>% filter(type=="m3") %>% 
  filter(yday(date)>=152)
if(lake=="castle") c14 <- read_csv("data/ca_daily14c_prod.csv") %>% 
  mutate(Date = mdy(Date)) %>% 
  mutate(yday=yday(Date),year=year(Date)) %>% 
  rename(p80 = mgc_l_d) %>% 
  mutate(p80 = p80*1000)
if(lake =="acton") c14 <- read_csv("data/acton_c14.csv") %>% 
  mutate(Date = dmy(Date),
         yday=yday(Date),
         year=year(Date)) %>% 
  rename(p80=ppr_mgc_m2_d) %>% 
  drop_na() %>% 
  left_join(sonde_data %>% group_by(year,yday) %>% summarize(z=mean(z,na.rm=T))) %>% 
  mutate(p80=p80/z)



#plot primary parameters
p1 <- fit_clean %>%  
  filter(name=="b0" | name == "r" | name == "i0" ) %>% 
  rename(unique_day = index) %>% 
  left_join(sonde_data %>% select(unique_day,yday,year) %>% distinct()) %>%
  ggplot(aes(x=yday,y=middle,color=factor(year))) + 
  geom_point(size=0.5) +
  geom_line(alpha=0.5) +
  facet_wrap(vars(name),ncol=1,scales="free_y") +
    theme_bw() +
    labs(y="Mean Estimated Value",color="year",x="Day of Year")
p1
ggsave(plot = p1,filename = paste("graphics/",lake,"_",year[1],"_",year[2],"_params.png",sep=""),width=11,height=8.5,dpi=300)

#plot time series of estimates
p2 <- ggplot(data = out %>% drop_na(year),aes(yday, middle, color = name))+
  geom_hline(yintercept = 0, size = 0.3, color = "gray50")+
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = name),
              linetype = 0, alpha = 0.2)+
  geom_line()+
  geom_point(data = out %>% left_join(c14),aes(x=yday,y=(p80/12.011),color="C14")) +
  scale_color_manual(values = c("green","black","dodgerblue","firebrick")) +
  scale_fill_manual(values = c("dodgerblue","firebrick","black")) +
  theme_bw() +
    labs(y=expression(mmol~O[2]~m^-3~d^-1)) +
  facet_wrap(vars(year),scales="free_y")
p2

ggsave(plot = p2,filename = paste("graphics/",lake,"_",year[1],"_",year[2],"_metabolism.png",sep=""),width=11,height=8.5,dpi=300)

dat <- c14 %>% left_join(out %>% filter(name=="GPP"))
ggplot(data = dat,aes(x=p80/12.011,y=middle)) + geom_point() + geom_abline(slope = 1,intercept = 0)
