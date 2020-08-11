rm(list=ls())

# load packages
library(tidyverse)
library(rstan)
library(loo)
library(patchwork)
library(lubridate)
source("model/stan_utility.R")

lake <- "trout" #c("sparkling","trout")
model_choice="varying"
year <- c(2007,2012)
# stan settings
rstan_options(auto_write = TRUE)
# specify analysis
analysis <- "alt_model" 

# con <- file(paste("logs/",lake,"_",year,"_log.txt",sep=""))
# sink(con, append=TRUE)
# sink(con, append=TRUE, type="message")

# read data
if(length(year)>1){
  data <- read_rdump(paste("model/input/alt_model/",lake,"_",min(year),"_",max(year),"_sonde_list.R",sep=""))
} else {
  data <- read_rdump(paste0("model/input/",analysis,"/",lake,"_",year,"_sonde_list.R"))
}
 
# set reference temperature
data$temp_ref <- 20

if(model_choice == "varying") {
  model <- "o2_model_inhibition.stan" #Steele 2 param inhibition
}

model_path <- paste0("model/stan/",model)
chains <- 6
iter <-4000 #Issues with 2000 iterations. Need to increase
warmup <- 1000
adapt_delta <- 0.80
max_treedepth <- 10
thin <- 1
data$sig_b0 <- 0.01
data$sig_r <- 0.01
data$sig_i0 <- 0.2

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
output_path <- paste0("model/output/",analysis)
# # 
# # # save model full output
saveRDS(fit, paste0(output_path,"/",lake,"_fit.rds"))
# # 
# # 
# # 
# # 
# # 
# # #==========
# # #========== Examine Chains
# # #==========
# # 
# # function for selecting fixed parameters
# fixed_par_fn <- function(x){
#   if(x=="o2_model.stan" | x== "o2_model_alt.stan"){return(c("gamma_1","gamma_2","sig_b0","sig_a","sig_r","sig_proc","lp__"))}
#   if(x=="o2_model_sig_obs.stan"){return(c("gamma_1","gamma_2","sig_b0","sig_a","sig_r",
#                                           "sig_proc","sig_obs","lp__"))}
#   if(x=="o2_model_fixed.stan"){return(c("gamma_1","gamma_2","sig_proc","lp__"))}
# }
# 
# # fixed parameters by step
# fixed_pars <- rstan::extract(fit, pars=fixed_par_fn(model)) %>%
#   lapply(as_tibble) %>%
#   bind_cols() %>%
#   set_names(fixed_par_fn(model)) %>%
#   mutate(chain = rep(1:chains, each = iter/2), step = rep(c(1:(iter/2)), chains))
# 
# # examine chains for parameters
# fixed_pars %>%
#   gather(par, value, -chain, -step) %>%
#   filter(par != "lp__") %>%
#   ggplot(aes(step, value, color=factor(chain)))+
#   facet_wrap(~par, scales="free_y")+
#   geom_line(alpha=0.5)+
#   theme_bw()
# 
# # posterior densities
# fixed_pars %>%
#   gather(par, value, -chain, -step) %>%
#   filter(par != "lp__") %>%
#   ggplot(aes(value))+
#   facet_wrap(~par, scales="free")+
#   stat_density(alpha=0.5, geom = "line")+
#   theme_bw()
# # 
# # 
# # 
# # 
# # 
# # #==========
# # #========== Prepare output for export
# # #==========
# # 
# # # beta0 and rho full
# daily_pars <- c("GPP")
# daily <- rstan::extract(fit, pars=daily_pars) %>%
# {lapply(1:length(daily_pars), function(x){y = .[[x]] %>% as_tibble %>%
#   mutate(chain = rep(1:chains, each = iter/2), step = rep(c(1:(iter/2)), chains)) %>%
#   gather(var, value, -chain, -step) %>%
#   mutate(day = strsplit(var, "V") %>% map_int(~as.integer(.x[2])),
#          name = daily_pars[x]) %>%
#   select(name, chain, step, day, value)
# return(y)
# })} %>%
#   bind_rows()

# clean variable names in summary
fit_clean <- fit_summary %>%
  rename(lower = '2.5%', middle = `50%`,upper = '97.5%')  %>%
  mutate(name = strsplit(var, "\\[|\\]|,") %>% map_chr(~.x[1]),
         index = strsplit(var, "\\[|\\]|,") %>% map_int(~as.integer(.x[2])),
         day = ifelse(name %in% c("GPP","ER","NEP","AIR","Flux","GPP_m2","ER_m2","NEP_m2","b0","r","i0","b"), 
                      index, 
                      data$map_days[index])) %>%
  select(name, index, day, middle,lower,upper)

# 
# # data
sonde_data <- read_csv(paste0("analyses/int_par/model_fit/input/sonde_prep_",lake,"_",year[1],"_",year[2],".csv"))

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
# 
# #==========
# #==========  Likelihoods & LOO
# #==========
# 
# # # log-likelihoods
# # likelihood <- tibble(analysis = analysis,
# #                      mean_log_lik = {fit_summary %>%
# #     filter(str_detect(fit_summary$var, "lik"))}$mean %>% sum(),
# #     median_log_lik = {fit_summary %>%
# #         filter(str_detect(fit_summary$var, "lik"))}$`50%` %>% sum())
# # 
# # # extract log Likelihoods and exponentiate
# # log_lik <- extract_log_lik(fit, merge_chains = FALSE)
# # r_eff <- relative_eff(exp(log_lik)) 
# # 
# # # LOO
# # loo <- loo(log_lik, r_eff = r_eff, cores = parallel::detectCores()-2)
# # 
# # 
# 
# 
# 
# #==========
# #==========  Export
# #==========
# 
# # export
# # write_csv(fixed_pars, paste0(output_path,"/fixed_pars_full.csv"))
write_csv(out3, paste0(output_path,"/",lake,"_",model_choice,"_daily_full.csv"))
# # write_csv(daily, paste0(output_path,"/daily_full.csv"))
write_csv(fit_clean, paste0(output_path,"/",lake,"_",model_choice,"_summary_clean.csv"))
# # write_csv(likelihood, paste0(output_path,"/log_liks.csv"))
# # saveRDS(loo, paste0(output_path,"/loo.rds"))

year_plot <- year

if(lake=="sparkling") c14 <- read_tsv("C14_data/SP_Daily14CProd_new.txt") %>% 
  mutate(year = year(date),yday=yday(date)) %>% filter(type=="m3")
if(lake =="trout") c14 <- read_tsv("C14_data/TR_Daily14CProd_new.txt") %>% 
  mutate(year = year(date),yday=yday(date)) %>% filter(type=="m3") %>% 
  filter(yday(date)>=152)
if(lake=="castle") c14 <- read_csv("C14_data/castle_c14.csv") %>% 
  mutate(Date = mdy(Date)) %>% 
  mutate(yday=yday(Date),year=year(Date)) %>% 
  rename(p80 = mgc_l_d) %>% 
  mutate(p80 = p80*1000)
# 
# c14 <- c14 %>% 
#   mutate(year = year(date)) %>% 
#   mutate(yday = yday(date)) %>% 
#   filter(type=="m3") %>% 
#   filter(yday <= 259) %>% 
#   filter(year == year_plot)

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

p2 <- ggplot(data = out %>% drop_na(year),aes(yday, middle/1.25, color = name))+
  geom_hline(yintercept = 0, size = 0.3, color = "gray50")+
  geom_ribbon(aes(ymin = lower/1.25, ymax = upper/1.25, fill = name),
              linetype = 0, alpha = 0.2)+
  geom_line()+
  geom_point(data = out %>% left_join(c14),aes(x=yday,y=(p80/12.011),color="C14")) +
  scale_color_manual(values = c("green","black","dodgerblue","firebrick")) +
  scale_fill_manual(values = c("dodgerblue","firebrick","black")) +
  theme_bw() +
    labs(y=expression(mmol~O[2]~m^-3~d^-1)) +
  facet_wrap(vars(year))
p2

# p1/p3/p2 +plot_layout(heights = c(2,1,2))

ggsave(plot = p2,filename = paste("graphics/",lake,"_",year[1],"_",year[2],"_metabolism.png",sep=""),width=11,height=8.5,dpi=300)

p3 <- ggplot(data = sonde_data,aes(x=datetime,y=scale(do))) + geom_line() + 
  geom_line(aes(x=datetime,y=scale(z)),col="red") +
  geom_line(aes(x=datetime,y=scale(wtemp)),col="blue") +
  facet_wrap(vars(year),scales="free_x")
p3
p2
p1
