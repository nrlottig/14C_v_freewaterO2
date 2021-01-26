temp <- biplot %>% mutate(CI = ifelse(p80>=lower & p80 <= upper,1,0))
temp2 <- temp %>% group_by(lake) %>% 
    summarize(covered = sum(CI),n=n(),frac=covered/n)

ggplot(data=temp,aes(x=p80,y=middle,color=factor(CI))) + 
    geom_errorbar(aes(ymin=lower, ymax=upper),col="lightgrey") +
    geom_point() +
    geom_abline(slope = 1,intercept = 0) +
    theme_bw()+
    coord_fixed(xlim = c(1,600),ylim=c(1,600)) +
    theme(aspect.ratio=1) +
    labs(y =  expression(Free*"-"*water~O[2]~Method),
         x = expression(""^14*C~Incubation~Method)) +
    scale_x_log10() +
    scale_y_log10() +
    facet_wrap(vars(lake))

ggsave("acton0m_1m_ppr.png",dpi=300,width=6,height=6)


temp3 <- biplot %>% select(middle,p80) %>%
    mutate(log.gpp = log10(middle+1),log.ppr=log10(p80+1)) %>% 
    rename(gpp = middle) %>% 
    rename(ppr = p80) %>% 
    mutate(observation = seq(1,101,1)) %>% 
    pivot_longer(cols = c(-observation))

library(ggalt)
library(viridis)
ggplot(temp3, aes(x = value, fill = name)) +
    geom_bkde() +
    geom_rug() +
    scale_fill_viridis(guide = FALSE, discrete = TRUE) +
    facet_wrap(~name, scales = "free") 
