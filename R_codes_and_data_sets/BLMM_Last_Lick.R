rm(list=ls())

library(brms)

setwd("~/Dropbox/Doctorado UNED/PhD Thesis/FI+SID/R_FI_SID")

data <- read.csv("data_licks.csv")

m0 = brm(formula = LL ~ (1|Subject), 
         data = data, chains = 4, iter = 3000,
         cores = getOption("mc.cores", 2L),
         control = list(adapt_delta=0.99, max_treedepth = 12),
         seed = 1702, save_all_pars = T)

################ BEST MODEL #################
m1 = brm(formula = LL ~ FI_L + (1|Subject), 
         data = data, chains = 4, iter = 3000,
         cores = getOption("mc.cores", 2L),
         control = list(adapt_delta=0.99, max_treedepth = 12),
         seed = 1702, save_all_pars = T)

m2 = brm(formula = LL ~ FI_L + (1|Subject) + (1|Session), 
         data = data, chains = 4, iter = 2000,
         cores = getOption("mc.cores", 2L),
         control = list(adapt_delta=0.99, max_treedepth = 12),
         seed = 1702, save_all_pars = T)

post_samples_m1 = posterior_samples(m1)
head(post_samples_m1 %>% round(1))
post_samples_m1
plot(m1)
ppm1<-pp_check(m1,nsamples = 1000)
ppm1

mean(post_samples_m1$b_FI_LB>0)
mean(post_samples_m1$b_FI_LC>0)

bayes_factor(m1,m2)


