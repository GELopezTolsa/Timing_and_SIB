rm(list=ls())

library(brms)

setwd("~/Dropbox/Doctorado UNED/PhD Thesis/FI+SID/R_FI_SID")

data <- read.csv("data_LP_Licks.csv")

##################### LEVER PRESSES #########################

m0 = brm(formula = LP ~ (1|Subject), 
         data = data, chains = 4, iter = 3000,
         cores = getOption("mc.cores", 2L),
         control = list(adapt_delta=0.99, max_treedepth = 12),
         seed = 1702, save_all_pars = T)

m1 = brm(formula = LP ~ Group + FI + (1|Subject), 
         data = data, chains = 4, iter = 3000,
         cores = getOption("mc.cores", 2L),
         control = list(adapt_delta=0.99, max_treedepth = 12),
         seed = 1702, save_all_pars = T)

############## BEST MODEL ################
m2 = brm(formula = LP ~ Group + FI + (1|Subject) + (1|Session), 
         data = data, chains = 4, iter = 2000,
         cores = getOption("mc.cores", 2L),
         control = list(adapt_delta=0.99, max_treedepth = 12),
         seed = 1702, save_all_pars = T)

m3 = brm(formula = LP ~ FI + (1|Subject) + (1|Session), 
         data = data, chains = 4, iter = 2000,
         cores = getOption("mc.cores", 2L),
         control = list(adapt_delta=0.99, max_treedepth = 12),
         seed = 1702, save_all_pars = T)

m4 = brm(formula = LP ~ Group + (1|Subject) + (1|Session), 
         data = data, chains = 4, iter = 2000,
         cores = getOption("mc.cores", 2L),
         control = list(adapt_delta=0.99, max_treedepth = 12),
         seed = 1702, save_all_pars = T)

post_samples_m2 = posterior_samples(m2)
head(post_samples_m2 %>% round(1))
post_samples_m2
plot(m2)
ppm2<-pp_check(m2,nsamples = 1000)
ppm2


mean(post_samples_m2$b_GroupS>0)
mean(post_samples_m2$b_FIB<0)
mean(post_samples_m2$b_FID<0)

bayes_factor(m2,m0)

######################### LICKS (SPOUT-LICKS) ###############################

mo0 = brm(formula = Licks ~ (1|Subject), 
         data = data, chains = 4, iter = 3000,
         cores = getOption("mc.cores", 2L),
         control = list(adapt_delta=0.99, max_treedepth = 12),
         seed = 1702, save_all_pars = T)

############# BEST MODEL ##################

mo1 = brm(formula = Licks ~ FI + (1|Subject), 
         data = data, chains = 4, iter = 3000,
         cores = getOption("mc.cores", 2L),
         control = list(adapt_delta=0.99, max_treedepth = 12),
         seed = 1702, save_all_pars = T)

mo2 = brm(formula = Licks ~ FI + (1|Subject) + (1|Session), 
         data = data, chains = 4, iter = 2000,
         cores = getOption("mc.cores", 2L),
         control = list(adapt_delta=0.99, max_treedepth = 12),
         seed = 1702, save_all_pars = T)

post_samples_mo1 = posterior_samples(mo1)
plot(mo1)
ppmo1<-pp_check(mo1,nsamples = 1000)
ppmo1

mean(post_samples_mo1$b_FIB<0)
mean(post_samples_mo1$b_FID<0)

bayes_factor(mo1,mo0)
