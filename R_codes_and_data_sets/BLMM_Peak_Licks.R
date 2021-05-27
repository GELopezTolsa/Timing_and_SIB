rm(list=ls())

library(brms)

setwd("~/Dropbox/Doctorado UNED/PhD Thesis/FI+SID/R_FI_SID")

data <- read.csv("data_peak.csv")

m0 = brm(formula = Peak ~ (1|Subject), 
         data = data, chains = 4, iter = 2000,
         cores = getOption("mc.cores", 2L),
         control = list(adapt_delta=0.99, max_treedepth = 12),
         seed = 1702, save_all_pars = T)

############# BEST MODEL #################3
m1 = brm(formula = Peak ~ FI + (1|Subject), 
         data = data, chains = 4, iter = 2000,
         cores = getOption("mc.cores", 2L),
         control = list(adapt_delta=0.99, max_treedepth = 12),
         seed = 1702, save_all_pars = T)

m2 = brm(formula = Peak ~ FI + (1|Subject) + (1|Session), 
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

ppm2<-pp_check(m2,nsamples = 1000)
ppm2

mean(post_samples_m1$b_FIB>0)
mean(post_samples_m1$b_FIC>0)

bayes_factor(m1,m0)
