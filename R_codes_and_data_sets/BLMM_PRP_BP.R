rm(list=ls())

library(brms)

setwd("~/Dropbox/Doctorado UNED/PhD Thesis/FI+SID/R_FI_SID")

data <- read.csv("Data_Total.csv")

############ POST-RENFORCEMENT PAUSE #############

ba0 = brm(formula = PRP ~ (1|Sujeto_total), 
          data = data, chains = 4, iter = 3000, warmup = 1000,
          cores = getOption("mc.cores", 2L),
          control = list(adapt_delta=0.99, max_treedepth = 12),
          seed = 1702, save_all_pars = T)
  
ba1 = brm(formula = PRP ~ Group + (1|Sujeto_total) + (1 + Group|FI_Letra), 
          data = data, chains = 4, iter = 2000, warmup = 1000,
          cores = getOption("mc.cores", 2L),
          control = list(adapt_delta=0.99, max_treedepth = 12),
          seed = 1702, save_all_pars = T)

#### EBEST MODEL #####
ba2 = brm(formula = PRP ~ Group + FI_Letra + (1|Sujeto_total), 
          data = data, chains = 4, iter = 3000, warmup = 1000,
          cores = getOption("mc.cores", 2L),
          control = list(adapt_delta=0.99, max_treedepth = 12),
          seed = 1702, save_all_pars = T)

ba3 = brm(formula = PRP ~ Group + (1|Sujeto_total) + (1|FI_Letra), 
          data = data, chains = 4, iter = 3000, warmup = 1000,
          cores = getOption("mc.cores", 2L),
          control = list(adapt_delta=0.99, max_treedepth = 17),
          seed = 1702, save_all_pars = T)

### ADDITIONAL DATA BEST MODEL ####
post_samples_ba2 = posterior_samples(ba2)
head(post_samples_ba2 %>% round(1))
post_samples_ba2
plot(ba2)
ppba2<-pp_check(ba2,nsamples = 1000)
ppba2

mean(post_samples_ba2$b_GroupS<0)
mean(post_samples_ba2$b_FI_LetraB>0)
mean(post_samples_ba2$b_FI_LetraD>0)

bayes_factor(ba2,ba0)

coef(ba2)
ba2

################### BREAKPOINT #######################

bay0 = brm(formula = Breakpoint ~ (1|Sujeto_total), 
          data = data, chains = 4, iter = 3000, warmup = 1000,
          cores = getOption("mc.cores", 2L),
          control = list(adapt_delta=0.99, max_treedepth = 21),
          seed = 1702, save_all_pars = T)

############ BEST MODEL ##############
bay1 = brm(formula = Breakpoint ~ Group + FI_Letra + (1|Sujeto_total), 
          data = data, chains = 4, iter = 3200,
          cores = getOption("mc.cores", 2L),
          control = list(adapt_delta=0.99, max_treedepth = 21),
          seed = 1702, save_all_pars = T)

bay2 = brm(formula = Breakpoint ~ Group + (1|Sujeto_total) + (1 + Group|FI_Letra), 
          data = data, chains = 4, iter = 3000,
          cores = getOption("mc.cores", 2L),
          control = list(adapt_delta=0.99, max_treedepth = 21),
          seed = 1702, save_all_pars = T)

bay3 = brm(formula = Breakpoint ~ Group + (1|Sujeto_total) + (1|FI_Letra), 
           data = data, chains = 4, iter = 3200,
           cores = getOption("mc.cores", 2L),
           control = list(adapt_delta=0.99, max_treedepth = 21),
           seed = 1702, save_all_pars = T)


post_samples_bay1 = posterior_samples(bay1)
head(post_samples_bay1 %>% round(1))
post_samples_bay1
plot(bay1)
ppbay1<-pp_check(bay1,nsamples = 1000)
ppbay1

mean(post_samples_bay1$b_GroupS<0)
mean(post_samples_bay1$b_FI_LetraB>0)
mean(post_samples_bay1$b_FI_LetraD>0)

bayes_factor(bay1,bay0)

coef(bay1)
