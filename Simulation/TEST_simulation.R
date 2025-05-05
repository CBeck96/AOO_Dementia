library(tidyverse)
library(survival)
library(ggplot2)
library(prodlim)
library(riskRegression)
library(pracma)
library(flexsurv)

# Set up ------------------------------------------------------------------

n_obs <- 1000

fixed_shape <- 1
fixed_mean <- 10

scale <- fixed_mean/gamma(1+1/fixed_shape)

# Simulating data without competing risk ----------------------------------

dat_wo_CR <- data.frame(sim_dementia = rweibull(n_obs, shape = fixed_shape, scale = scale)) %>% 
  mutate(dementia = ifelse(sim_dementia < 20,sim_dementia + 75, NaN),
         Tslut = pmin(dementia,95,na.rm = TRUE),
         censor_stat = ifelse(!is.na(dementia),1,0))

table(dat_wo_CR$censor_stat)/n_obs*100

# Case Only 
mean(dat_wo_CR$dementia,na.rm = T)

# Area under the curve
fit_wo_CR <- prodlim(Hist(Tslut,censor_stat) ~ 1, 
                     data = dat_wo_CR)

pred_wo_CR <- predict(fit_wo_CR,
                      times = c(0,seq(75,95,1)))


G1_wo_CR <- 1 - pred_wo_CR/max(pred_wo_CR)
# Mean by integrating under the survival curve.
aa_wo_CR <- mean_age(c(0,seq(75,95,1)),pred_wo_CR) 
aa_wo_CR


# plot(c(0,seq(75,95,1)),pred_wo_CR,type = "l")

# 
inv_trunc_weibull <- function(shape, scale, u, Trunc = 20){
  x = scale*(-log(1-u*(1-exp(-(Trunc/scale)^shape))))^(1/shape)
  return(x)
}

# 
theoretic_mean <- function(shape, scale, Trunc = 20){
  mu = (scale/(1-exp(-(Trunc/scale)^shape))) *gammainc(1+1/shape,(Trunc/scale)^shape)
  mu1 = as.numeric(mu)[1]
  return(mu1)
}


# Sanity check ------------------------------------------------------------

n_rep <- 10000
n_obs <- 10000

fixed_shapes <- c(0.5,1,2)
fixed_mean <- 10

scales <- fixed_mean/gamma(1+1/fixed_shapes)

mat_mean_CO = matrix(rep(0,length(fixed_shapes)*n_rep), nrow = n_rep, ncol = length(fixed_shapes))
mat_mean_AUC = matrix(rep(0,length(fixed_shapes)*n_rep), nrow = n_rep, ncol = length(fixed_shapes))
mat_mean_Theo = matrix(rep(0,length(fixed_shapes)*n_rep), nrow = n_rep, ncol = length(fixed_shapes))

for(i in 1:n_rep){

  for(s in 1:length(fixed_shapes)){
    dat <- data.frame(status = rbinom(n = n_obs,size = 1, prob = c(0.2)),
                      x = inv_trunc_weibull(shape = fixed_shapes[s], scale = scales[s], u = runif(n_obs)) + 75)
    
    dat$Tslut <- ifelse(dat$status == 1, dat$x, 95)
   
    # # Case Only 
    CO_mean = mean(dat$Tslut[dat$status == 1])
    mat_mean_CO[i,s] = CO_mean
    
    # Area under the curve
    fit = prodlim(Hist(Tslut,status) ~ 1, 
                  data = dat)
    
    pred = predict(fit,
                   times = c(0,seq(75,95,1)))
    
    F1 = 1 - pred
    G1 = 1 - F1/max(F1)
    # Mean by integrating under the survival curve.
    auc_mean = mean_age(c(0,seq(75,95,1)),G1) 
    mat_mean_AUC[i,s] = auc_mean
    
    # Theory
    theo_mean = theoretic_mean(fixed_shapes[s],
                               scales[s]) + 75
    mat_mean_Theo[i,s] = theo_mean
  }
}

# mat_mean_CO 
# mat_mean_AUC 
# mat_mean_Theo

colSums(mat_mean_CO)/nrow(mat_mean_CO)
colSums(mat_mean_AUC)/nrow(mat_mean_AUC)
colSums(mat_mean_Theo)/nrow(mat_mean_Theo)


# Sanity check with competing risk ----------------------------------------

# table(colSums(rmultinom(n = n_obs,size = 1, prob = c(0.4,0.1,0.5))*1:3)-1)/n_obs*100

n_rep <- 1000
n_obs <- 100000

fixed_shapes <- c(0.5,1,2)
fixed_mean <- 10

scales <- fixed_mean/gamma(1+1/fixed_shapes)

mat_meanCR_CO = matrix(rep(0,length(fixed_shapes)*n_rep), nrow = n_rep, ncol = length(fixed_shapes))
mat_meanCR_AUC = matrix(rep(0,length(fixed_shapes)*n_rep), nrow = n_rep, ncol = length(fixed_shapes))
mat_meanCR_Theo = matrix(rep(0,length(fixed_shapes)*n_rep), nrow = n_rep, ncol = length(fixed_shapes))

for(i in 1:n_rep){
  
  for(s in 1:length(fixed_shapes)){
    dat <- data.frame(censor_stat = colSums(rmultinom(n = n_obs,size = 1, prob = c(0.2,0.2,0.6))*1:3)-1,
                      x = inv_trunc_weibull(shape = fixed_shapes[s], scale = scales[s], u = runif(n_obs)),    # Event
                      y = inv_trunc_weibull(shape = fixed_shapes[2], scale = scales[2], u = runif(n_obs)))    # Competing risk
    
    dat$Tslut <- 666
    
    dat$Tslut[dat$censor_stat == 1] = dat$x[dat$censor_stat == 1] + 75
    dat$Tslut[dat$censor_stat == 2] = dat$y[dat$censor_stat == 2] + 75
    dat$Tslut[dat$censor_stat == 0] = 95
    
    # # Case Only 
    CO_mean = mean(dat$Tslut[dat$censor_stat == 1]) 
    mat_meanCR_CO[i,s] = CO_mean
    
    # Area under the curve
    auc_mean = pseudo_theta(dat)
    mat_meanCR_AUC[i,s] = auc_mean 
    
    if(is.na(auc_mean)) stop()
    
    # # Theory
    theo_mean = theoretic_mean(fixed_shapes[s],
                               scales[s]) + 75
    mat_meanCR_Theo[i,s] = theo_mean
  }
}

# mat_meanCR_CO 
# mat_meanCR_AUC 
# mat_meanCR_Theo

colSums(mat_meanCR_CO)/nrow(mat_meanCR_CO)
colSums(mat_meanCR_AUC)/nrow(mat_meanCR_AUC)
colSums(mat_meanCR_Theo)/nrow(mat_meanCR_Theo) 
# var(mat_meanCR_CO[,1])
# var(mat_meanCR_AUC[,1])


colSums(is.na(mat_meanCR_AUC))


# dat_special_case <- dat

table(dat_special_case$censor_stat)/n_obs*100
ggplot(data = dat_special_case, aes(y = Tslut, x = 1:n_obs, col = factor(censor_stat))) + 
  geom_point()

summary(dat_special_case)

dat_special_case %>% arrange(Tslut) %>% head()
