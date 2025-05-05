library(ggplot2)
library(pracma)
library(prodlim)

set.seed(66)

# -------------------------------------------------------------------------

n_sim <- 1000
Trunc <- 20

# Set values
shape <- 1.25
theo_mean <- 10

# Compute the scale
(scale <- theo_mean/gamma(1+1/shape))

# Weibull (non-truncated, non-censored) -----------------------------------

# simulate a simple weibull
w <- rweibull(n_sim,shape = shape, scale = scale)

mean(w) ; abs(mean(w) - theo_mean)

# Estimate parameters of weibull
parameters <- MASS::fitdistr(w, densfun = dweibull, start = list(scale = 1, shape = 1))
est_scale <- as.numeric(parameters$estimate[1]) ; est_shape <- as.numeric(parameters$estimate[2])

# Mean based on estimates of variable
(est_par_mean <- est_scale*gamma(1+1/est_shape)) ; abs(est_par_mean - theo_mean)

# Mean based on AUC with our method
fit_w <- prodlim(Hist(time, stat) ~ 1,
                 data = data.frame(time = w,
                                   stat = 1))
pred_w <- predict(fit_w,times = seq(0,Trunc,0.1))
F1_w <- 1 - pred_w
G1_w <- F1_w/max(F1_w)
auc_w_mean <- mean_age(seq(0,Trunc,0.1),1-G1_w)
auc_w_mean ; abs(auc_w_mean - theo_mean)

# Mean based on AUC from CDF
CDF_w <- pweibull(seq(0,50,0.1),shape = shape, scale = scale)
auc_cdfw_mean <- mean_age(seq(0,50,0.1),1 - CDF_w)
auc_cdfw_mean ; abs(auc_cdfw_mean - theo_mean)

# Mean based on AUC from CDF with estimates parameters
CDF_est <- pweibull(seq(0,50,0.1),shape = est_shape, scale = est_scale)
auc_cdfest_mean <- mean_age(seq(0,50,0.1),1-CDF_est)
auc_cdfest_mean ; abs(auc_cdfest_mean - theo_mean)

# Weibull (non-truncated, censored) ---------------------------------------

# simulate a simple weibull with censoring
censor_stat <- rbinom(n_sim,size = 1, prob = 0.3)
wc <- w[censor_stat == 1]

mean(wc) ; abs(mean(wc) - theo_mean)

# Estimate parameters of weibull with censoring
para_cens <- MASS::fitdistr(wc, densfun = dweibull, start = list(scale = 1, shape = 1))
est_scale_cens <- as.numeric(para_cens$estimate[1]) ; est_shape_cens <- as.numeric(para_cens$estimate[2])

# Mean based on estimates of variable
est_par_mean_cens <- est_scale_cens*gamma(1+1/est_shape_cens)
est_par_mean_cens ; abs(est_par_mean_cens - theo_mean)

# Mean based on AUC with our method
fit_wc <- prodlim(Hist(time, stat) ~ 1,
                  data = data.frame(time = w,
                                    stat = censor_stat))
pred_wc <- predict(fit_wc,times = seq(0,Trunc,0.1))
F1_wc <- 1 - pred_wc
G1_wc <- F1_wc/max(F1_wc)
auc_wc_mean <- mean_age(seq(0,Trunc,0.1),1-G1_wc)
auc_wc_mean ; abs(auc_wc_mean - theo_mean)


# # Mean based on AUC from CDF
# CDF_w <- pweibull(seq(0,Trunc,0.1),shape = shape, scale = scale)
# auc_cdfw_mean <- mean_age(seq(0,Trunc,0.1),CDF_w)

# Mean based on AUC from CDF with estimates parameters
CDF_est_cens <- pweibull(seq(0,50,0.1),shape = est_shape_cens, scale = est_scale_cens)
auc_cdfest_mean_cens <- mean_age(seq(0,50,0.1), 1 - CDF_est_cens)
auc_cdfest_mean_cens ; abs(auc_cdfest_mean_cens - theo_mean)

# Weibull (truncated, non-censored) ---------------------------------------

# Truncate a simple weibull
w_trunc <- w[w <= Trunc]

mean(w_trunc) ; abs(mean(w_trunc) - theo_mean)

# Estimate parameters of weibull sample truncated
param_trunc <- MASS::fitdistr(w_trunc, densfun = dweibull, start = list(scale = 1, shape = 1))
est_scale_trunc <- as.numeric(param_trunc$estimate[1]) ; est_shape_trunc <- as.numeric(param_trunc$estimate[2])

est_par_trunc_mean <- est_scale_trunc*gamma(1+1/est_shape_trunc)
est_par_trunc_mean  ; abs(est_par_trunc_mean - theo_mean)

# Theoretical mean based on initial set parameters
tmean_init <- theoretic_mean(shape = shape, scale = scale, Trunc = Trunc)
tmean_init ; abs(tmean_init - theo_mean)

# # Theoretical mean based on estimated parameters
# tmean_est <- theoretic_mean(shape = est_shape_trunc, scale = est_scale_trunc, Trunc = Trunc)
# tmean_est ; abs(tmean_est - theo_mean)

# Mean based on AUC with our method
fit_wt <- prodlim(Hist(time, stat) ~ 1,
                  data = data.frame(time = w_trunc,
                                    stat = 1))
pred_wt <- predict(fit_wt,times = seq(0,Trunc-0.2,0.1))
F1_wt <- 1 - pred_wt
G1_wt <- F1_wt/max(F1_wt)
auc_wt_mean <- mean_age(seq(0,Trunc-0.2,0.1), 1 - G1_wt)
auc_wt_mean ; abs(auc_wt_mean - theo_mean)

# Mean based on AUC from CDF with estimated parameters
CDF_est_trunc <- pweibull(seq(0,Trunc,0.1),shape = est_shape_trunc, scale = est_scale_trunc)
G_est_trunc <- CDF_est_trunc/max(CDF_est_trunc)
auc_cdfest_mean <- mean_age(seq(0,Trunc,0.1),1-G_est_trunc)
auc_cdfest_mean ; abs(auc_cdfest_mean - theo_mean)

# Mean based on the AUC for the CDF of the truncated weibull with set parameters
CDF_trunc_set <- (1-exp(-(seq(0,50,0.1)/scale)^shape))/(1-exp(-(Trunc/scale)^shape))
auc_cdfset_mean_trunc <- mean_age(seq(0,50,0.1), 1-CDF_trunc_set)
auc_cdfset_mean_trunc ; abs(auc_cdfset_mean_trunc - theo_mean)

# Mean based on the AUC for the CDF of the truncated weibull with estimasted parameters
CDF_trunc_est <- (1-exp(-(seq(0,50,0.1)/est_scale_trunc)^est_shape_trunc))/(1-exp(-(Trunc/est_scale_trunc)^est_shape_trunc))
auc_cdfest_mean_trunc <- mean_age(seq(0,50,0.1), 1-CDF_trunc_est)
auc_cdfest_mean_trunc ; abs(auc_cdfest_mean_trunc - theo_mean)

###############################################################################

# Simulate from a truncated weibull
z <- inv_trunc_weibull(shape = shape, scale = scale, Trunc = Trunc, u = runif(n_sim))

z_mean <- mean(z)
z_mean ; abs(z_mean - theo_mean)

# Estimte parameters of truncated weibull sample
param_z <- MASS::fitdistr(z, densfun = dweibull, start = list(scale = 1, shape = 1))
est_scale_z <- as.numeric(param_z$estimate[1]) ; est_shape_z <- as.numeric(param_z$estimate[2])

est_par_z_mean <- est_scale_z*gamma(1+1/est_shape_z)
est_par_z_mean ; abs(est_par_z_mean - theo_mean)

# # Theoretical mean based on estimated parameters
# tzmean_est <- theoretic_mean(shape = est_shape_z, scale = est_scale_z, Trunc = Trunc)
# tzmean_est ; abs(tzmean_est - theo_mean)

# Mean based on AUC with our method
fit_z <- prodlim(Hist(time, stat) ~ 1,
                 data = data.frame(time = z,
                                   stat = 1))
pred_z <- predict(fit_z,times = seq(0,Trunc-0.1,0.1))
F1_z <- 1 - pred_z
G1_z <- F1_z/max(F1_z)
auc_z_mean <- mean_age(seq(0,Trunc-0.1,0.1), 1 - G1_z)
auc_z_mean ; abs(auc_z_mean - theo_mean)

# Mean based on AUC from CDF with estimated parameters
CDF_est_z <- pweibull(seq(0,Trunc,0.1),shape = est_shape_z, scale = est_scale_z)
auc_cdfestz_mean <- mean_age(seq(0,Trunc,0.1),1-CDF_est_z)
auc_cdfestz_mean ; abs(auc_cdfestz_mean - theo_mean)

# Mean based on the AUC for the CDF of the truncated weibull with estimasted parameters
CDF_trunc_zest <- (1-exp(-(seq(0,50,0.1)/est_scale_z)^est_shape_z))/(1-exp(-(Trunc/est_scale_z)^est_shape_z))
auc_cdfzest_mean_trunc <- mean_age(seq(0,50,0.1), 1-CDF_trunc_zest)
auc_cdfzest_mean_trunc ; abs(auc_cdfzest_mean_trunc - theo_mean)

# #  Weibull (truncated, censored) ------------------------------------------
# 
# # simulate a simple weibull with censoring
# wt <- pmin(w,Trunc)
# wtc_1 <- wt[censor_stat == 1]
# wtc <-  
# 
# mean(wtc) ; abs(mean(wtc) - theo_mean)
# 
# # Estimate parameters of weibull with censoring
# para_cens <- MASS::fitdistr(wc, densfun = dweibull, start = list(scale = 1, shape = 1))
# est_scale_cens <- as.numeric(para_cens$estimate[1]) ; est_shape_cens <- as.numeric(para_cens$estimate[2])
# 
# # Mean based on estimates of variable
# est_par_mean_cens <- est_scale_cens*gamma(1+1/est_shape_cens)
# est_par_mean_cens ; abs(est_par_mean_cens - theo_mean)
# 
# # Mean based on AUC with our method
# fit_wc <- prodlim(Hist(time, stat) ~ 1,
#                   data = data.frame(time = w,
#                                     stat = censor_stat))
# pred_wc <- predict(fit_wc,times = seq(0,Trunc,0.1))
# F1_wc <- 1 - pred_wc
# G1_wc <- 1 - F1_wc/max(F1_wc)
# auc_wc_mean <- mean_age(seq(0,Trunc,0.1), G1_wc)
# auc_wc_mean ; abs(auc_wc_mean - theo_mean)
# 
# 
# # # Mean based on AUC from CDF
# # CDF_w <- pweibull(seq(0,Trunc,0.1),shape = shape, scale = scale)
# # auc_cdfw_mean <- mean_age(seq(0,Trunc,0.1),CDF_w)
# 
# # Mean based on AUC from CDF with estimates parameters
# CDF_est_cens <- pweibull(seq(0,Trunc,0.1),shape = est_shape_cens, scale = est_scale_cens)
# auc_cdfest_mean_cens <- mean_age(seq(0,Trunc,0.1), CDF_est_cens)
# auc_cdfest_mean_cens ; abs(auc_cdfest_mean_cens - theo_mean)
# 
# 
# theo_mean



































































