n_obs <- 100000

shape = 1.25
scale = 10
Trunc = 20


theoretic_var <- function(shape, scale, Trunc = Trunc){
  m2u = scale^2/(1-exp(-(Trunc/scale)^shape))*gammainc(1+2/shape,(Trunc/scale)^shape)
  variance = as.numeric(m2u)[1] - theoretic_mean(shape = shape, scale = scale, Trunc = Trunc)^2
  return(variance)
}

x <- rweibull(n = n_obs, shape = shape, scale = scale)
xTrunc <- ifelse(x <= Trunc, x, NaN)
u <- runif(n_obs)
z <- inv_trunc_weibull(shape = shape, scale = scale, u = u, Trunc = Trunc)

test_dat <- data.frame(x = x,
                       xTrunc = xTrunc,
                       z = z)

s <- seq(0,30,0.25)
d <- dweibull(x = s, shape = shape, scale = scale)
d1 <- shape/scale*(s/scale)^(shape - 1)*exp(-(s/scale)^shape)/(1-exp(-(Trunc/scale)^shape))

dens_dat <- data.frame(s = s,
                       d = d,
                       d1 = d1)

ggplot(test_dat) + 
  geom_histogram(aes(x), fill = "cornflowerblue", alpha = 0.3, stat = "density") +
  # geom_histogram(aes(xTrunc), fill = "red", alpha = 0.2, stat = "density") +
  geom_histogram(aes(z), fill = "green", alpha = 0.2, stat = "density") +
  geom_line(data = dens_dat, aes(x = s, y = d), col = "black", size = 1) +
  geom_line(data = dens_dat, aes(x = s, y = d1), col = "purple", size = 1) +
  xlim(0,25) +  ylab("") + xlab("") + theme_bw()



data.frame(mean = c(mean(x),
                    mean(xTrunc, na.rm = T),
                    mean(z),
                    scale*gamma(1+1/shape),
                    theoretic_mean(shape = shape,scale = scale, Trunc = Trunc))) %>%
  mutate(lower = mean - c(sd(x)/sqrt(n_obs),
                          sd(xTrunc,na.rm = T)/sqrt(sum(!is.na(xTrunc))),
                          sd(z)/sqrt(n_obs),
                          sqrt(scale^2*(gamma(1+2/shape) - gamma(1+1/shape)^2))/sqrt(n_obs),
                          sqrt(theoretic_var(shape = shape,scale = scale, Trunc = Trunc))/sqrt(n_obs))) %>%
  mutate(upper = mean + c(sd(x)/sqrt(n_obs),
                          sd(xTrunc,na.rm = T)/sqrt(sum(!is.na(xTrunc))),
                          sd(z)/sqrt(n_obs),
                          sqrt(scale^2*(gamma(1+2/shape) - gamma(1+1/shape)^2))/sqrt(n_obs),
                          sqrt(theoretic_var(shape = shape,scale = scale, Trunc = Trunc))/sqrt(n_obs)))



# Survival ----------------------------------------------------------------

tsurv_dat <- test_dat %>% 
  mutate(censoring = pmin(rweibull(n_obs,scale = 25, shape = 1),Trunc)) %>%
  mutate(censor_stat = rbinom(n_obs,size = 1, prob = 0.15)) %>%
  mutate(Tslut = ifelse(censor_stat == 0, censoring, z)) 

ggplot(tsurv_dat) + 
  # geom_histogram(aes(z), fill = "green", alpha = 0.2, stat = "density") +
  geom_histogram(data = tsurv_dat %>% filter(censor_stat == 1),
                 aes(Tslut), fill = "cornflowerblue", alpha = 0.3, stat = "density") +
  geom_line(data = dens_dat, aes(x = s, y = d), col = "black", size = 1) +
  geom_line(data = dens_dat, aes(x = s, y = d1), col = "purple", size = 1) +
  xlim(0,25) +
  theme_bw()

fit = prodlim(Hist(Tslut,censor_stat) ~ 1, data = tsurv_dat)
pred = predict(fit, times = c(0,seq(0,Trunc,0.1)))
F1 = 1 - pred
G1 = 1 - F1/max(F1)
auc_mean = mean_age(c(0,seq(0,Trunc,0.1)),G1) 






data.frame(mean = c(theoretic_mean(shape = shape,scale = scale, Trunc = Trunc),
                    as.numeric(tsurv_dat %>% filter(censor_stat == 1) %>% summarise(mean = mean(Tslut))),
                    auc_mean)) %>%
  mutate(lower = mean - c(sqrt(theoretic_var(shape = shape,scale = scale, Trunc = Trunc)),
                          as.numeric(tsurv_dat %>% filter(censor_stat == 1) %>% summarise(sd = sd(Tslut))),
                          0))
  



# Competing risk ----------------------------------------------------------

tCR_dat <- test_dat %>% 
  mutate(censoring = pmin(rweibull(n_obs,scale = 25, shape = 1),Trunc)) %>%
  mutate(censor_stat = colSums(rmultinom(n = n_obs, size = 1, prob = c(0.1,0.15,0.75))*1:3)-1) %>%
  mutate(CR = inv_trunc_weibull(shape = shape, scale = scale, u = runif(n_obs))) %>%
  mutate(Tslut = ifelse(censor_stat == 0,censoring, ifelse(censor_stat == 1, z, CR))) %>%
  select(x,xTrunc,z,censoring,CR,censor_stat,Tslut)

ggplot(tCR_dat) + 
  # geom_histogram(aes(z), fill = "green", alpha = 0.2, stat = "density") +
  geom_histogram(data = tCR_dat %>% filter(censor_stat == 1),
                 aes(Tslut), fill = "cornflowerblue", alpha = 0.3, stat = "density") +
  # geom_line(data = dens_dat, aes(x = s, y = d), col = "black", size = 1) +
  geom_line(data = dens_dat, aes(x = s, y = d1), col = "purple", size = 1) +
  xlim(0,25) +
  theme_bw()

mod <- CSC(Hist(time = Tslut, censor_stat) ~ 1, data = tCR_dat)
pred_F <- predict(mod, cause = 1, time = c(0,seq(0,Trunc,0.1)),
                  newdata = tCR_dat[1,])
CIF <- as.numeric(pred_F$absRisk)
G1 <- 1 - CIF/max(CIF)
# Mean by integrating under the survival curve.
aa <- mean_age(c(0,seq(0,Trunc,0.1)),G1)


data.frame(mean = c(theoretic_mean(shape = shape,scale = scale, Trunc = Trunc),
                    as.numeric(tCR_dat %>% filter(censor_stat == 1) %>% summarise(mean = mean(Tslut))),
                    aa),
           var = c(theoretic_var(shape = shape,scale = scale, Trunc = Trunc),
                   as.numeric(tCR_dat %>% filter(censor_stat == 1) %>% summarise(var = var(Tslut))),
                   NaN))

