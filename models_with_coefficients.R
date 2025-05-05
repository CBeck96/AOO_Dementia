# Age of dementia described by coefficients 

# In this script the age of dementia is computed as a function of 
# either sex or cohort. This is done with a linear model on the 
# case-only and pseudo-observations data. Additionally, a risk 
# regression model is fitted on the survival data using the 
# R-package 'riskRegression'.

# Packages used:
# riskRegression


dat_theta_res

dat_dem_coef <- dat_dem[,c(1,2,3,5,7,8,9)]


dat_lJoin <- dplyr::left_join(dat_dem_coef, dat_theta_res, by = "PID")

dat_coef <- dat_lJoin[,c(1,2,3,4,5,6,7,8,10,12)]

# The cohort variable is normalized around the end of the year
# 2001, for both scenarios. The variable used 'start_date', 
# which is birth date + 75 years. 
year_2001E <- as.Date("2001-12-31", "%Y-%m-%d")
dat_coef$coefYear2001 <- (as.numeric(dat_coef$start_date) - as.numeric(year_2001E))/365.25

## Case only, sex ##
lm_CO_kqn <- lm(Tslut ~ kqn + 0,
                data = dat_coef[dat_coef$censor_stat == 1,])
## Case only, cohort ##
lm_CO_cohort <- lm(Tslut ~ coefYear2001,
                   data = dat_coef[dat_coef$censor_stat == 1,])

## Pseudo observations, sex ##
lm_pse_kqn <- lm(theta ~ kqn + 0,
                     data = dat_coef)
## Pseudo observations, cohort ##
lm_pse_cohort <- lm(theta ~ coefYear2001,
                    data = dat_coef)

## Risk regression, sex ##
riskReg_kqn <- CSC(Hist(time = Tslut , censor_stat) ~ kqn,
                   data = dat_coef)
## Risk regression, cohort ##
riskReg_cohort <- CSC(Hist(time = Tslut , censor_stat) ~ coefYear2001,
                      data = dat_coef)


