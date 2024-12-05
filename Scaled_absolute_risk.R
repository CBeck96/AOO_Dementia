# Covariate year of 75 

# In this script we compute the MAOO for different covariates. In the paper it 
# is done in the two different scenarios. For the scenario 'Complete follow-up', 
# the cohort years: 1994, 1997, 1999, and 2001 are used. For the scenario 
# 'Total cohort' the years: 1994,2001,2011, and 2011 are used. For the year 1994,
# the date is set at the first of January, and all the other years the date is 
# 31. of Dacember. In this script it is also shown how the figure showing the 
# scaled absolute risk. 

# NOTE: In the code below it is only shown for the dates of 1994 and 2001.

# Packages used:
# pracma
# riskRegression
# ggplot2

# Setting the dates
year_1994 <- as.Date("1994-01-01", "%Y-%m-%d")
year_2001E <- as.Date("2001-12-31", "%Y-%m-%d")

# Setting the ages at which the predictions are made
pred_age <- seq(75,95,0.1) 

# Predicitng for the two different dates
pred_1994 <- predict(riskReg_cohort, 
                     newdata = data.frame("coefYear2001" = (as.numeric(year_1994) - as.numeric(year_2001E))/365.25),
                     times = pred_age, cause = 1, se = T)
pred_2001 <- predict(riskReg_cohort, 
                     newdata = data.frame("coefYear2001" = (as.numeric(year_2001E) - as.numeric(year_2001E))/365.25),
                     times = pred_age, cause = 1, se = T)

# Computing the Mean age of onset and the confidence interval for the two dates
# CI(mean) 1994
mean_age(c(0,pred_age), c(1,as.numeric(1 - pred_1994$absRisk/max(pred_1994$absRisk))))
mean_age(c(0,pred_age), c(1,as.numeric(1 - pred_1994$absRisk.lower/max(pred_1994$absRisk.lower))))
mean_age(c(0,pred_age), c(1,as.numeric(1 - pred_1994$absRisk.upper/max(pred_1994$absRisk.upper))))
# CI(mean) 2001
mean_age(c(0,pred_age), c(1,as.numeric(1 - pred_2001$absRisk/max(pred_2001$absRisk))))
mean_age(c(0,pred_age), c(1,as.numeric(1 - pred_2001$absRisk.lower/max(pred_2001$absRisk.lower))))
mean_age(c(0,pred_age), c(1,as.numeric(1 - pred_2001$absRisk.upper/max(pred_2001$absRisk.upper))))

# Making data frame to use for plotting
cov_yr <- data.frame("age" = pred_age,
                     "G" = c(as.numeric(pred_1994$absRisk/max(pred_1994$absRisk)),
                             as.numeric(pred_2001$absRisk/max(pred_2001$absRisk))),
                     "Year" = c(rep("1994", length(pred_age)), 
                                rep("2001", length(pred_age))),
                     "Mean" = c(rep(mean_age(c(0,pred_age),
                                             c(1,as.numeric(1 - pred_1994$absRisk/max(pred_1994$absRisk)))),
                                    length(pred_age)),
                                rep(mean_age(c(0,pred_age),
                                             c(1,as.numeric(1 - pred_2001$absRisk/max(pred_2001$absRisk)))),
                                    length(pred_age))))
# Plot 
ggplot(data = cov_yr,
       aes(x = age)) + 
  geom_line(aes(y = G, col = Year)) +
  geom_segment(aes(x = cov_yr[Year == "1994",]$Mean[1],
                   y = 0,
                   xend = cov_yr[Year == "1994",]$Mean[1],
                   yend = max(G[(age <= cov_yr[Year == "1994",]$Mean[1]) & (Year == "1994")])),
               linetype = 2) + 
  geom_segment(aes(x = cov_yr[Year == "2001",]$Mean[1],
                   y = 0,
                   xend = cov_yr[Year == "2001",]$Mean[1],
                   yend = max(G[(age <= cov_yr[Year == "2001",]$Mean[1]) & (Year == "2001")])),
               linetype = 2, col = "red") + 
  scale_color_manual("  ",
                     values = c("black","red")) +
  labs(x = "Age in years", y = "Absolute risk", title = " ") +
  scale_x_continuous(expand = expansion(c(0,0.002))) +
  scale_y_continuous(expand = expansion(c(0,0.02))) + 
  theme_bw() +
  theme(text = element_text(size = 18),
        legend.position = c(0.7,0.2))



