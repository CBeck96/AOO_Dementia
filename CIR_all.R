# Computations for for the cohorts regardless of sex

# In this script the cumulative incidence is computed. This is done for both scenarios. 
# Disclaimer: This is not the exact code used in the analysis, so the code is generalized.  

# Packages used:
# prodlim
# pracma
# ggplot2


# Mean for the full cohort:
res <- mean_age_of_onset(dat, rawdat = TRUE, se = TRUE) 

# Collects the desired results in a data frame for later plotting.
Dem_fig <- data.frame("time" = res$CIF$time,
                      "Est" = res$CIF$prob.1*100,
                      "lower" = c(0,res$lower$`1`*100),
                      "upper" = c(0,res$upper$`1`*100))

# Computing F(A), where A = 95. 
F95 <- max(res$CIF$prob.1*100)

# Saves the mean from the two methods (Area under the curve using the 
# Aalen-Johansen estimator, and case-only)
dem_res_mean <- data.frame("Method" = c("EA","CO"),
                           "Mean" = c(res$Mean$Mean.1, 
                                      mean(dat$Tslut[dat$censor_stat == 1])))

# Plotting the cumulative incidence regardless of sex
ggplot() + 
  geom_line(data = Dem_fig, aes(x = time, y = Est/100), col = "blue") + 
  # Adding confidence bands to the plot
  geom_ribbon(data = Dem_fig, aes(x = time, ymin = lower/100, ymax = upper/100),
              alpha = 0.3, fill = "blue") +
  # Adding the two means from the two methods
  geom_point(data = dem_res_mean, 
             aes(x = Mean, y = 0.5/100,
                 shape = factor(Method)),
             size = 5, col = "blue") +
  # Adding lines for the median and F(A)
  geom_segment(aes(x = c(75,75,res$median[[2]]),
                   xend = c(95,res$median[[2]],res$median[[2]]),
                   y = c(c(F95,F95/2,F95/2)/100),
                   yend = c(c(F95,F95/2,0))/100),
               linetype = 2,
               col = "blue") +
  labs(x = "Age in years", title = "") +
  scale_shape_manual("", values = c(3,6), label = c("Case-only", "New method")) +
  scale_x_continuous(expand = expansion(c(0,0.002)), limits = c(75,95)) +
  scale_y_continuous(expand = expansion(c(0,0.02)),  limits = c(0,17.5)/100,
                     "Absolute risk") +
  annotate("text", x = 76, y = 17/100, label = "F(95)", size = 6) + 
  theme_bw() +
  theme(text = element_text(size = 18),
        legend.position = c(0.75,0.2))
