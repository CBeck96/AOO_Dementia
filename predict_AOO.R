# Predicting Age of Onset

# In this script the cumulative incidence is computed. This is done for both scenarios. 
# Disclaimer: This is not the exact code used in the analysis, so the code is generalized.  

# Packages used:
# ggplot2
# ggExtra


# Data frame made to make the plots
dat_lines <- data.frame("cohort" = c(dat_cohort$cohort,
                                     dat_cohort_CO$cohort,
                                     (pred_lm_spline$cohort),
                                     (pred_lm_spline_CO$cohort)), 
                        "age" = c(dat_cohort$age,
                                  dat_cohort_CO$age,
                                  pred_lm_spline$pred.fit,
                                  pred_lm_spline_CO$pred.fit),
                        "col" = c(rep("A",5),
                                  rep("B",5),
                                  rep("A",nrow(pred_lm_spline)),
                                  rep("B",nrow(pred_lm_spline_CO))), 
                        "lt" = c(rep("A",5),
                                 rep("A",5),
                                 rep("B",nrow(pred_lm_spline)),
                                 rep("B",nrow(pred_lm_spline_CO))),
                        "group" = c(rep("C",5),
                                    rep("A",5),
                                    rep("D",nrow(pred_lm_spline)),
                                    rep("B",nrow(pred_lm_spline_CO))))


# The data frames used above is the following:
# dat_cohort            : Contains the cohort years and the predicted age of 
#                         onset at the given cohort time from a linear model on
#                         pseudo observation data.
# dat_cohort_CO         : Contains the cohort years and the predicted age of 
#                         onset at the given cohort time from a linear model on
#                         case only data.
# pred_lm_spline        : Contains the cohort years and the predicted age of 
#                         onset at the given cohort time from a linear model 
#                         with splines on pseudo observation data.
# pred_lm_spline_CO     : Contains the cohort years and the predicted age of 
#                         onset at the given cohort time from a linear model 
#                         with splines on case only data.

# These data frames are also used in the figures
# dat_point             : Contains the points for both the pseudo observations 
#                         and the case only.
# dat_CI                : This is similar to "dat_lines", but contains the 
#                         95% confidence intervals for the lines made.

legend_text <- c("Case only: Linear regression",
                 "Case only: Spline regression",
                 "Pseudo-observations: Linear regression",
                 "Pseudo-observations: Spline regression")

# Figure
pp_1 <- ggplot() +
  geom_point(data = dat_point, aes(x = avg_cohort, y = avg_exit, col = group),shape = " ") + 
  geom_line(data = dat_lines, 
            aes(x = cohort, y = age, col = group, linetype = group),
            size = 1.2) + 
  geom_ribbon(data = dat_CI, aes(x = cohort,
                                     ymin = lower,
                                     ymax = upper,
                                     fill = group,
                                     linetype = group),
              alpha = 0.2) +
  labs(x = "Calender year", y = "Age in years") + 
  scale_color_manual("", 
                     values = c("forestgreen","forestgreen","black","black"), 
                     label = legend_text,
                     aesthetics = c("fill","color")) +
  scale_linetype_manual("" , values = c(3,1,3,1),
                        label = legend_text) + 
  scale_x_continuous(expand = expansion(c(0,0.002))) +
  scale_y_continuous(expand = expansion(c(0,0.02)),
                     limits = c(75,95)) + 
  theme_bw() +
  theme(text = element_text(size = 15), legend.position = c(0.5,0.8)) 


# Making special plot with densities in the margins of the plots
ggMarginal(p = pp_1, type = "density", groupColour = TRUE)


## Figure
pp_2 <- ggplot() + 
  geom_point(data = dat_point, aes(x = avg_cohort, y = avg_exit, col = group), shape = " ") + 
  geom_line(data = dat_lines, 
            aes(x = cohort, y = age, col = group, linetype = group),
            size = 1.2) + 
  geom_ribbon(data = dat_CI, aes(x = cohort, 
                                     ymin = lower,
                                     ymax = upper,
                                     fill = group,
                                     linetype = group),
              alpha = 0.2) + 
  labs(x = "Calender year", y = "Age in years") + 
  scale_color_manual("", values = c("forestgreen","forestgreen","black","black"), 
                     label = legend_text, 
                     aesthetics = c("fill","color")) +
  scale_linetype_manual("" , values = c(3,1,3,1), 
                        label = legend_text) + 
  scale_x_continuous(expand = expansion(c(0,0.002))) +
  scale_y_continuous(expand = expansion(c(0,0.02)),
                     limits = c(83,85)) + 
  theme_bw() +
  theme(text = element_text(size = 15), legend.position = c(0.5,0.8)) 


# Making special plot with densities in the margins of the plots
ggMarginal(p = pp_2, type = "density", groupColour = TRUE)

