# Cohort sample figure

# This script shows how the figure depicting the cohorts for the two scenarios.

# Packages used:
# ggplot2

triangle <- data.frame(x = c(2021,2021,2041),
                       y = c(75,95,95))
cohort <- data.frame(x = c(1994,2014,2021,2001),
                     y = c(75,95,95,75))
add_cohort <- data.frame(x =  c(2001,2021,2021),
                         y = c(75,95,75))

ggplot() + 
  expand_limits(x = c(1994,2041), y = c(75,95)) + 
  geom_vline(aes(xintercept = 2021)) +
  geom_polygon(data = triangle, aes(x = x, y = y), alpha = 0.6, fill = "yellow") +
  geom_polygon(data = cohort, aes(x = x, y = y), alpha = 0.6, fill = "blue") +
  geom_polygon(data = add_cohort, aes(x = x, y = y), alpha = 0.6, fill = "forestgreen") +
  geom_abline(aes(intercept = 75 - c(1994,2001,2021), slope = 1), linetype = c(1,5,1)) +
  scale_x_continuous(expand = expansion(c(0,0.01)),
                     breaks = c(1994,2001,2011,2021,2031,2041)) +
  scale_y_continuous(expand = expansion(c(0,0))) +
  labs(x = "Calender year", y = "Age in years") + 
  theme_bw() + 
  theme(text = element_text(size = 15))
