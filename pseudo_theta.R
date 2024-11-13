# Pseudo theta 

# This function is used in computing the pseudo-observations. 
# Note that this a simplification of the mean_age_of_onset-function, since it 
# computes more than what is used for the pseudo-observations.

# Generate pseudo survival function
# F(t) = int_0^t S(u)a(u)du
# G(t) = 1 - F(t)/F(A)
# where F(A) = max(F(t))

# Packages used:
# prodlim
# riskRegression
# pracma

# Input:
# df      : Data used.

# Output:
#   - Returns the mean age of onset


pseudo_theta = function(df){
  mod <- CSC(Hist(time = Tslut, censor_stat) ~ 1,
             data = df)
  
  pred_F <- predict(mod,
                    cause = 1,
                    time = c(0,seq(75,95,0.25)),
                    newdata = df[1,])
  
  CIF <- as.numeric(pred_F$absRisk)
  
  
  G1 <- 1 - CIF/max(CIF)
  # Mean by integrating under the survival curve.
  aa <- mean_age(c(0,seq(75,95,0.25)),G1)
  return(aa)
} 

