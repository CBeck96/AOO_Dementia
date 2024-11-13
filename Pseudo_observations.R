# Pseudo-observations 

# This scrips shows how the pseudo-observations are computed. 
# The code vary between the two scenarios described in the paper. 

# Packages used:
# prodlim
# pracma
# riskRegression
# ggplot2
# doParallel
# foreach

n_batch <- 10 # This is for full follow-up, otherwise it is 8
n <- nrow(dat) # Saves number of observations 

# Sets seed
set.seed(66)
draw <- sample(x = 1:n_batch, size = n, replace = TRUE, prob = rep(1,n_batch)/n_batch)

# Creates data set with batches
dat_pse_draw <- dat
dat_pse_draw$batch <- draw

# Checks number of cases in each batch
table(dat_pse_draw$batch,dat_pse_draw$censor_stat)

# Create list for saving results
dat_theta <- data.frame("PID" = c(),
                        "KQN" = c(),
                        "theta" = c(),
                        "theta_i" = c(),
                        "b" = c())

dat_theta_pse <- dat_theta

# creates vectors to save the values from the computations of the pseudo-obsevations.
vtheta <- rep(0,n_batch) ; v_nb <- rep(0,n_batch)

## Parallelized ##
# Note: Check your pc to see how many cores are available. 
cl <- makeCluster(14) ; doParallel::registerDoParallel(cl)

# Starting calculating the pseudo observations
for(b in 1:n_batch){
  dat_b <- dat_pse_draw %>% filter(batch == b)
  n_b <- nrow(dat_b)
  v_nb[b] <- n_b
  theta_b <- pseudo_theta(dat_b)
  vtheta[b] <- theta_b
  dat_theta_pse <- foreach(i = 1:n,
                           .packages = c("prodlim", "riskRegression", "pracma"),
                           .combine = "rbind") %dopar% {
                             theta_b_i <- pseudo_theta(dat_b[-i,])
                             theta_bi <- n_b*theta_b - (n_b - 1)*theta_b_i
                             c(dat_b$PID[i],dat_b$kqn[i],theta_bi,theta_b_i,b)
                             }
  
  dat_theta <- rbind(dat_theta,dat_theta_pse)
  
} ; stopCluster(cl)  

colnames(dat_theta) <- c("PID","KQN","theta","theta_i","b")

# Create list for saving results
dat_theta_res <- data.frame("PID" = dat_theta$ID,
                            "KQN" = dat_theta$KQN,
                            "theta" = as.numeric(dat_theta$theta),
                            "theta_i" = as.numeric(dat_theta$theta_i),
                            "b" = dat_theta$b)
