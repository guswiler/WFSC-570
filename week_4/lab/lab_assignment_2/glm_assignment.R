# Olivia Guswiler
# Lab Assignment 2
# 2024-09-24

# load package and plot.glm() function into environment
library(ggplot2)
source("week_3/lab/lab_assignment/plot.glm.R")



# Part I: Effects on parameter estimate precision ----

# define sample sizes
sample_sizes <- c(25, 100, 500, 5000)

b0 <- -0.75 # Intercept
b1 <- 0.05 # Slope

# create empty data frame
results <- expand.grid(n = as.character(sample_sizes),
                       b0_est = NA, b0_lci = NA, b0_uci = NA,
                       b1_est = NA, b1_lci = NA, b1_uci = NA)

# loop function to calculate estimates and CIs for each sample size
for(i in 1:nrow(results)){
  
  n_i <- as.numeric(as.character(results$n[i]))
  
  X_i <- runif(n_i,   # Number of samples
             0,   # Lower limit of the uniform distribution
             100) # Upper limit of the uniform distribution
  
  link_i <- plogis(b0 + b1 * X_i)
  
  y_i <- rbinom(n_i,    # Number of samples
              1,    # Trial size (one because we have Bernoulli
              # trials or coin flips)
              link_i) # Per trial probabilities
  
  glm_mod_i <- glm(y_i ~ X_i, family = "binomial")
  ci_i <- confint(glm_mod_i)
  
  results$b0_est[i] <- coef(glm_mod_i)[1]
  results$b1_est[i] <- coef(glm_mod_i)[2]
  results[i,c("b0_lci","b0_uci")] <- ci_i[1,]
  results[i,c("b1_lci","b1_uci")] <- ci_i[2,]
}

# These plots will plot your parameter estimates and their 95% CI
# The true value of the parameter is indicated by the solid black
# horizontal line. The dashed horizontal line indicates zero.

# plot of β₀ and 95% CIs at each sample size, red line indicating the true value
ggplot(results, aes(x = n, y = b0_est)) +
  geom_hline(yintercept = b0,
             color = "red") +
  geom_errorbar(aes(ymin = b0_lci, ymax = b0_uci),
                colour = "black", width = 0.2) +
  geom_point(size = 4, shape = 18) + 
  labs(x = "Sample Size", y = "Intercept and 95% CI")

# plot of slope and 95% CIs at each sample size, red line indicating the true value
ggplot(results, aes(x = n, y = b1_est)) +
  geom_hline(yintercept = b1,
             color = "red") +
  geom_hline(yintercept = 0, lty = 3) +
  geom_errorbar(aes(ymin = b1_lci, ymax = b1_uci),
                colour = "black", width = 0.2) +
  geom_point(size = 4, shape = 18) + 
  labs(x = "Sample Size", y = "Slope and 95% CI")



# Part II: Plotting expected values from a binomial GLM ----

# Pick a single sample size (something realistic) and then
# change the value for b0 and b1 to see how your expected
# values change as you change the intercept and effect size
# (i.e., slope or b1). The solid red line are the expected
# values while the grey shaded ribbon is the 95% CI around
# the expected values.
# Keep n the same while changing b0 and b1

plot.glm(n = 46,
         b0 = 1,
         b1 = -0.05,
         seed = 1467)


# Part III: Effect of sample size on expected value precision

plot.glm(n = 500,
         b0 = 1,
         b1 = 0.05,
         seed = 1467)
