# Olivia Guswiler / Caleb Milford
# Lab Assignment 2
# 2024-09-24

# load package and plot.glm() function into environment
library(ggplot2)
source("lab_assignments/lab_assignment_2/plot.glm.R")



# Part I: Effects on parameter estimate precision ----

# define sample sizes
sample_sizes <- c(25, 100, 500, 5000)

b0 <- -0.75 # Intercept
b1 <- 0.05 # Slope

# create empty data frame
results <- expand.grid(n = as.character(sample_sizes),
                       b0_est = NA, b0_lci = NA, b0_uci = NA,
                       b1_est = NA, b1_lci = NA, b1_uci = NA)

# loop function to calculate estimates and CIs for each sample size and place in df just created
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


# plot of β₀ and 95% CIs at each sample size, red line indicating the true value
ggplot(results, aes(x = n, y = b0_est)) +
  geom_hline(yintercept = b0,
             color = "red") +
  geom_errorbar(aes(ymin = b0_lci, ymax = b0_uci),
                colour = "black", width = 0.2) +
  geom_point(size = 4, shape = 18) + 
  labs(x = "Sample Size", y = "Intercept and 95% CI")

# save last printed plot as PNG with described file extension and name
ggsave("lab_assignments/lab_assignment_2/outputs/part1_intercept.png")

# plot of slope and 95% CIs at each sample size, red line indicating the true value
ggplot(results, aes(x = n, y = b1_est)) +
  geom_hline(yintercept = b1,
             color = "red") +
  geom_hline(yintercept = 0, lty = 3) +
  geom_errorbar(aes(ymin = b1_lci, ymax = b1_uci),
                colour = "black", width = 0.2) +
  geom_point(size = 4, shape = 18) + 
  labs(x = "Sample Size", y = "Slope and 95% CI")

ggsave("lab_assignments/lab_assignment_2/outputs/part1_slope.png")



# Part II: Plotting expected values from a binomial GLM ----

# plot with β1 = 0.025
plot.glm(n = 45,      # sample size
         b0 = -1,     # intercept
         b1 = 0.025,  # slope
         seed = 1467)

ggsave("lab_assignments/lab_assignment_2/outputs/part2_slope_0.025.png")

# plot with β1 = 0.07
plot.glm(n = 45,
         b0 = -1,
         b1 = 0.07,
         seed = 1467)

ggsave("lab_assignments/lab_assignment_2/outputs/part2_slope_0.07.png")



# Part III: Effect of sample size on expected value precision

# plot with n = 15
plot.glm(n = 15,
         b0 = -1,
         b1 = 0.05,
         seed = 1467)

ggsave("lab_assignments/lab_assignment_2/outputs/part3_n15.png")

# plot with n = 100
plot.glm(n = 100,
         b0 = -1,
         b1 = 0.05,
         seed = 1467)

ggsave("lab_assignments/lab_assignment_2/outputs/part3_n100.png")

# plot with n = 1000
plot.glm(n = 1000,
         b0 = -1,
         b1 = 0.05,
         seed = 1467)

ggsave("lab_assignments/lab_assignment_2/outputs/part3_n1000.png")

# plot with n = 10000
plot.glm(n = 10000,
         b0 = -1,
         b1 = 0.05,
         seed = 1467)

ggsave("lab_assignments/lab_assignment_2/outputs/part3_n10000.png")