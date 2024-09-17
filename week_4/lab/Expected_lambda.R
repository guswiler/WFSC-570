# Week 4 Lab Notes
# 2024-09-17

# This script provides another example showing the deterministic (B0 + B1 * X; what we expect to see for Y) and
# stochastic components (what we actually see, variation around the expected value) of a (very simple) generalized linear model
# using the example from the lecture on Kellner et al.'s discussion
# of using finite rate of population growth (lambda) to evaluate
# habitat suitability and quality.

# We will simulate annual values of lambda using an intercept-only
# GLM with Gaussian (normal) errors. When I say intercept-only, I
# mean that the deterministic component of our GLM (the expected
# value of lambda) is defined simply by a single value, i.e.,
# a single mean.

# Expected (mean) lambda
E_lambda <- 1.1 # population growing 10% from one time period to the next

# Number of years for which we wish to simulate lambda
years <- 100

# The amount of error or noise in our simulated values of lambda.
# This is also the standard deviation of the normal distribution
# which we will use to simulate values of lambda.
variability <- 0.125

# Now simulate lambda
lambda_t <- rnorm(years, # Number of simulated values
                  E_lambda, # The mean of this normal distribution, also our expected value of lambda
                  variability) # The SD of this normal distribution

# Plot our simulated values of lambda
plot(x = seq(1,years),
     y = lambda_t,
     type = "b",
     ylim = c(0.7,1.5))

# Add a horizontal line showing our mean or expected value of lambda
abline(h = E_lambda, lwd = 3)

# plot line at 1 to visualize what years our population declining 
abline(h = 1, lwd = 1)



# reminder: B0 = intercept; B1 = slope

# Remember how we illustrated expected values using the example of measuring our response variable (y) many times at a single value of our covariate (x). Our stochastic component of our GLM (the error term or error component) would describe the variation in these repeated samples.

# Lets illustrate this by plotting our expected value of lambda as a single point and plotting our simulated values of lambda over that point.

# we only have one site, multiple years

plot(x = 1,
     y = E_lambda,
     cex = 3,
     pch = 21,
     ylab = "Lambda",
     xlab = "",
     bg = "grey")
points(x = rep(1,years),
       y = lambda_t)

# if we reduce the variability, we see our simulated data closer to our mean (expected value of lambda)




# We can also show that information in a different form using a histogram. Lets create a histogram of our simulated lambda values

hist(lambda_t,
     xlim = c(0.7,1.5),
     freq = F)

# Now lets add a vertical red line showing our expected value of
# lambda
abline(v = E_lambda, lwd = 3, col = "red")

# We can add a curve over these points representing the normal distribution we have used to simulate these data
curve(dnorm(x, mean = E_lambda, sd = variability),
      from = 0, to = 2, add = T, lwd = 2, col = "blue")



# Finally, lets calculate our "errors" or residuals, defined as the difference between our simulated values and our expected value

errors <- E_lambda - lambda_t

hist(errors, xlim = c(-0.4,0.4),
     freq = F)
abline(v = 0, lwd = 3, col = "green")

# We can plot the same normal curve but this time specify the
# mean as zero since the mean of our errors in this example
# will be zero

curve(dnorm(x, mean = 0, sd = variability),
      from = -1, to = 2, add = T, lwd = 2, col = "blue")
