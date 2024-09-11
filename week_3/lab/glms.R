rm(list=ls()) # clean workspace.  Caution!!!!

# Lab 3: Introduction generalized linear models (GLMs)

# In this lab we will cover generalized linear models. While this may be a review of classes you are currently taking or have already taken, analyses of wildlife habitat associations rely heavily on GLMs and therefore a good grasp on GLMs is important for understanding wildlife habitat analyses.

# We will also take this lab as an opportunity to introduce concepts of data simulation which are important for understanding the underlying mechanics of the models you are working with but which can also be a powerful tool in your analytical toolbox. 
# For example, you might be concerned that a small sample size will have too much of a detrimental affect on your results. You can simulate data under your observed sample size and see what type of precision and bias you might expect your parameter estimates.



# In this lab we will cover the two families of GLMs that are most relevant for wildlife habitat analyses: 
  # binomial GLM: modeling binary data (probability of getting y/n heads/tails, presence/absence)
  # and the Poisson GLM 



# Before we discuss these GLMs, lets review the basic ideas behind slopes and intercepts in regression models.
# These concepts are important to grasp because when we start analyzing wildlife-habitat data, most of our inferences are going to come from the slopes estimated using regression-type analyses.

# Lets start with simple linear regression: ----
  # one response variable (y): what you go out an measure in the field, trying to figure out what explains the variation in your measurements and why
  # and one explanatory variable or covariate (x)
  # how does y respond to increase/decrease in x? ----


# You may have seen the equation for a straight line in high-school algebra or an undergraduate stats class represented something like this:
  # y = m*x + b or y = m*x + c
# Does anyone remember what these letters represent?
  # m = slope
  # b (or c) = y-intercept
  # y = response variable
  # x = explanatory variable

# Lets plot some data for y and x. Lets say that y = home range size (in hectares) of a forest dwelling bird and x = the proportion of forest land cover in a 200-m circular buffer around the center of the bird's home range

y <- c(6.1,4.2,5.7,2.3,5.1,8.5,7.7,3.2)
x <- c(0.9,0.5,0.7,0.3,0.4,0.6,0.7,0.2)

plot(y ~ x, ylim = c(0,10), xlim = c(0,1),
     ylab = "Bird Home Range Size (ha)",
     xlab = "Proportion Forest Cover")

# We can see that there is something of trend here: ----
  # as x goes up, y also goes up
  # as forest cover increases, bird home range size also increases
  # positive relationship/association ----

# We want to quantify the relationship
# Now lets create a statistical model that describes how home range size changes as a function of surrounding forest cover.
# Lets start with using a simple linear regression:
  # a model that assumes a linear relationship between y and x (here, home range size and proportion forest) cover.
# We can estimate a simple linear regression model using the lm() function (lm for linear model).


# The code here is very simple:

lm_mod <- lm(y ~ x) # how does y change as a function of x ( how does y change as x changes)
class(lm_mod)       # class of this object is "lm"

# Our newly create lm object (lm_mod) has all the information we need to plot a straight line through our y/x data.
abline(lm_mod,  # use abline() to fit an lm object to our plot
       lwd = 2) # Line width, for a thicker line


# Now lets look at the summary() of our lm object.
summary(lm_mod)

# Output:
    # Residuals:
    #   Min      1Q  Median      3Q     Max 
    # -1.5805 -1.0625 -0.3375  0.8018  2.7482 
    # 
    # Coefficients:
    #   Estimate Std. Error t value Pr(>|t|)  
    # (Intercept)    1.894      1.528   1.240   0.2614  
    # x              6.429      2.636   2.439   0.0505 .
    # ---
    #   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
    # 
    # Residual standard error: 1.622 on 6 degrees of freedom
    # Multiple R-squared:  0.4979,	Adjusted R-squared:  0.4142 
    # F-statistic:  5.95 on 1 and 6 DF,  p-value: 0.05052


# What do we see in our summary output? We actually see quite a bit of information but there are three pieces of information we need to focus on for now. These are three parameters that are estimated from our data on y and x. ----
  # 1. y-intercept aka β₀
  # 2. slope (here, x is the name of our explanatory variable, which r names our slope in the output)
  # 3. residual standard error, model will never describe all the variation within our collected points
#  Two of these parameters are often called
# coefficient or beta estimates. We can use the coef() function
# to pull out our model's coefficient estimates. ----

coef(lm_mod)
    # (Intercept)          x 
    # 1.894389      6.429043 

# Lets talk about these two parameters more.

# 1) The y-intercept [(Intercept)] ----
  # The y-intercept (usually just called "the intercept" because we really don't care about the x-intercept) is the point at which the model's line crosses the y-axis, which occurs when x = 0.

# Where is this point? We can plot it!
abline(v = 0) # Plots a vertical line originating from x = 0
points(x = 0,               # x "coordinate" for our point
       y = coef(lm_mod)[1], # y "coordinate" for point; [] used for indexing, here we want the 1st col
       pch = 21,            # point symbol, hollow circle
       bg = "grey",         # background color for hollow symbols
       cex = 2)             # point size ----

# 2) The slope [here, x, but will be output as whatever you name the explanatory variable] ----
  # The slope describes how your response variable (y) changes as our explanatory variable or covariate (x) changes.
  # The slope is often called the effect size because the magnitude of the slope describes the strength of the effect that x has on y. If the slope is large, x has a stronger effect on y than if the slope is small. ----

# Lets look at our slope again
coef(lm_mod)[2]

# What does this value by itself tell us about how bird home range size changes as a function of proportion forest cover?
    # positive interaction, as proportion of forest cover incr, home range size incr

# Now lets use our equation for a straight line to calculate bird home range size at any given value of proportion forest cover. Remember the equation? y = m*x + b

b <- coef(lm_mod)[1]
m <- coef(lm_mod)[2]

# Forest cover = 0.00
m*0.00 + b
# Does this look familiar? We've calculated our intercept!

# Forest cover = 1.00
m*1 + b
# Lets plot this value
points(x = 1,
       y = m*1 + b,
       pch = 21,            # point symbol, hollow circle
       bg = "green",         # background color for hollow symbols
       cex = 2)             # point size

# What is the difference between these two values?
(m*1 + b) - (m*0.00 + b)
    # x = 6.429043
# Does this look familiar? We've calculated our slope!



# Lets plot horizontal lines representing the two predicted values of home range size we have just calculated.

abline(h = m*0.00 + b,
       lty = 3) # Line type, dashed line
abline(h = m*1.00 + b,
       lty = 3) # Line type, dashed line
# The distance between the points on these two horizontal lines is our slope.

# The two parameters we have just estimated, the intercept and the slope, make up one of the three fundamental components of generalized linear models (GLM):
  # The deterministic function: It is called the deterministic function because there is no uncertainty
    # when we plug in a given value of x, we will get the same value of y every time.
    # Every regression-type model has a deterministic function that describes how y is expected to change as x changes.
      # There is a key word, expected. The values we have been calculating with our equation for a straight line are also called expected values. Why call them expected values and not something a bit more certain or confident?
      # Because of the third parameter in our lm object we need to consider.



# 3) The residual standard error ----
#     You probably have gathered that real-world data in
#     natural resources can be highly variable or highly
#     heterogeneous. These are fancy words for "messy!"
#     This "messiness" in our data comes from the fact that
#     the straight line we just plotted doesn't fit perfectly
#     through every data point. In fact, some of our points
#     are quite far from this fitted line, which means that
#     if we went out and estimated a bird's home range
#     size when proportion forest cover was 0.60, we may have
#     observed an estimate quite different from what was modeled:

# deviation of points from our linear slope
m*0.60 + b
points(x = 0.60,
       y = m*0.60 + b,
       pch = 21,
       bg = "lightblue",
       cex = 1.5)
# our plotted point on the slope does not match our measured value
# what we expect (modeled) vs what is observed (measured)


# This residual standard error/deviation is important because it
# describes the second fundamental component of all 
# generalized linear models, the stochastic model or
# error term.
# All regression-type models, including GLMs,
# need an error term to describe the variation or scatter
# in our data that is not explained by our deterministic 
# function.
# When selecting a statistical model with which to model our data, a key choice is our choice of stochastic model.



# Lets illustrate the choice of a stochastic model by simulating some data.
# simulating our data allows us to verify that our model is doing what we want it to do and that we are using the correct model.
# For this example, we will simulate data using a Gaussian or normal error distribution although this will be the only time in this class where we will use a model with a Gaussian error distribution. bell shaped distribution

# Specify the number of birds for whose home range sizes we will simulate
n <- 30 

# Now lets specify our intercept and slope. But instead
# of the m*x + b notation, we will switch over to using
# notation more consistent with our use of the term
# betas.
# The intercept is usually specified as beta_0 while the slope is usually specified as beta_1.

b0 <- 2 # Intercept
b1 <- 6 # Slope

# Now lets simulate some values of proportion forest cover.
# We can draw "n" values from a uniform distribution between 0 and 100 and then divide by 100 to get proportions.
    # later on, useful to simulate data using your actual sample sizes to determine if your model is accurate


X <- runif(n,   # Number of random samples
           0,   # Lower bound on the uniform distribution
           100)/100 # Upper bound on the uniform distribution
# runif() pulls n (you specify) random numbers in a normal distribution between lower and upper bounds (you specify)
# if we plotted these values, would this look like actual collected data?
  # No, we are missing the error around each point


# Now we are ready to simulate our response values, our y's. 
# Because we are simulating data using a normal error distribution we will use R's rnorm() function.
?rnorm()

# You will see that we have three arguments to consider.
  # n is the number of random samples.
  # mean is the mean of our normal distribution.
    # Do you know what our mean is here?
    # The mean is another word for the expected value and how do we get our expected value? From our deterministic function.
    # So the mean is our equation for a straight line.
  # Finally, we have sd for standard deviation.
    # This is our residual standard error and describes the amount of "noise" or deviation around our expected value. 

# Lets simulate some data.

Y <- rnorm(n,         # Number of bird home ranges
           b0 + b1*X, # Mean or expected value, which is our deterministic function
           1.622)     # Standard deviation, error, or noise around our expected value.

# b0 + b1*X, these output values are the mean values we would expect to see if we measured 100 birds at each forest cover value. We would expect them to be centered on our slope line.

# Now plot our data and fit a lm model to it so that we can add a fitted line to our plot.
plot(Y ~ X,
     ylim = c(0,10),
     xlim = c(0,1))

abline(a = b0, b = b1, col = "red")
points(x = 0, y = b0, bg = "green", pch = 21, cex = 2)
points(x = 1, y = b0 + (b1*1), bg = "green", pch = 21, cex = 2)
# if we want slope, we can subtract highest point from lowest point (1 unit change in x because proportional)


### Ended 2024-09-10 notes here!!

new_lm_mod <- lm(Y ~ X)
abline(new_lm_mod)

# What is our intercept?
est_intercept <- coef(new_lm_mod)[1]
est_intercept

abline(v = 0)
points(x = 0,
       y = est_intercept,
       pch = 21,
       bg = "grey",
       cex = 2)

# What is our slope?
est_slope <- coef(new_lm_mod)[2]
est_slope

(est_slope*1 + est_intercept) - (est_slope*0.00 + est_intercept)

# How do our estimates of intercept and slope compare to our
# true values?
b0
est_intercept

b1
est_slope

# The binomial GLM -------------

# In a study that yields binary response data, for example presence/absence 
# data from a habitat selection study or whether or not an individual 
# exhibited symptoms of a disease, the error distribution that naturally 
# accommodates the discrete and 0,1 bounding of the data is the 
# binomial distribution. The binomial distribution has two 
# parameters: p, the per trial probability of success, and n, the trial size. 
# You can think of n as the number of times you flip a single coin 
# and p as the probability of getting heads on any given flip. A special 
# case of the binomial distribution is when each coin is only flipped 
# once, i.e., the trial size is n = 1, and therefore produces binary data. 
# The binomial distribution where n = 1 is called the Bernoulli distribution. 
# The expected value (mean) of the binomial distribution is n*p, so it
# follows that the mean of the Bernoulli distribution is simply p. For most
# applications of the binomial distribution in wildlife habitat analysis
# one will use the Bernoulli distribution.

# Lets illustrate the Bernoulli distribution using the rbinom() function.
# rbinom() is the function to simulate random values according to a binomial
# distribution. As always when seeing a new function, take a look
# at the help page.

?rbinom

# You will see that there are three arguments in the rbinom() function.
# n is the number of random values we wish to generate,
# size is the trial size (number of coin flips), and p is 
# the per trial probability. Remember that we can think
# of p as the probability of getting heads (getting a 1) for
# any given coin flip.

# Lets start by randomly selecting 1000 binomially distributed 
# values with trial size equal to one and per-trial probability of 0.5

y <- rbinom(n = 1000, # number of random numbers to simulate
            size = 1, # trial size (number of coin flips)
            p = 0.5)  # per trial probability (probability of
                      #  getting a 1 or flipping heads

# Now lets use the table function to see how many ones and how
# many zeros we generated.

table(y)

# We can verify that p is 0.5 by taking the mean of our data. Remember
# that for binary data, the mean is equal to p.

mean(y)

# Remember that there are three components to a GLM.
# 1) the linear predictor which is a deterministic function
# 2) the appropriate error distribution for the response data
# 3) the link function that maps the expected values from the 
#    the linear predictor scale to the scale of the response
#    data.

# We can verify that per-trial probability is indeed 0.5 if we fit an
# intercept-only binomial GLM. Remember that the intercept gives the 
# expected or mean value of the response (y) at a given value of 
# the covariate (x). We have no covariates here, and hence no slopes
# to estimate. We will simply estimate the intercept and in this
# case the intercept represents the mean or expected value of 
# our randomly generated binomial data (i.e., the mean of y).
# This if this as:
# (m * 0 + b) or (b0 + b1 * 0) which collapses to:
# b or b0

# Our linear predictor is simply an intercept describing the mean
# value of our response. We specify intercept-only models using
# 1 to the right of the ~.
# The error distribution is binomial. The
# link function is the logit link which allows us to model binary
# data on a continuous scale. We are going to let the link 
# function be a bit of a black box for now. The important thing
# to remember is that the link function allows us to take
# non-continuous non-linear data and model it on a 
# continuous linear scale. This in turn allows us to interpret
# the intercepts and slopes that same way that we interpreted
# them for the Gaussian or normal regression model earlier. 

# Within the glm() function there is the family= argument which 
# specifies both the error distribution and the link function.
# We can see this explicitly in the following expression. 

mod <- glm(y ~ 1, family = binomial(link = "logit"))

# Which is the same as

mod <- glm(y ~ 1, family = "binomial")

# Lets see what our expected value is

summary(mod)

# What do we see here?

# We only have an intercept value (no slope) because we
# have an intercept-only model. It is on the logit scale,
# not the probability scale, so we need to do some back-
# transformation to get our intercept onto the probability
# scale. We do this using the logit transformation. We can
# do this "manually" by dividing the exponent of our intercept
# by one plus the exponent of the intercept.

b0 <- coef(mod)

exp(b0)/(1 + exp(b0))

# Or we can use the plogis() function

plogis(b0)

# Notice that we get the same result as taking the mean of
# our response variable, y. This is because the intercept is
# the mean or expected value of our response variable when
# x is zero.

# What if we want to model binary response data as a function of covariates?
# This would allow us to estimate slopes for each covariate describing the 
# change in our expected value of the response (p) as a function of the
# covariate. For wildlife habitat analyses this might how does the 
# probability of a species being present at a sampling site
# vary as a function of the surrounding landscape composition.

# Lets set up a simulation the same way as we did with our Gaussian 
# distributed data. We'll specify a sample size, a slope and intercept,
# and randomly generate values of x.

# We will use a large sample size to cut down on the variability
# in our estimated parameters.
n <- 500

b0 <- -4.5 # Intercept
b1 <- 0.09 # Slope

X <- runif(n,   # Number of samples
           0,   # Lower limit of the uniform distribution
           100) # Upper limit of the uniform distribution

# Lets combine our parameters into the linear predictor for our model

lin_pred <- b0 + b1 * X

# We can use the plogis() function to see our linear predictor values
# on the probability scale

link <- plogis(lin_pred) 
summary(link)

# What happens if X = 0 in this example?
plogis(b0 + b1 * 0)

# How does this compare to the smallest value we see in "link?"

# Now lets simulate our response variable using rbinom. We
# are using a trial size of one because we are simulating
# Bernoulli trials (i.e., coin flips), such as we would have
# if we were doing presence/absence surveys at a site. 

y <- rbinom(n,    # Number of samples
            1,    # Trial size (one because we have Bernoulli
                  # trials or coin flips)
            link) # Per trial probabilities

# You can see that we have simulated a vector of binary data
# representing the outcome of each Bernoulli trial (i.e., of
# each coin flip).
table(y)

# Lets plot these binary data against our covariate X

plot(y ~ X, 
     ylim=c(0,1), 
     pch=16, 
     col=adjustcolor(4,alpha.f = 0.3), # Semi-transparent points
     cex=1.5, 
     main="Binary data")

# How can we model these binary data as a continuous linear function
# of our covariate, X? The answer is the link function.

# Lets fit our Binomial GLM using the glm() function and being sure
# specify that we are using a binomial error distribution with 
# the family= argument. The default link for the binomial family
# is the logit link so we do not need to specify it here.

bin.glm <- glm(y ~ X, family="binomial")

# What are our intercept and slope coefficient estimates?

coef(bin.glm)

# Remember that these are on the logit scale, not the probability
# scale. But lets stay on the logit scale to show that, as long as
# we are on the logit scale, we can think about these coefficients
# the same way we do for coefficients estimated from simple
# Gaussian or normal regression models.

# First, lets plot the expected values of y against X. This means
# plotting the values, on the logit scale, from our deterministic
# function against our covariate.

plot(lin_pred ~ X,
     ylim = c(-6,6),
     ylab = "Response variable on the logit scale (y)",
     xlab = "Covariate (X)")

# Where should our intercept fall?
est_b0 <- coef(bin.glm)[1]
est_b0
abline(v = 0)
points(x = 0,
       y = est_b0 + est_b1*0,
       pch = 21,
       bg = "grey",
       cex = 2)

# What is our slope?
est_b1 <- coef(bin.glm)[2]
est_b1

(est_b0 + est_b1*1) - (est_b0 + est_b1*0)

# Plotting coefficient estimates on the link scale for binomial data
# hopefully helps illustrate the conceptual links between simple
# Gaussian linear regression and GLMs but in the real world we
# typically want to see how per-trial probability varies as
# a function of our covariates. To do this, we need to be
# able to calculate expected values on the probability scale.
# Fortunately, this is relatively straight-forward to do.

# Here, we can illustrate these calculations using the plogis()
# function.

plogis(est_b0 + est_b1*0)
plogis(est_b0 + est_b1*1)
plogis(est_b0 + est_b1*10)
plogis(est_b0 + est_b1*50)
plogis(est_b0 + est_b1*90)
plogis(est_b0 + est_b1*100)

# Notice some major non-linearities in our expected probabilities?
# This is because the plogis() function ensures that our probabilities
# stay bounded between 0 and 1, which is appropriate given that
# our actual response variable is made up of 0's and 1's.

# Lets show how we can use the predict() function to calculate
# these expected values for us. predict() is a pretty useful
# function and if we look at the help we will see that there
# are several arguments, only three of which need to concern
# us for now.

?predict.glm

# We need to supply a glm object, a data frame containing the
# value(s) of our covariates at which we would like to 
# calculate the expected values, and type=, which we use
# to specify whether we want these expected values returned
# on the link (i.e., logit) scale or the response 
# (i.e., probability) scale. 

df <- data.frame(X = seq(0,100,1)) # Make sure your column
                                   # names are identical to 
                                   # the names of your 
                                   # covariate(s).

# Add a new column to df containing the expected values on 
# the link function.
df$lin.pred <- predict(bin.glm,     # Our glm object
                       newdata=df,  # The new data for which
                                    # we would like the expected
                                    # values.
                       type="link") # The scale of those expected
                                    # values
df$response <- predict(bin.glm, newdata=df, type="response")
head(df)
tail(df)

# Now lets re-plot our simulated data and add our expected values
# as lines.
plot(y ~ X, 
     ylim=c(0,1), 
     pch=16, 
     col=adjustcolor(4,alpha.f = 0.3),
     cex=1.5, 
     main="Binary data example")

lines(df$X, df$lin.pred, col="grey", lwd=1)
lines(df$X, df$response, col="red", lwd=2)

