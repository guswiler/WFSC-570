rm(list=ls()) # clean workspace.  Caution!!!!

### week 5 lab 2024-09-24 ----

# Lab 3: Occupancy modeling for wildlife habitat analyses

# Illustrate occupancy model use for quantifying wildlife-habitat relationships when working with presence-absence or use-non-use data
## account for our inability to perfectly detect our study species. That is, to account for our uncertainty in whether our zeros are actually true zeros. 

# Before we get into occupancy modeling revist the binomial generalized linear model (GLM) that we use for presence-absence or used-unused data
# We have already covered the components of the binomial GLM so lets use it to fit some presence-absence data. 


# The data for these lab are inspired by prairie-associated songbirds from the Midwest (e.g., Illinois). Across the Midwest USA, native prairie communities often exist as islands (small, isolated patches) within a matrix of row-crop agriculture (e.g., corn, soybeans). Many of these species are more likely to occur within patches of native prairie habitat that are larger.

# Our data represent 100 sites located on patches of native prairie habitat which vary in size.
# Our first example data set represent a single point-count survey conducted at each site to detect Bobolinks. ----




# Read in our data from CSV file
single_survey_data <- read.csv("lab/week_5/Bobolink_single_survey.csv")

# As always, view our data before doing anything else.
head(single_survey_data)

# We see a column denoting our detections of bobolinks (BOBO) and a column denoting the value of native prairie patch size (Native).
# Native has been rescaled to fit on the z-scale.
  # This just means that it has a mean = 0 and a SD = 1.
  # z-score standardizing continuous covariates is a good practice in many and puts continuous covariates on the same scale so you can more easily compare their effect sizes.
  # changing to z-scale helps models converge better
  # puts all of continuous covariates onto the same scale

# larger values (positive) indicate more native prairie habitat, smaller values (negative) indicate less native prairie habitat.
summary(single_survey_data)

# At how many sites did we detect bobolinks?
## mean of binary data will give you proportion of 1's
mean(single_survey_data$BOBO)  # BOBO present at 25% of sites surveyed
table(single_survey_data$BOBO) # use table() to get counts




# Fit a binomial GLM to these data and see if the size of native prairie patch influences the presence of bobolinks ----

BOBO_mod <- glm(BOBO ~ Native,             # y as a function of x (y ~ x)
                data = single_survey_data, # where we're getting the data from
                family = binomial)         # running a binomial GLM

summary(BOBO_mod)

# What do we see here?
  # an increase in the number of bobolinks as native prairie habitat increases
  # but it is not outside of what we would see randomly (insignificant P)
    #              Estimate   Pr(>|z|)
    # (Intercept)  -1.09887   1.96e-06
    #   Native      0.03201       0.89

# how to determine the probability of finding bobolinks at a site with an average amount of prairie habitat: plogis of the intercept
plogis(-1.09887) # 25% prob of BOBO presence at site with average amount of prairie



# Now lets plot the relationship between probability of bobolinks being present and the size of native prairie patch.
# We can use the predict function here
  # We need to provide predict() with covariate values (with X's) at which to predict probability of bobolinks being present


# So lets create a vector of values that range from the minimum to the maximum value of our actual Native covariate
Native_pred <- seq(min(single_survey_data$Native),
                   max(single_survey_data$Native),
                   length.out = 30)
Native_pred

# Add these new values to a new data frame which will be used to predict the probability of bobolinks being present given each of these values of Native.
BOBO_predicted <- data.frame(Native = Native_pred) 

# creat a column in this df with predicted probabilities calculated using our original model based on observed data
BOBO_predicted$p <- predict(BOBO_mod,                # Our glm object
                            newdata=BOBO_predicted,  # The new data for which
                                                      # we would like the expected
                                                      # values.
                            type="response")         # The scale of those expected
                                                      # values
head(BOBO_predicted)
tail(BOBO_predicted)

# Now lets our actual bobolink detections as a function of native prairie patch size
  # add a line showing the predicted (or expected) probability of bobolink being present as a function of native prairie patch size.

# plotting our 0,1 data as a function of the native prairie
plot(BOBO ~ Native, data = single_survey_data, 
     ylim=c(0,1), 
     pch=16, 
     col=adjustcolor(4,alpha.f = 0.3),
     cex=1.5, 
     main="Binary data example")

lines(BOBO_predicted$Native, 
      BOBO_predicted$p, 
      col="red", lwd=2)

# What do we see?
  # a really flat line
# we only did one survey at each site, what is a reason why we don't see a strong relationship that we expect of a prairie obligate species?
  # imperfect detection: species may be there, but we don't see it every time
  # some of the 0s in our data could be false


# Major assumption of this analysis has been that if bobolinks are present we would have detected them on our single site survey. ----
  # Is this a realistic assumption?
    # NO, imperfect detection

# Fortunately, the hypothetical researcher in this scenario did not think so either
# They actually conducted three visits to each of our 100 sites during the bobolink nesting season.
  # They also recorded wind speed during each site visit (which could make it difficult to hear bobolinks at a site)

# This study design allows us to model the probability of bobolinks being present as a function of native prairie patch size while also accounting for imperfect detection.
# We will do this using the single-season occupancy model first formulated by Makenzie et al. (2002).

# There are many ways to fit single-species single-season occupancy models which are the focus of today's lab.
# Arguably the simplest approach is to use the unmarked package in R. ----

### week 6 lab 2024-10-01 ----

library(unmarked)

?unmarked

# The function for fitting our model is the occu() function
#   Single species, single season model
#     (assume assumption closure is true or violated competely at random)
?occu # fit occupancy model as described by MacKenzie et al. 2002
      #   using the unmarked packed in R 

# The two primary arguments
#   formula: y ~ x
#   data: site and survey level covariates



# We will first discuss the data argument for occu()

# unmarked functions need the data formatted into unmarkedFrame() object
#   in this case unmarkedFrameOccu()
# The core of this object is the encounter history
#   the matrix (or data frame) of zeros and ones representing our encounters of our study species at each survey and at each site

# I have already created an unmarkedFrameOccu() object containing our data
# This has been saved as an RDS file using the saveRDS() function.
# RDS files are very helpful because they can consist of any object
#   (e.g., vectors, data frames, lists, SpatRasters, SpatVectors, unmarkedFrameOccu, etc.)
#   and they do some file compression (which is really nice when you save multi-layered SpatRasters).

# read in unmarkedFrameOccu from RDS file
occu_data <- readRDS("lab/week_5/Example_occupancy_data.rds")

class(occu_data) # confirming that it is an unmarkedFrameOccu
summary(occu_data)

# We have data from:
  # 100 sites
  # 3 visits per site
  # 50 sites with at least one detection
# This right here shows us these three site visits have detected bobolinks at twice as many sites as our single survey.

table(apply(occu_data@y,1,max)) # BOBO detected for at least one survey
mean(apply(occu_data@y,1,max))  # proportion of sites where sp. was det. at least once
                                #   known as naive occupancy estimate
                                #   does not account for imperfect detection

table(single_survey_data$BOBO)  # we can see that multiple site visits gives us very
                                # different results than a single visit to each site


# Within unmarkedFrameOccu() object we also have our covariate called
# Native (size of native prairie patch) and a covariate called Wind (wind
# speed). You will see that these are broken down to site-level and
# observation-level covariates. Site-level covariates are those whose value 
# is constant for each site across the length of the study while 
# observation-level covariates are those whose value changes for each site-visit.


# site covariates
occu_data@siteCovs
# survey covariates
occu_data@obsCovs
    # in unmarkedFrame, they are set up in a single column, b/c the package knows which is associated with which site visit. But when you read them in, they will be set um in a data frame set up just like the data frame for your encounter histories!!
# encounter history
occu_data@y

# Notice the number of sites where bobolinks were detected during only one or two surveys. #   Very unlikely that we will detect the species at an occupied site with few surveys



# Our goal is to model probability of bobolink being present ~ native prairie patch size
# so lets do that first while assuming a constant detection probability ----

# This is equivalent to fitting an intercept-only or mean (or "dot") model for detection probability. The model will estimate a single detection probability and assume that single probability holds true for every survey at a site where bobolink are present. 

p_dot <- occu(~1 ~Native, data = occu_data) # we have (~ detection ~ occupancy) 
                                            # b/c there are two glms being fit 
                                            # inside this model
# ~1: p = intercept + slope *X, assumes X=0, so p = intercept (detection)
# ~Native: ψ = intercept + slope *Native (occupancy)
summary(p_dot)


# What do we see here? Strong positive association with BOBO presence and native prairie
    # Occupancy (logit-scale):
    #             Estimate    SE    z P(>|z|)
    # (Intercept)    0.723 0.472 1.53 0.12584
    # Native         1.273 0.455 2.80 0.00518
    # 
    # Detection (logit-scale):
    #   Estimate    SE     z P(>|z|)
    #     -0.351 0.209 -1.68   0.093


# What is our detection probability? How can we calculate it?
#   Need to take the plogis() or with unmarked can use backTransform()
plogis(-0.351)              # 0.4131399
backTransform(p_dot, 'det') # 0.413
  # probability of observing BOBO at any site is 41.3%
  # but with p = 0.093, not significantly different than 50% (0 on logit)

# What is our occupancy probability? 
  # backTransform() doesn't work because only works with dot model (~1)
    # Only would work for occupancy probability if...
    # p_dot <- occu(~1 ~1, data = occu_data)
    # backTransform(p_dot, 'state') # 0.624
# So better to just use plogis
plogis(1.273) # 0.7812559


# predicting BOBO occupancy ~ Native
BOBO_predicted <- predict(p_dot, 
                          type = 'state', 
                          newdata = BOBO_predicted, 
                          appendData=TRUE)
head(BOBO_predicted)

plot(Predicted~Native, BOBO_predicted,ylim=c(0,1),type="l",lwd=3,
     ylab="Probability of Occupancy",xlab="Native Prairie")
lines(BOBO_predicted$Native,BOBO_predicted$lower,lty=3) # lower 95% CI
lines(BOBO_predicted$Native,BOBO_predicted$upper,lty=3) # upper 95% CI

# What does this graph say predicted BOBO occu at site with average Native?
  #about 60%

# How does this compare to the estimated probability occurrence from our single survey?
lines(BOBO_predicted$Native,BOBO_predicted$p,lwd = 3, col = "red")

  # Very different!!



# Now remember one of the assumptions of occupancy models is that we have no unmodeled heterogeneity in detection probability.
# But Illinois is very windy and relatively high winds may make it difficult to detect bobolinks during point counts. 
# That is probably why these researchers recorded wind speed during each survey! :)

# Lets fit another occupancy model
  # this time modeling detection probability as a function of wind speed ----
p_wind <- occu(~Wind ~Native, data = occu_data)
summary(p_wind)

# What do we see here?
  # wind has an effect on detection probability!
plogis(-0.441) # detection intercept, detect prob at average wind speed is 15%

# how can we calculate detection probability at really high and really low wind speed?
summary(occu_data@obsCovs)
#    Min.   1st Qu.   Median     Mean  3rd Qu.     Max.
# -1.7933   -0.9016   0.1122   0.0000   0.8684   1.7213 
plogis(-0.441 + (-1.671 * 1.7213)) # det prob at high wind, 0.3%
plogis(-0.441 + (-1.671 * -1.7933)) # det prob at high wind, 92.8%

# if nature conservation can only afford to survey sites once per year, we can inform them that they should only do their single survey on a day with as little wind as possible to optimize detection rate



# We can use a metric called AIC to compare these two models. ----
# Lower AIC is better so which of the two models has more empirical support?
p_dot@AIC  # 321.0371
p_wind@AIC # 253.9946 better model fit



# plot the predicted values from all our models
BOBO_predicted_wind <- predict(p_wind, 
                          type = 'state', 
                          newdata = BOBO_predicted[,c("Native","p")], 
                          appendData=TRUE)
head(BOBO_predicted_wind)

plot(Predicted~Native, BOBO_predicted_wind,
     ylim=c(0,1), # limit of y-axis
     type="l",    # solid line
     lwd=3,       # line width
     ylab="Probability of Occupancy",xlab="Native Prairie",
     col="blue")
lines(BOBO_predicted_wind$Native,BOBO_predicted_wind$lower,lwd=3,lty=3,col="blue")
lines(BOBO_predicted_wind$Native,BOBO_predicted_wind$upper,lwd=3,lty=3,col="blue")

lines(BOBO_predicted$Native,BOBO_predicted$Predicted,lwd=3,col="green")
lines(BOBO_predicted$Native,BOBO_predicted$lower,lwd=3,lty=3,col="green")
lines(BOBO_predicted$Native,BOBO_predicted$upper,lwd=3,lty=3,col="green")

lines(BOBO_predicted$Native,BOBO_predicted$p,lwd = 3, col = "red")

legend("topleft",
       legend = c("Binomial GLM","p(.)","p(Wind)"),
       lwd = 3,col=c("red","green","blue"))

# How does predicted occupancy change once we account for wind speed?
# accounting for high heterogeneity in detection probability does not matter much in this system. But it is still a bias and it does matter to include this in your study design. If we can do a better job of describing variation in our data, we should. Can be seen in the better fit model from the AIC comparisons.