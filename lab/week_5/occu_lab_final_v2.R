rm(list=ls()) # clean workspace.  Caution!!!!

# Lab 3: Occupancy modeling for wildlife habitat analyses

# Today, we will explore the use of occupancy models for wildlife habitat 
# analyses. Our goal is to not focus on occupancy models per se but rather
# to illustrate their use for quantifying wildlife-habitat relationships
# when working with presence-absence or use-non-use data in a manner
# that allows us to account for our inability to perfectly detect our study
# species. That is, to account for our uncertainty in whether our zeros
# are actually true zeros. 

# Before we get into occupancy modeling per se lets step back and briefly revist
# the binomial generalized linear model (GLM) that we use for presence-absence
# or used-unused data. We have already covered the components of the binomial
# GLM so lets use it to fit some presence-absence data. 

# First, lets set our working directory (and make sure it exists!)

base_dir <- "C:/Users/jbauder/Box/Bauder_Coop_Lab/UA_Teaching/WFSC 570 Habitat Analysis 3cr/Labs/occupancy/"
dir.exists(base_dir)
setwd(base_dir)

# The data for these lab are inspired by prairie-associated songbirds from the
# Midwest (e.g., Illinois). Across the Midwest USA, native prairie communities
# often exist as islands (small, isolated patches) within a matrix of 
# row-crop agriculture (e.g., corn, soybeans). Many of these species are
# more likely to occur within patches of native prairie habitat that are larger.
# Our data represent 100 sites located on patches of native prairie habitat
# which vary in size. Our first example data set represent a single point-count
# survey conducted at each site to detect Bobolinks. Lets read in our data
# as CSV text file.

single_survey_data <- read.csv("Bobolink_single_survey.csv")

# As always, lets view our data before doing anything else.

head(single_survey_data)

# We see a column denoting our detections of bobolinks (BOBO) and a column
# denoting the value of native prairie patch size (Native). Native has been
# rescaled to fit on the z-scale. This just means that it has a mean = 0 and
# as SD = 1. z-score standardizing continuous covariates is a good practice
# in many situations because it aids in model convergence (not an issue here)
# and puts continuous covariates on the same scale so you can more easily
# compare their effect sizes.

summary(single_survey_data)

# At how many sites did we detect bobolinks?

mean(single_survey_data$BOBO)
table(single_survey_data$BOBO)

# Now lets fit a binomial GLM to these data and see if the size of native 
# prairie patch influences the presence of bobolinks.

BOBO_mod <- glm(BOBO ~ Native, data = single_survey_data, 
                family = binomial)

summary(BOBO_mod)

# What do we see here?

# Now lets plot the relationship between probability of bobolinks being present
# and the size of native prairie patch. Remember that we can use the 
# predict function here. We need to provide predict() with covariate values
# (with X's) at which to predict probability of bobolinks being present. 
# So lets create a vector of values that range from the minimum to the 
# maximum value of our actual Native covariate.

Native_pred <- seq(min(single_survey_data$Native),
                   max(single_survey_data$Native),
                   length.out = 30)
Native_pred

# Now lets add these new values to a new data frame which will be used to
# predict the probability of bobolinks being present given each of these
# values of Native.

BOBO_predicted <- data.frame(Native = Native_pred) 

BOBO_predicted$p <- predict(BOBO_mod,     # Our glm object
                            newdata=BOBO_predicted,  # The new data for which
                                                     # we would like the expected
                                                     # values.
                            type="response")         # The scale of those expected
                                                     # values

head(BOBO_predicted)
tail(BOBO_predicted)

# Now lets our actual bobolink detections as a function of native prairie patch
# size and add a line showing the predicted (or expected) probability of 
# bobolink being present as a function of native prairie patch size.

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

# Now a major assumption of this analysis has been that if bobolinks are present
# we would have detected them on our single site survey.
# Is this a realistic assumption?

# Fortunately, the hypothetical researcher in this scenario did not think so
# either. They actually conducted three visits to each of our 100 sites during
# the bobolink nesting season. They also recorded wind speed during each site 
# visit. This study design allows us to model the probability of bobolinks
# being present as a function of native prairie patch size while also
# accounting for imperfect detection. We will do this using the
# single-season occupancy model first formulated by Makenzie et al. (2002).

# There are many ways to fit single-species single-season occupancy models
# which are the focus of today's lab. Arguably the simplest approach is to
# use the unmarked package in R. 

library(unmarked)

?unmarked

# The function for fitting this model is the occu() function. Lets take a look
# at it.

?occu

# The two primary arguments we need to concern ourselves with are the formula
# and the data arguments. We will come back to the formula argument later.

# unmarked functions need the data formatted into specific objects, in this
# case an unmarkedFrameOccu() object. The core of this object is the 
# encounter history, the matrix (or data frame) of zeros and ones representing
# our encounters of our study species at each survey and at each site.
# I have already created an unmarkedFrameOccu() object containing our data
# This has been saved as an RDS file using the saveRDS() function. RDS files
# are very helpful because they can consist of any object (e.g., vectors,
# data frames, lists, SpatRasters, SpatVectors, unmarkedFrameOccu, etc.)
# and they do some file compression (which is really nice when you save
# multi-layered SpatRasters). Lets read it in and look at it.

occu_data <- readRDS("Example_occupancy_data.rds")

class(occu_data)

summary(occu_data)

# You will see that we have data from 100 sites, three visits per site, and 50
# sites with at least one detection. This right here shows us these three
# site visits have detected bobolinks at twice as many sites as our single
# survey.

table(apply(occu_data@y,1,max))
table(single_survey_data$BOBO)

# Within our unmarkedFrameOccu() object we also have our covariate called
# Native (size of native prairie patch) and a covariate called Wind (wind
# speed). You will see that these are broken down to site-level and
# observation-level covariates. Site-level covariates are those whose value 
# is constant for each site across the length of the study while 
# observation-level covariates are those whose value changes for each site-visit.

# Lets look at our encounter history

occu_data@y

# Notice the number of sites where bobolinks were detected during only one
# or two surveys. 

# Our goal is to model probability of bobolink being present at a site as a
# function of native prairie patch size so lets do that first while assuming
# a constant detection probability. This is equivalent to fitting an
# intercept-only or mean (or "dot") model for detection probability. The model
# will estimate a single detection probability and assume that single probability
# holds true for every survey at a site where bobolink are present. 

p_dot <- occu(~1 ~Native, data = occu_data)

summary(p_dot)

# What do we see here?

# What is our detection probability? How can we calculate it?

backTransform(p_dot, 'det')

# What is our occupancy probability? 

BOBO_predicted <- predict(p_dot, 
                          type = 'state', 
                          newdata = BOBO_predicted, 
                          appendData=TRUE)
head(BOBO_predicted)

plot(Predicted~Native, BOBO_predicted,ylim=c(0,1),type="l",lwd=3,
     ylab="Probability of Occupancy",xlab="Native Prairie")
lines(BOBO_predicted$Native,BOBO_predicted$lower,lty=3)
lines(BOBO_predicted$Native,BOBO_predicted$upper,lty=3)

# How does this compare to the estimated probability occurrence from our 
# single survey?

lines(BOBO_predicted$Native,BOBO_predicted$p,lwd = 3, col = "red")

# Now remember one of the assumptions of occupancy models is that we have no
# unmodeled heterogeneity in detection probability. But Illinois is very windy
# and relatively high winds may make it difficult to detect bobolinks during
# point counts. That is probably why these researchers recorded wind speed
# during each survey! :)

# Lets fit another occupancy model this time modeling detection probability
# as a function of wind speed.

p_wind <- occu(~Wind ~Native, data = occu_data)

summary(p_wind)

# What do we see here?

# We can use a metric called AIC to compare these two models. Lower AIC is better
# so which of the two models has more empirical support?

p_dot@AIC
p_wind@AIC

# How does predicted ocupancy change once we account for wind speed?

BOBO_predicted_wind <- predict(p_wind, 
                          type = 'state', 
                          newdata = BOBO_predicted[,c("Native","p")], 
                          appendData=TRUE)
head(BOBO_predicted_wind)

plot(Predicted~Native, BOBO_predicted_wind,ylim=c(0,1),type="l",lwd=3,
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
