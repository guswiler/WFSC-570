# Olivia Guswiler
# Lab Assignment 3
# 2024-10-08

library(unmarked)
library(AICcmodavg)
library(ggplot2)



#### Task I: Fit a model assuming constant occupancy and detection ####

# read in csv files
encounter <- read.csv("lab_assignments/lab_assignment_3/Bobcat_encounter_history.csv")
landcover <- read.csv("lab_assignments/lab_assignment_3/Bobcat_site_covariates.csv")
doy <- read.csv("lab_assignments/lab_assignment_3/Bobcat_survey_level_covariate_DOY.csv")

# take initial look at the data
head(encounter)
summary(encounter)

head(landcover)
summary(landcover)

head(doy)
summary(doy)

# standardize continuous covariates (doy and landcover) on the z-scale
doy <- as.matrix(doy)               # convert to matrix
doy[,] <- (doy - mean(doy))/sd(doy) # z-score standardize
summary(doy)

landcover <- as.matrix(landcover)
landcover[,] <- (landcover - mean(landcover))/sd(landcover)
summary(landcover)


# combine bobcat encounter history and covariates into unmarkedFrameOccu object
?unmarkedFrameOccu

bobcat_umf <- unmarkedFrameOccu(bobcat,
                                siteCovs = data.frame(Forest=landcover[,1], # vary at the
                                                      Grass=landcover[,2]), #site level
                                obsCovs = list(doy = doy))  # vary within sites
summary(bobcat_umf)


# fit a dot model (assume constant occupancy and detection, aka intercept-only model)
p_dot <- occu(~ 1 ~ Forest, bobcat_umf)
summary(p_dot)

# proportion of sites where bobcats were detected
40/102

# probability of use


# probability of detection
backTransform(p_dot, 'det')




#### Task II: Determine how forest and grassland land cover influence the probability that a bobcat used a site ####

# Fit the following four models with their specified terms for occupancy:
#   
# •	Constant occupancy
??????
  
  
# •	Occupancy varies as a function of forest cover
p_forest <- occu(~ 1 + doy ~ Forest, bobcat_umf)
summary(p_forest)

# •	Occupancy varies as a function of grassland cover
p_grass <- occu(~ 1 + doy ~ Grass, bobcat_umf)
summary(p_grass)

# •	Occupancy varies as a function of forest and grassland cover
p_cover <- occu(~ 1 + doy ~ Forest + Grass, bobcat_umf)
summary(p_cover)


# compare fit of models
AICc(p_dot) # still need to fit this one to determine best fit
AICc(p_forest) # 118.88
AICc(p_grass)  # 150.62
AICc(p_cover)  # 120.97**

# occupancy coefficient estimates of covariates
summary(p_forest)
plogis(0.151)     # probability of use with forest only (0.538, p = 0.602)

summary(p_grass)
plogis(-0.364)    # probability of use with grass only (0.410, p = 0.083)

summary(p_cover)
plogis(0.1646)    # probability of use with forest + grass (0.541, p = 0.573)


#### Task III: Plot the predicted relationship between probability of use and grassland and forest land cover ####

# create "new data" for predictions
forest_new <- data.frame(Forest = seq(min(bobcat_umf@siteCovs$Forest),
                                    max(bobcat_umf@siteCovs$Forest),
                                    length.out = 30))

grass_new <- data.frame(Grass = seq(min(bobcat_umf@siteCovs$Grass),
                                    max(bobcat_umf@siteCovs$Grass),
                                    length.out = 30))


# calculating predicted values for forest cover
bobcat_Forest_pred <- predict(p_forest, 
                              type = 'state', 
                              newdata = forest_new, 
                              appendData=TRUE)

# define data by land cover type
bobcat_Forest_pred$Feature <- "Forest"
# rename column as covariate
colnames(bobcat_Forest_pred)[5] <- "Covariate"

# calculating predicted values for grass cover
bobcat_Grass_pred <- predict(p_grass, 
                              type = 'state', 
                              newdata = grass_new, 
                              appendData=TRUE)

# define data by land cover type
bobcat_Grass_pred$Feature <- "Grass"
# rename column as covariate
colnames(bobcat_Grass_pred)[5] <- "Covariate"


# combine the data frames and check
bobcat_pred <- rbind(bobcat_Forest_pred[,c(1,3,4,5,6)],
                     bobcat_Grass_pred[,c(1,3,4,5,6)])
head(bobcat_pred)
tail(bobcat_pred)


# plot the predicted probability of use as a function of forest and grass cover
bobcat_plot <- ggplot(bobcat_pred,
                      aes(x=Covariate))+
  geom_ribbon(aes(ymin=lower,ymax=upper,
                  fill=Feature),alpha=0.25)+
  geom_line(aes(y=Predicted,colour=Feature),linewidth=2)+
  scale_y_continuous("Probability of Use (psi)",limits=c(0,1))+
  xlab("Covariate")+
  scale_fill_manual(values=c("green","darkgoldenrod1"))+
  scale_colour_manual(values=c("darkgreen","darkgoldenrod4"))+
  theme_bw()

bobcat_plot +
  labs(title = "Predicted Probability of Bobcat Site Use \nas a Function of Forest and Grass Cover")+
  theme(plot.title = element_text(hjust = 0.5))



#### Bonus ####
doy_new <- data.frame(DOY = seq(min(bobcat_umf@obsCovs$doy),
                                    max(bobcat_umf@obsCovs$doy),
                                    length.out = 30))


# calculating predicted values for forest cover
doy_pred <- predict(p_doy,           ### need to model doy
                    type = 'state', 
                    newdata = doy_new, 
                    appendData=TRUE)

# define data by land cover type
doy_pred$Feature <- "DOY"
# rename column as covariate
colnames(doy_pred)[5] <- "Covariate"

# plot
doy_plot <- ggplot(doy_pred,
                      aes(x=Covariate))+
  geom_ribbon(aes(ymin=lower,ymax=upper,
                  fill=Feature),alpha=0.25)+
  geom_line(aes(y=Predicted,colour=Feature),linewidth=2)+
  scale_y_continuous("Probability of Detection",limits=c(0,1))+
  xlab("Day of Year")+
  scale_fill_manual(values=c("green","darkgoldenrod1"))+
  scale_colour_manual(values=c("darkgreen","darkgoldenrod4"))+
  theme_bw()

doy_plot +
  labs(title = "Predicted Probability of Bobcat Detection \nas a Function of Day-of-Year")+
  theme(plot.title = element_text(hjust = 0.5))