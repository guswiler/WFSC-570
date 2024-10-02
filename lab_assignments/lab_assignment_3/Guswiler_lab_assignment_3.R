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
doy <- as.matrix(doy)
doy[,] <- (doy - mean(doy))/sd(doy)
summary(doy)

landcover <- as.matrix(landcover)
landcover[,] <- (landcover - mean(landcover))/sd(landcover)
summary(landcover)


# combine bobcat encounter history and covariates into unmarkedFrameOccu object
?unmarkedFrameOccu

bobcat_umf <- unmarkedFrameOccu(encounter,
                                siteCovs = data.frame(Forest=landcover[,1], # vary at the
                                                      Grass=landcover[,2]), #site level
                                obsCovs = list(doy = doy))  # vary within sites
summary(bobcat_umf)



# fit a dot model (assume constant occupancy and detection, aka intercept-only model)
p_dot <- occu(~1 ~1, bobcat_umf)
summary(p_dot)


# Proportion of sites where bobcats were detected
mean(apply(bobcat_umf@y,1,max))
  # naive use estimate, can also calculate using backTransform()
backTransform(p_dot, type='state')
  # Estimate     SE LinComb (Intercept)
  #    0.392 0.0483  -0.438           1

# calculate 95% CIs for use probability estimate
confint(p_dot, type='state')
  #               0.025       0.975
  # psi(Int) -0.8357415 -0.04076848
plogis(-0.8357415)  # 0.3024324
plogis(-0.04076848) # 0.4898093


# Probability of detection
backTransform(p_dot, 'det')
  # Estimate      SE LinComb (Intercept)
  #     0.99 0.00704     4.6           1
  # 
  # Transformation: logistic

# calculating 95% CIs for detection probability
confint(p_dot, type='det')
  #               0.025       0.975
  # psi(Int)   3.202234    5.988006
plogis(3.202234) # 0.9609183
plogis(5.988006) # 0.9974976



#### Task II: Determine how forest and grassland land cover influence the probability that a bobcat used a site ####

# Model assuming constant occupancy
p_doy <- occu(~ doy ~ 1, bobcat_umf)
summary(p_doy)

backTransform(p_doy, 'state')
  # Estimate     SE LinComb (Intercept)
  #    0.392 0.0483  -0.438           1
  # 
  # Transformation: logistic 

confint(p_doy, type='state')
  #                0.025         0.975
  # psi(Int)  -0.8357553   -0.04078117
plogis(-0.8357553)  # 0.3024295
plogis(-0.04078117) # 0.4898061


# Model assuming occupancy varies as a function of forest cover
p_forest <- occu(~ doy ~ Forest, bobcat_umf)
summary(p_forest)

plogis(0.151) # Int estimate
plogis(0.290) # Int SE
#  Estimate        SE
# 0.5376784 0.5719961
plogis(1.829) # Forest estimate
plogis(0.404) # Forest SE
  #  Estimate        SE
  # 0.8616426 0.5996483

confint(p_forest, type='state')
  #                  0.025     0.975
  # psi(Forest)  1.0361850 2.6215026
plogis(1.0361850) # 0.7381132
plogis(2.6215026) # 0.9322327


# Model assuming occupancy varies as a function of grassland cover
p_grass <- occu(~ doy ~ Grass, bobcat_umf)
summary(p_grass)

plogis(-0.279) # Grass estimate
plogis(0.205)  # Forest SE
  # Estimate        SE
  # 0.430699 0.5510713

confint(p_grass, type='state')
  #                 0.025      0.975
  # psi(Grass) -0.6810336 0.12376685
plogis(-0.6810336) # 0.3360307
plogis(0.12376685) # 0.5309023


# Model assuming occupancy varies as a function of forest and grassland cover
p_cover <- occu(~ doy ~ Forest + Grass, bobcat_umf)
summary(p_cover)

plogis(1.8045)  # Int estimate
plogis(0.409)   # Int SE
plogis(-0.0865) # grass estimate
plogis(0.250)   # grass SE
  # Covariate  Estimate        SE
  #    Forest 0.8586958 0.6008481
  #     Grass 0.4783885 0.5621765

confint(p_cover, type='state')
  #                  0.025     0.975
  # psi(Forest)  1.0021568 2.6068076
  # psi(Grass)  -0.5762939 0.4032359
plogis(1.0021568)  # F lower: 0.7314824
plogis(2.6068076)  # F upper: 0.9312984
plogis(-0.5762939) # G lower: 0.3597858
plogis(0.4032359)  # G upper: 0.5994649


# compare fit of models
AICc(p_dot)    # 163.1413
AICc(p_doy)    # 150.3759
AICc(p_forest) # 118.8805 winner!
AICc(p_grass)  # 150.6232
AICc(p_cover)  # 120.9737




#### Task III: Plot the predicted relationship between probability of use and grassland and forest land cover ####

# create "new data" for predictions
forest_new <- data.frame(Forest = seq(min(bobcat_umf@siteCovs$Forest),
                                    max(bobcat_umf@siteCovs$Forest),
                                    length.out = 102))

grass_new <- data.frame(Grass = seq(min(bobcat_umf@siteCovs$Grass),
                                    max(bobcat_umf@siteCovs$Grass),
                                    length.out = 102))


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

bobcat_plot <- bobcat_plot +
  labs(title = "Predicted Probability of Bobcat Site Use \nas a Function of Forest and Grass Cover")+
  theme(plot.title = element_text(hjust = 0.5))

bobcat_plot

# # save bobcat_plot as .png
# ggsave("lab_assignments/lab_assignment_3/outputs/bobcat_occupancy_landcover.png",
#        bobcat_plot,
#        width = 7,height = 5)




#### Bonus ####
doy_new <- data.frame(doy = seq(min(bobcat_umf@obsCovs$doy),    # covariates in new data must
                                                                #   match name in model to predict
                                    max(bobcat_umf@obsCovs$doy),
                                    length.out = 102))
head(doy_new)

# calculating predicted values for forest cover
doy_pred <- predict(p_doy,             
                    type = 'det', 
                    newdata = doy_new, 
                    appendData=TRUE)

# define data as DOY
doy_pred$Feature <- "DOY"
# rename new doy data as covariate
colnames(doy_pred)[5] <- "Covariate"

# plot
doy_plot <- ggplot(doy_pred,
                      aes(x=Covariate))+
  geom_ribbon(aes(ymin=lower,ymax=upper,
                  fill=Feature),alpha=0.25)+
  geom_line(aes(y=Predicted,colour=Feature),linewidth=2)+
  scale_y_continuous("Probability of Detection",limits=c(0,1))+
  xlab("Day of Year")+
  scale_fill_manual(values="blue")+
  scale_colour_manual(values="darkblue")+
  theme_bw()

doy_plot <- doy_plot +
  labs(title = "Predicted Probability of Bobcat Detection \nas a Function of Day-of-Year")+
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none")

doy_plot

# # save doy_plot as .png
# ggsave("lab_assignments/lab_assignment_3/outputs/bobcat_detection_doy.png",
#        doy_plot,
#        width = 7,height = 5)