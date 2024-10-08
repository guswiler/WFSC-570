rm(list=ls()) # clean workspace.  Caution!!!!

# Lab 4: Scale in wildlife-habitat analyses

# In today's lab we will illustrate one way in which we can account for the 
# role of scale in wildlife-habitat analyses. As we have discussed in lecture,
# scale is a very important concept in ecology, in part because ecological
# patterns and processes are dependent upon the scale at which we observe them.
# A key focus in ecology is therefore to determine the appropriate scale
# at which to study our patterns and processes. Multi-scale analyses measure
# ecological patterns and processes at different spatial scales in an effort
# to determine the appropriate scale in which to study the phenomena in 
# question (e.g., habitat selection). This "most appropriate" scale(s)
# is often called the characteristic scale or scale of effect.

# But how does one determine the scale of effect? Within a wildlife-habitat
# analysis framework, one simply measures the environmental features of interest
# at used, available, and/or unused locations across multiple spatial scales,
# fits their statistical model using environmental covariates at different
# spatial scales, and then uses some objective criterion (e.g., AIC, % variation
# explained, predictive performance) to select the optimal scale(s). While this
# may seem simply there are many nuances that often make this exercise very
# complicated.

# In this lab, we will use what McGarigal et al. (2016) call a pseudo-optimization
# approach to identifying scales of effect and fitting multi-scale
# wildlife-habitat models. The optimization comes from the process of empirically
# comparing modeling results at different scales and using empirical means
# (e.g., AIC, % variation explained, predictive performance) to select the
# best scale, which is considered the scale of effect. The pseudo comes from
# two components of the analysis. First, only a limited (but sometimes large)
# number of scales are compared. In this sense, the optimization is already
# constrained by the scales chosen to be included by the investigator.
# Second, the best scale is chosen for each environmental feature independently
# of the other features. We will talk later about why this might be an issue.
# Nevertheless, pseudo-optimization is a widespread and acceptable approach
# for identifying scales of effect and for creating multi-scale wildlife-
# habitat models.

# Our data today are inspired by Canyon Treefrogs in southeast Arizona.
# Canyon Treefrogs are widespread across Arizona and occur in a variety of
# landscapes and vegetation communities. Our data represent presence-absence
# surveys at 171 sites in southeast Arizona. We will make the (unrealistic)
# assumption that our hypothetical observers have the ability to always
# detect Canyon Treefrogs at sites where they occur, so we will treat our data
# as binary presence-absence data (although in reality we would want repeated
# site visits so as to estimate and account for detection rate).

# We are interested in understanding how topography, precipitation, and
# land cover affect the distribution of Canyon Treefrogs across our sites
# and the scales at which these effects occur. To address these objectives
# we have buffered each site with a series of circular buffers whose radii
# range from 100 to 1200 m. We will assume that Canyon Treefrogs respond
# to landscape features somewhere within this range of scales. We then 
# measure the average or proportion of our landscape features within 
# each buffer. Lets read in our data and see what data we have.

base_dir <- paste0("C:/Users/jbauder/Box/Bauder_Coop_Lab/UA_Teaching/",
                   "WFSC 570 Habitat Analysis 3cr/Labs/scale/")
setwd(base_dir)

HYAR_data <- read.csv("HYAR_data_scale_lab.csv")
head(HYAR_data)

# HYAR: zeros and ones denoting the absence or presence, respectively, of 
#       Canyon Treefrogs (Hyla arenicolor)
# x and y: the UTM coordinates of each site in NAD83 UTM Zone 12
# Slope_100 through Slope_1200: the average percent slope within each buffer
#       derived from a 30-m digital elevation model (DEM)
# Water_100 through Water_1200: the proportion of each buffer that is NLCD
#       land cover classes Open Water (11), Woody Wetlands (90), or
#       Emergent Herbaceous Wetlands (95)
# PREC_100 through PREC_1200: the average precipitation within each buffer
#       derived from PRISM climate data

# Lets illustrate the pseudo-optimization approach for one covariate, Slope.
# To do this, we will fit 12 binomial generalized linear models (GLM), one for
# each of our buffer sizes (i.e., scales). We will use AIC to compare these
# models and identify the "best" scale which we will consider the scale
# of effect for slope.

# For a procedure like this I like to create an empty data frame to store
# the results of my analyses. We know that this storage data frame should
# have at least three columns, one denoting our covariate (Slope), one
# denoting the scale (100-1200), and one containing the AIC value of the
# binomial GLM for that scale.

slope_scales <- data.frame(Covariate = "Slope",
                           Scale = seq(100,1200,by = 100),
                           AIC = NA)

# Now we can loop through each row of this data frame, pull out the 
# column from HYAR_data containing slope at the appropriate scale,
# fit a binomial GLM, and then store the AIC in our slope_scales data frame.

for(i in 1:nrow(slope_scales)){
  
  cov_i <- paste0(slope_scales$Covariate[i],"_",slope_scales$Scale[i])
  data_i <- HYAR_data[,c("HYAR",cov_i)]
  model_i <- glm(data_i[,1] ~ data_i[,2], data = data_i, family = binomial)
  slope_scales$AIC[i] <- AIC(model_i)
}

slope_scales

# Now lets plot AIC as a function of scale

plot(AIC ~ Scale, slope_scales,
     type = "b",
     xlab = "Scale (buffer radius in meters)",
     main = "Scale of effect for Slope")

# What is our scale of effect for slope?

# We can also pull out the row from slope_scales that includes our scale
# of effect

slope_scales[which(slope_scales$AIC==min(slope_scales$AIC)),]

# What is the effect of slope at the scale of effect?

slope_SOE <- glm(HYAR ~ Slope_700, data = HYAR_data, family = binomial)
summary(slope_SOE)

# What is the expected probability of Canyon Treefrogs occurring at a site
# with an average slope?

# Now that we have identified the scale of effect for slope lets do the same
# for water and precipitation

water_scales <- data.frame(Covariate = "Water",
                           Scale = seq(100,1200,by = 100),
                           AIC = NA)

for(i in 1:nrow(water_scales)){
  
  cov_i <- paste0(water_scales$Covariate[i],"_",water_scales$Scale[i])
  data_i <- HYAR_data[,c("HYAR",cov_i)]
  model_i <- glm(data_i[,1] ~ data_i[,2], data = data_i, family = binomial)
  water_scales$AIC[i] <- AIC(model_i)
}

water_scales

plot(AIC ~ Scale, water_scales,
     type = "b",
     xlab = "Scale (buffer radius in meters)",
     main = "Scale of effect for Water land cover")


PREC_scales <- data.frame(Covariate = "PREC",
                           Scale = seq(100,1200,by = 100),
                           AIC = NA)

for(i in 1:nrow(PREC_scales)){
  
  cov_i <- paste0(PREC_scales$Covariate[i],"_",PREC_scales$Scale[i])
  data_i <- HYAR_data[,c("HYAR",cov_i)]
  model_i <- glm(data_i[,1] ~ data_i[,2], data = data_i, family = binomial)
  PREC_scales$AIC[i] <- AIC(model_i)
}

PREC_scales

plot(AIC ~ Scale, PREC_scales,
     type = "b",
     xlab = "Scale (buffer radius in meters)",
     main = "Scale of effect for Precipitation")

# Now that we have identified our scales of effect of slope, mesic land covers,
# and precipitation we could fit a pseudo-optimized multi-scale model with these
# three covariates. But we are interested in a couple more topographic features.
# First, Canyon Treefrogs may show an association with elevation as lower sites
# may be in more xeric landscape contexts and therefore be less likely to
# support Canyon Treefrogs. So we should probably measure the elevation at
# and around each site. Secondly, Canyon Treefrogs often live up to their 
# names in that they occur in rocky ravine-like situations. It is therefore
# conceivable that Canyon Treefrogs may be strongly associated with 
# ravine- or valley like topography. Fortunately, there is a simple covariate
# that we can calculate to distinguish between valley and ridge-like 
# topography: Topographic Position Index (TPI). 

# TPI is simply the difference between the elevation at a single pixel and
# the average elevation within some surrounding area. Pixels that have a higher
# elevation than the average surrounding elevation are more likely to be on
# ridges while pixels that have lower elevation than the average surrounding 
# elevation are more likely to be in valleys. 

# To calculate TPI we will first need to extract the elevation at each site.
# Remember that we can do this using the extract() function in terra. So lets
# load in terra, read in our DEM raster, and extract the elevation at each
# of our sites.

library(terra)

DEM <- rast("Final_DEM_for_scale_lab.tif")
DEM
plot(DEM)

# Lets also plot our points
points(HYAR_data[,c("x","y")],
       pch = 21,
       bg = "lightgrey")

site_elev <- extract(DEM, 
                     HYAR_data[,c("x","y")])
summary(site_elev)
hist(site_elev$DEM)

# We can now add a new column to HYAR_data with this site elevation data

HYAR_data$site_elev <- site_elev$DEM

# But how do we measure the average elevation within our varying radii buffers
# around each site? There are two ways we can do this. The first is to transform
# our DEM raster into a raster where the pixel value is actually the average
# elevation of some neighborhood or buffer around each pixel. We can do this 
# by applying a "moving window" across our entire raster, that is, move a circular
# buffer over every pixel in our raster, calculate the average elevation within
# that buffer, and store that average value into the focal pixel. 
# terra's focal() function can do just that. focal() is actually a very 
# versatile function that can do much more than we will cover today.

# The focal function has three main arguments:
#       x: the raster on which we wish to apply our "moving window"
#       w: a window. This represents the buffer we with to use as our 
#           "moving window" and must be a square or rectangular matrix.
#           We will need to calculate this separately outside of the focal()
#           function.
#       fun: the function we wish to apply within our window. We will use "mean"

# Lets start with a 30-m (one pixel) radius circular moving window just as
# an illustration.

buffer <- focalMat(DEM, 
                   d = 30, 
                   type=c('circle'))
buffer

# Notice that we have a matrix of fractional values. Also notice how these values
# will sum to one. What does this mean? 

sum(buffer)

# We are going to use this matrix to calculate what is called a weighted average.
# Weighted averages are another very useful tool that we won't get into much
# today. But they work by multiplying your data values (elevation in this case)
# by the weights and then summing the products. Lets create a 3 x 3 matrix
# with some simple integer values.

test_elev <- matrix(c(1,2,3,4,5,6,7,8,9),ncol = 3, nrow = 3)

mean(test_elev)

(test_elev * buffer)

sum(test_elev * buffer)

# Our actual buffer that we will use as a moving window does the same thing,
# calculates the weighted average of elevation within the buffer, and because
# our weights are all equal this is the same as taking the arithmatic mean.

# Now lets create a new buffer with a 500-m radius buffer and apply that over
# our entire DEM.

buffer_500 <- focalMat(DEM, 
                   d = 500, 
                   type=c('circle'))

# Lets put our focal() function inside the system.time() function to see how
# long this will take

system.time(scale_DEM_500 <- focal(DEM, 
                                    w = buffer_500, 
                                    fun = "mean"))

# How does our "smoothed" elevation raster compare to our original DEM?

plot(scale_DEM_500)
plot(DEM)

# Once we have our "smoothed" raster, we can now extract values from this raster
# at each site and we will be extracting the average elevation within a 500-m
# circular radius buffer.

elev_500 <- extract(scale_DEM_500, 
                    HYAR_data[,c("x","y")])

summary(elev_500$focal_mean)
summary(HYAR_data$site_elev)


# Now the focal() function can take a good bit of time to run, especially for
# large rasters (rasters with many pixels) and for large buffer or window
# sizes. Just for reference, smoothing this DEM with a 1200-m radius buffer
# took over four minutes. Which is too long for our lab so we will go to 
# option #2 for extracting data at multiple spatial scales.

# But before we do, what are some advantageous of having rasters representing
# our landscape features at different spatial scales? 

# Option #2 is to use our buffer or window directly within terra's extract()
# function. Recall that extract can extract all the pixel values within a
# SpatVector polygon. So if we create a SpatVector polygon object representing
# 500-m circular buffers around each site, we will be able to extract the
# elevation values from each pixel within that buffer and then summarize them.
# extract() does have a fun= argument (function) where we can tell it to take
# the mean of our extract pixels.

# To implement this option we will first need to convert our site data into a
# SpatVector point object and then create our SpatVector polygon buffers.

site_data_SV <- vect(x = HYAR_data,
                     geom = c("x","y"),
                     crs = "epsg:26912")

site_buffers_500 <- buffer(site_data_SV,
                           width = 500)

elev_500_buffer <- extract(DEM,
                           site_buffers_500,
                           fun = "mean")

head(elev_500_buffer)

# Now lets put all this into a loop that will calculate the average elevation
# around each site for each of our buffer sizes. We will then add a new column
# to our HYAR_data data frame with average elevation within each of these
# buffer sizes.

buffer_sizes <- seq(100,1200,by = 100)

for(i in 1:length(buffer_sizes)){
  
  buff_i <- buffer_sizes[i]
  
  cat("Starting buffer size =",buff_i,"\n")
  
  site_buffers_i <- buffer(site_data_SV,
                             width = buff_i)
  
  elev_buffer_i <- extract(DEM,
                             site_buffers_i,
                             fun = "mean")
  
  HYAR_data$TMP <- elev_buffer_i$DEM
  colnames(HYAR_data)[which(colnames(HYAR_data)=="TMP")] <- paste0("Elev_",buff_i)
  
}

head(HYAR_data)

# We now have the data we need to find the scale of effect for elevation on
# Canyon Treefrog occurrence. But what about TPI? Well, we have the elevation
# of each site (site_elev) and now we have the average elevation in a variety
# of neighborhood sizes, so we have all the data we need.

all_TPI <- HYAR_data$site_elev - HYAR_data[,grep("Elev_",colnames(HYAR_data))]
colnames(all_TPI) <- paste0("TPI_",buffer_sizes)

HYAR_data_final <- cbind(HYAR_data,all_TPI)

head(HYAR_data_final)

# Now lets identify the scale of effect for both elevation and TPI

Elev_scales <- data.frame(Covariate = "Elev",
                          Scale = seq(100,1200,by = 100),
                          AIC = NA)

for(i in 1:nrow(Elev_scales)){
  
  cov_i <- paste0(Elev_scales$Covariate[i],"_",Elev_scales$Scale[i])
  data_i <- HYAR_data_final[,c("HYAR",cov_i)]
  model_i <- glm(data_i[,1] ~ data_i[,2], data = data_i, family = binomial)
  Elev_scales$AIC[i] <- AIC(model_i)
}

Elev_scales

plot(AIC ~ Scale, Elev_scales,
     type = "b",
     xlab = "Scale (buffer radius in meters)",
     main = "Scale of effect for Elevation")


TPI_scales <- data.frame(Covariate = "TPI",
                          Scale = seq(100,1200,by = 100),
                          AIC = NA)

for(i in 1:nrow(TPI_scales)){
  
  cov_i <- paste0(TPI_scales$Covariate[i],"_",TPI_scales$Scale[i])
  data_i <- HYAR_data_final[,c("HYAR",cov_i)]
  model_i <- glm(data_i[,1] ~ data_i[,2], data = data_i, family = binomial)
  TPI_scales$AIC[i] <- AIC(model_i)
}

TPI_scales

plot(AIC ~ Scale, TPI_scales,
     type = "b",
     xlab = "Scale (buffer radius in meters)",
     main = "Scale of effect for Topographic Position Index")

# Now we have completed our pseudo-optimization procedure to identify 
# the scale of effect for each of our covariates. Lets fit a series of single-
# scale models with each covariate at its scale of effect.

slope_scales[which(slope_scales$AIC==min(slope_scales$AIC)),]
water_scales[which(water_scales$AIC==min(water_scales$AIC)),]
PREC_scales[which(PREC_scales$AIC==min(PREC_scales$AIC)),]
Elev_scales[which(Elev_scales$AIC==min(Elev_scales$AIC)),]
TPI_scales[which(TPI_scales$AIC==min(TPI_scales$AIC)),]

summary(glm(HYAR ~ Slope_700, data = HYAR_data_final, family = binomial))
summary(glm(HYAR ~ Water_100, data = HYAR_data_final, family = binomial))
summary(glm(HYAR ~ PREC_300, data = HYAR_data_final, family = binomial))
summary(glm(HYAR ~ Elev_1200, data = HYAR_data_final, family = binomial))
summary(glm(HYAR ~ TPI_100, data = HYAR_data_final, family = binomial))

# But we are really interested in how all of these landscape features together
# influence the presence of Canyon Treefrogs. This is where we need to fit
# multi-scale model with each covariate represented at its pseudo-optimized
# scale of effect.

multi_scale_mod <- glm(HYAR ~ Slope_700 + Water_100 + 
                         PREC_300 + Elev_1200 + 
                         TPI_100, 
                       data = HYAR_data_final,
                       family = binomial)

summary(multi_scale_mod)

# What do we see?

# Lets z-score standardize the covariates in this model to better compare effect
# sizes. We can do this very easily by putting the scale() function around
# each of our covariates within our model formula and the glm() function will
# know to apply the scale() function to each covariate before fitting the model.

zmulti_scale_mod <- glm(HYAR ~ scale(Slope_700) + scale(Water_100) + 
                          scale(PREC_300) + scale(Elev_1200) + 
                         scale(TPI_100), 
                        data = HYAR_data_final,
                       family = binomial)

summary(zmulti_scale_mod)

# Now what can we say about the effect of landscape features on Canyon Treefrog
# distribution? 

new_data <- data.frame(Slope_700 = seq(min(HYAR_data_final$Slope_700),
                                       min(HYAR_data_final$Slope_700),length.out=30),
                       Water_100 = seq(min(HYAR_data_final$Water_100),
                                       min(HYAR_data_final$Water_100),length.out=30),
                       PREC_300 = seq(min(HYAR_data_final$PREC_300),
                                       min(HYAR_data_final$PREC_300),length.out=30),
                       Elev_1200 = seq(min(HYAR_data_final$Elev_1200),
                                       min(HYAR_data_final$Elev_1200),length.out=30),
                       TPI_100 = seq(min(HYAR_data_final$TPI_100),
                                       min(HYAR_data_final$TPI_100),length.out=30))

predict