# Olivia Guswiler / Emma Sudbeck
# Lab Assignment 4
# 2024-10-15

library(terra)

#### Task I: Measure the elevation at each site and calculate the mean elevation around each site using circular buffers with radii ranging from 100-1200 m ----

# load ELKI data and DEM raster into environment 
ELKI_data <- read.csv("lab_assignments/lab_assignment_4/ELKI_data_scale_lab.csv")
DEM <- rast("lab_assignments/lab_assignment_4/Final_DEM_for_scale_lab.tif")

head(ELKI_data)

# set buffer range
buffer_sizes <- seq(100,1200,by=100)


# extract site elevation data
site_elev <- extract(DEM, 
                     ELKI_data[,c("x","y")])
summary(site_elev)

# add a new column to ELKI_data with this site elevation data
ELKI_data$site_elev <- site_elev$DEM

# create SpatVector from ELKI data
site_data_SV <- vect(x = ELKI_data,
                     geom = c("x","y"),
                     crs = "epsg:26912")

# extract DEM averages within buffer
for(i in 1:length(buffer_sizes)){
  buff_i <- buffer_sizes[i]                  #for each buffer size
  cat("Starting buffer size =",buff_i,"\n")   #name column
  site_buffers_i <- buffer(site_data_SV,      #
                           width = buff_i)
  elev_buffer_i <- extract(DEM,
                           site_buffers_i,
                           fun = "mean")
  ELKI_data$TMP <- elev_buffer_i$DEM
  colnames(ELKI_data)[which(colnames(ELKI_data)=="TMP")] <- paste0("Elev_",buff_i)
  
}

#### Task II: Calculate topographic position index (TPI) at each of your spatial scales ----

# calculate topographic position index (TPI) for range of elevation scales
all_TPI <- ELKI_data$site_elev - ELKI_data[,grep("Elev_",colnames(ELKI_data))]
colnames(all_TPI) <- paste0("TPI_",buffer_sizes)  #rename cols to include buffer sizes

# add TPI data cols to ELKI_data_final
ELKI_data_final <- cbind(ELKI_data,all_TPI)

head(ELKI_data_final)





#### Task III: Determine the scale of effect of each landscape feature ----

## Forest
# create data frame to hold AIC values for comparison
Forest_scales <- data.frame(Covariate = "Forest",
                          Scale = seq(100,1200,by = 100),
                          AIC = NA)

# for loop to calculate AICs and enter in df
for(i in 1:nrow(Forest_scales)){
  cov_i <- paste0(Forest_scales$Covariate[i],"_",Forest_scales$Scale[i])
  data_i <- ELKI_data_final[,c("ELKI",cov_i)]
  model_i <- glm(data_i[,1] ~ data_i[,2], data = data_i, family = binomial)
  Forest_scales$AIC[i] <- AIC(model_i)
}


## Grass
Grass_scales <- data.frame(Covariate = "Grass",
                            Scale = seq(100,1200,by = 100),
                            AIC = NA)

for(i in 1:nrow(Grass_scales)){
  cov_i <- paste0(Grass_scales$Covariate[i],"_",Grass_scales$Scale[i])
  data_i <- ELKI_data_final[,c("ELKI",cov_i)]
  model_i <- glm(data_i[,1] ~ data_i[,2], data = data_i, family = binomial)
  Grass_scales$AIC[i] <- AIC(model_i)
}


## Water
Water_scales <- data.frame(Covariate = "Water",
                           Scale = seq(100,1200,by = 100),
                           AIC = NA)

for(i in 1:nrow(Water_scales)){
  cov_i <- paste0(Water_scales$Covariate[i],"_",Water_scales$Scale[i])
  data_i <- ELKI_data_final[,c("ELKI",cov_i)]
  model_i <- glm(data_i[,1] ~ data_i[,2], data = data_i, family = binomial)
  Water_scales$AIC[i] <- AIC(model_i)
}


## PREC
PREC_scales <- data.frame(Covariate = "PREC",
                           Scale = seq(100,1200,by = 100),
                           AIC = NA)

for(i in 1:nrow(PREC_scales)){
  cov_i <- paste0(PREC_scales$Covariate[i],"_",PREC_scales$Scale[i])
  data_i <- ELKI_data_final[,c("ELKI",cov_i)]
  model_i <- glm(data_i[,1] ~ data_i[,2], data = data_i, family = binomial)
  PREC_scales$AIC[i] <- AIC(model_i)
}


## Elevation
Elev_scales <- data.frame(Covariate = "Elev",
                          Scale = seq(100,1200,by = 100),
                          AIC = NA)

for(i in 1:nrow(Elev_scales)){
  cov_i <- paste0(Elev_scales$Covariate[i],"_",Elev_scales$Scale[i])
  data_i <- ELKI_data_final[,c("ELKI",cov_i)]
  model_i <- glm(data_i[,1] ~ data_i[,2], data = data_i, family = binomial)
  Elev_scales$AIC[i] <- AIC(model_i)
}


## TPI
TPI_scales <- data.frame(Covariate = "TPI",
                          Scale = seq(100,1200,by = 100),
                          AIC = NA)

for(i in 1:nrow(TPI_scales)){
  cov_i <- paste0(TPI_scales$Covariate[i],"_",TPI_scales$Scale[i])
  data_i <- ELKI_data_final[,c("ELKI",cov_i)]
  model_i <- glm(data_i[,1] ~ data_i[,2], data = data_i, family = binomial)
  TPI_scales$AIC[i] <- AIC(model_i)
}


# scales of effect
Forest_scales[which(Forest_scales$AIC==min(Forest_scales$AIC)),] # Forest 300
Grass_scales[which(Grass_scales$AIC==min(Grass_scales$AIC)),]    # Grass  100
Water_scales[which(Water_scales$AIC==min(Water_scales$AIC)),]    # Water  1200
PREC_scales[which(PREC_scales$AIC==min(PREC_scales$AIC)),]       # PREC   1200
Elev_scales[which(Elev_scales$AIC==min(Elev_scales$AIC)),]       # Elev   1200
TPI_scales[which(TPI_scales$AIC==min(TPI_scales$AIC)),]          # TPI    200


# single-scale logistic regression models
  # z-score standardized for ease of comparison
zForest_300_mod <- glm(ELKI ~ scale(Forest_300),
                       data = ELKI_data_final,
                       family = binomial)
zGrass_100_mod <- glm(ELKI ~ scale(Grass_100),
                      data = ELKI_data_final,
                      family = binomial)
zWater_1200_mod <- glm(ELKI ~ scale(Water_1200),
                       data = ELKI_data_final,
                       family = binomial)
zPREC_1200_mod <- glm(ELKI ~ scale(PREC_1200),
                      data = ELKI_data_final,
                      family = binomial)
zElev_1200_mod <- glm(ELKI ~ scale(Elev_1200),
                      data = ELKI_data_final,
                      family = binomial)
zTPI_200_mod <- glm(ELKI ~ scale(TPI_200),
                    data = ELKI_data_final,
                    family = binomial)


# summary of results
summary(zForest_300_mod)
    #                   Estimate Std. Error z value Pr(>|z|)  
    # (Intercept)         0.2796     0.1605   1.742   0.0815 .
    # scale(Forest_300)   0.4809     0.2751   1.748   0.0805 .
summary(zGrass_100_mod)
    #                   Estimate Std. Error z value Pr(>|z|)  
    # (Intercept)         0.2662     0.1583   1.682   0.0926 .
    # scale(Grass_100)    0.4426     0.1729   2.559   0.0105 *
summary(zWater_1200_mod)
    #                   Estimate Std. Error z value Pr(>|z|)
    # (Intercept)         0.2473     0.1547   1.599    0.110
    # scale(Water_1200)  -0.1693     0.1579  -1.072    0.284
summary(zPREC_1200_mod)
    #                   Estimate Std. Error z value Pr(>|z|)   
    # (Intercept)         0.2785     0.1599   1.742  0.08151 . 
    # scale(PREC_1200)    0.5793     0.2010   2.882  0.00396 **
summary(zElev_1200_mod)
    #                   Estimate Std. Error z value Pr(>|z|)   
    # (Intercept)         0.2611     0.1583   1.649  0.09912 . 
    # scale(Elev_1200)    0.4835     0.1729   2.797  0.00516 **
summary(zTPI_200_mod)
    #                   Estimate Std. Error z value Pr(>|z|)    
    # (Intercept)         0.4612     0.1893   2.436   0.0149 *  
    # scale(TPI_200)     -1.6084     0.3799  -4.234  2.3e-05 ***


# backtransform estimates to calculate probability of occupancy
plogis(zForest_300_mod$coefficients) # 0.6179698
plogis(zGrass_100_mod$coefficients)  # 0.6088709
plogis(zWater_1200_mod$coefficients) # 0.4577781
plogis(zPREC_1200_mod$coefficients)  # 0.6409159
plogis(zElev_1200_mod$coefficients)  # 0.6185717
plogis(zTPI_200_mod$coefficients)    # 0.1668174




#### Task IV: Fit a multi-covariate pseudo-optimized multi-scale model ----

# multi-scale pseudo-optimized multi-scale logistic regression model
zmulti_scale_mod <- glm(ELKI ~ scale(Forest_300) + scale(Grass_100) + 
                          scale(Water_1200) + scale(PREC_1200) + 
                          scale(Elev_1200) + scale(TPI_200),
                        data = ELKI_data_final,
                        family = binomial)

summary(zmulti_scale_mod)
    #                   Estimate Std. Error z value Pr(>|z|)    
    # (Intercept)         0.5428     0.2106   2.577  0.00996 ** 
    # scale(Forest_300)   0.5052     0.3362   1.503  0.13291    
    # scale(Grass_100)    0.5567     0.2069   2.691  0.00712 ** 
    # scale(Water_1200)   0.1254     0.1776   0.706  0.48034    
    # scale(PREC_1200)    0.1145     0.2564   0.446  0.65530    
    # scale(Elev_1200)    0.1226     0.2569   0.477  0.63332    
    # scale(TPI_200)     -1.8544     0.4495  -4.125  3.7e-05 ***


plogis(zmulti_scale_mod$coefficients)
    # scale(Forest_300) 0.6236900
    # scale(Grass_100)  0.6356843 **
    # scale(Water_1200) 0.5312968
    # scale(PREC_1200)  0.5285861
    # scale(Elev_1200)  0.5305994
    # scale(TPI_200)    0.1353571 ***




#### Bonus ----

# single-scale model, 100-m buffer for all covariates
zsingle_scale_mod <- glm(ELKI ~ scale(Forest_100) + scale(Grass_100) + 
                           scale(Water_100) + scale(PREC_100) + 
                           scale(Elev_100) + scale(TPI_100),
                         data = ELKI_data_final,
                         family = binomial)

summary(zsingle_scale_mod)
    #                     Estimate Std. Error z value Pr(>|z|)    
    # (Intercept)        0.4563995  0.1922863   2.374   0.0176 *  
    # scale(Forest_100)  0.1912816  0.2446730   0.782   0.4343    
    # scale(Grass_100)   0.5103934  0.2059736   2.478   0.0132 *  
    # scale(Water_100)   0.1992034  0.1835752   1.085   0.2779    
    # scale(PREC_100)    0.3867625  0.2542170   1.521   0.1282    
    # scale(Elev_100)   -0.0006012  0.2458037  -0.002   0.9980    
    # scale(TPI_100)    -1.6168475  0.3676102  -4.398 1.09e-05 ***


plogis(zsingle_scale_mod$coefficients)
    # scale(Forest_100) 0.5476751
    # scale(Grass_100)  0.6248987 *
    # scale(Water_100)  0.5496368
    # scale(PREC_100)   0.5955031
    # scale(Elev_100)   0.4998497
    # scale(TPI_100)    0.1656401 ***

# compare best fit
AIC(zsingle_scale_mod, zmulti_scale_mod)
    #                   df      AIC
    # zsingle_scale_mod  7 198.3974
    # zmulti_scale_mod   7 194.9354