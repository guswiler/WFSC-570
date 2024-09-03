# Lab Assignment 1
# Olivia Guswiler
# 10-09-2024

# load packages into r
library(terra)
library(tidyverse)

## 1. Create a SpatVector using the CSV file of site locations and data ----

# read frog_ponds.csv into r environment
frog_ponds <- read_csv("lab_assignment_due_10.09.2024/frog_ponds.csv")

# create SpatVector from data frame
frog_vect <- vect(frog_ponds,
              geom = c("x","y"),
              crs = "epsg:26917")

plot(frog_vect)

## 2. Create at least two SpatRaster objects representing two landscape features that could be used as covariates in an analysis of the habitat associations of at least one species. You get to choose both the landscape features and the species. ----

# loading in raster files
frog_DEM <- rast("lab_assignment_due_10.09.2024/frog_ponds_DEM.tif") # elevation

frog_NLCD <- rast("lab_assignment_due_10.09.2024/frog_ponds_NLCD.tif") # land cover


# separating species in frog_ponds
sp_vect <- function(data, sp_code) {  # function to separate species data and create vector
  df <- data %>% 
    filter(!!sym(sp_code) == 1) %>% 
    select(Site, x, y, sp_code)
  vector <- vect(df,
                 geom = c("x","y"),
                 crs = "epsg:26917")
  return(vector)
}

KISO_vect <- sp_vect(frog_ponds, "KISO")

BUAL_vect <- sp_vect(frog_ponds, "BUAL")

THCY_vect <- sp_vect(frog_ponds, "THCY")

RACA_vect <- sp_vect(frog_ponds, "RACA")

# looking at the data
plot(frog_DEM,
     main = "Kinosternon sonoriense, elevation")
plot(KISO_vect, add=TRUE,
     pch = 21)

plot(frog_DEM,
     main = "Bufo alvarius, elevation")
plot(BUAL_vect, add=TRUE,
     pch = 21)

plot(frog_DEM,
     main = "Thamnophis cyrtopsis, elevation")
plot(THCY_vect, add=TRUE,
     pch = 21)

plot(frog_DEM,
     main = "Rana catesbeiana, elevation")
plot(RACA_vect, add=TRUE,
     pch = 21)

# 3. Extract values for each landscape feature at some buffer around each site. You may use the same buffer for each feature or a different buffer for each feature ----

frog_buff <- buffer(frog_vect, width = 1000)

# create function to plot with buffers
plot_sp_buff <- function(raster, vector, title) { 
  plot(raster,
       main = title,
       ext = ext(vector))
  plot(vector,
       cex = 0.5,
       add = TRUE)
  plot(frog_buff,
       add = TRUE)
}

plot_sp_buff(frog_DEM, RACA_vect, "Rana catesbeiana, elevation")
plot_sp_buff(frog_NLCD, RACA_vect, "Rana catesbeiana, elevation")

# 4. Create boxplots describing the distributions of each feature at sites where the species was detected and sites where the species was not detected ----





# 5. Use a t-test (R function t.test()) or a Mann-Whitney test (R function wilcox.test()) to compare the values of each landscape feature between sites where your focal species was and was not detected.