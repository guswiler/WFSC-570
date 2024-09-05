# Lab Assignment 1
# Olivia Guswiler
# 10-09-2024

# load packages into r
library(terra)
library(tidyverse)

## 1. Create a SpatVector using the CSV file of site locations and data ----

# read frog_ponds.csv into r environment
data <- read_csv("lab/lab_assignment_due_10.09.2024/frog_ponds.csv")

# create SpatVector from data frame
frog_vect <- vect(data,     # data frame
              geom = c("x","y"),  # indicate columns containing spat. coords.
              crs = "epsg:26912") # define crs

plot(frog_vect)

## 2. Create at least two SpatRaster objects representing two landscape features that could be used as covariates in an analysis of the habitat associations of at least one species. You get to choose both the landscape features and the species. ----

# load in land cover file as SpatRaster
NLCD_rast <- rast("lab/lab_assignment_due_10.09.2024/frog_ponds_NLCD.tif")
NLCD_rast

# create SpatRasters for shrub/scrub LC and urban LC
scrub <- matrix(c(11,21,22,23,24,31,41,42,43,52,71,81,82,90,95, # matrix denoting 1 = scrub,
                  0,0,0,0,0,0,0,0,0,1,0,0,0,0,0),               #  0 = not scrub
                ncol = 2)
urban <- matrix(c(11,21,22,23,24,31,41,42,43,52,71,81,82,90,95, # matrix denoting 1 = urban,
                   0,1,1,1,1,0,0,0,0,0,0,0,0,0,0),              #  0 = not urban
                 ncol = 2)

scrub_rast <- classify(NLCD_rast, scrub) # apply matrices to original raster
urban_rast <- classify(NLCD_rast, urban)


# looking at the data
plot(scrub_rast,
     ext = ext(frog_vect),  # set extent to data
     col = c("lightgrey",   # not scrub
             "goldenrod2"), # scrub
     main = "scrub cover")
plot(frog_vect,
     add = T,
     pch = 20,
     col = c("cyan",    # KISO
             "yellow",  # BUAL
             "red",     # THCY
             "purple")) # RACA


plot(urban_rast,
     ext = ext(frog_vect),
     col = c("lightgrey",   # not urban
             "brown"),  # urban
     main = "urban cover")
plot(frog_vect,
     add = T,
     pch = 20,
     col = c("cyan",    
             "yellow",  
             "red",     
             "purple"))


# looking at just bullfrogs
sp_vect <- function(data, sp_code) {  
  df <- data %>% 
    filter(!!sym(sp_code) == 1) %>%  # keep rows with 1 in specified sp col
    select(Site, x, y, sp_code)      # remove cols other than specified
  vector <- vect(df,                 # create SpatVector from data frame
                 geom = c("x","y"),
                 crs = "epsg:26912")
  return(vector)
} # function to separate species data and create vector

RACA_vect <- sp_vect(data, "RACA") # do that
RACA_vect


# 3. Extract values for each landscape feature at some buffer around each site. You may use the same buffer for each feature or a different buffer for each feature ----

# define the buffer
RACA_buff <- buffer(RACA_vect,
                    width = 100) # 100m radius buffer

# add mean buffer values to vector
scrub_buff <- terra::extract(scrub_rast,
                             RACA_buff,
                             fun = "mean",
                             na.rm = T)
RACA_vect$scrub_buff <- scrub_buff$Red

urban_buff <- terra::extract(urban_rast,
                             RACA_buff,
                             fun = "mean",
                             na.rm = T)
RACA_vect$urban_buff <- urban_buff$Red


# looking at the data
RACA_ext <- ext(RACA_buff)  # set extent based on buffer


plot(scrub_rast,      # plot raster
     ext = RACA_ext,
     col = c("lightgrey",   
             "goldenrod2"),
     main = expression(italic("Rana catesbeiana")
                       *", scrub cover"))
plot(RACA_vect,       # add vector
     add = T,
     pch = 20,
     cex = 0.5,
     col = "purple")
plot(scrub_buff,      # add buffer
     add = T)


plot(urban_rast,
     ext = RACA_ext,
     col = c("lightgrey",
             "brown"),
     main = expression(italic("Rana catesbeiana")
                       *", urban cover"))
plot(RACA_vect,
     add = T,
     pch = 20,
     cex = 0.5,
     col = "purple")
plot(urban_buff,
     add = T)


# 4. Create boxplots describing the distributions of each feature at sites where the species was detected and sites where the species was not detected ----

boxplot(RACA_vect$scrub_buff,
        RACA_vect$urban_buff,
        names = c("scrub", "urban"),
        main = expression(italic("Rana catesbeiana")
                          *" observations between land cover types"),
        ylab = "Occupancy",
        xlab = "Land Cover Type",
        col = c("goldenrod2", "brown"),
        boxwex = 0.5)  # change box widths


# 5. Use a t-test (R function t.test()) or a Mann-Whitney test (R function wilcox.test()) to compare the values of each landscape feature between sites where your focal species was and was not detected.

# checking standard deviation and variance to determine what test to use
sd(RACA_vect$scrub_buff)
var(RACA_vect$scrub_buff)

sd(RACA_vect$urban_buff)
var(RACA_vect$urban_buff)

# 
wilcox.test(RACA_vect$scrub_buff,
            RACA_vect$urban_buff)
