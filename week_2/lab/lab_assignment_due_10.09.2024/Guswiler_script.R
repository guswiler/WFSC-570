# Lab Assignment 1
# Olivia Guswiler
# 10-09-2024

# load packages into r
library(terra)

## 1. Create a SpatVector using the CSV file of site locations and data ----

# read frog_ponds.csv into r environment
frog_ponds <- read.csv("lab/lab_assignment_due_10.09.2024/frog_ponds.csv")

# create SpatVector from data frame
frog_vect <- vect(frog_ponds,     # data frame
              geom = c("x","y"),  # indicate columns containing spat. coords.
              crs = "epsg:26917") # define crs

plot(frog_vect)

## 2. Create at least two SpatRaster objects representing two landscape features that could be used as covariates in an analysis of the habitat associations of at least one species. You get to choose both the landscape features and the species. ----

# load in land cover file as SpatRaster
NLCD_rast <- rast("lab/lab_assignment_due_10.09.2024/frog_ponds_NLCD.tif")
NLCD_rast

# create SpatRasters for shrub/scrub LC and grassland/herbaceous LC
scrub <- matrix(c(11,21,22,23,24,31,41,42,43,52,71,81,82,90,95, # matrix denoting 1 = scrub,
                  0,0,0,0,0,0,0,0,0,1,0,0,0,0,0),               #  0 = not scrub
                ncol = 2)
herb <- matrix(c(11,21,22,23,24,31,41,42,43,52,71,81,82,90,95, # matrix denoting 1 = herbaceous,
                   0,0,0,0,0,0,0,0,0,0,1,0,0,0,0),              #  0 = not herbaceous
                 ncol = 2)

scrub_rast <- classify(NLCD_rast, scrub) # apply matrices to original raster
herb_rast <- classify(NLCD_rast, grass)


# looking at the data
frog_ext <- ext(frog_vect) # set extent for maps based on vector


plot(scrub_rast,
     ext = frog_ext,
     col = c("lightgrey",   # not scrub
             "goldenrod2"), # scrub
     main = "scrub cover")
plot(frog_vect,
     add = T,
     pch = 20,
     col = c("cyan",    # KISO
             "yellow",  # BUAL
             "red",     # THYC
             "purple")) # RACA


plot(herb_rast,
     ext = frog_ext,
     col = c("lightgrey",   # not herb
             "limegreen"),  # herb
     main = "herbaceous cover")
plot(frog_vect,
     add = T,
     pch = 20,
     col = c("cyan",    # KISO
             "yellow",  # BUAL
             "red",     # THYC
             "purple")) # RACA


# separate vectors for each species in frog_ponds
sp_vect <- function(data, sp_code) {  
  df <- data %>% 
    dplyr::filter(!!sym(sp_code) == 1) %>% 
    select(Site, x, y, sp_code)
  vector <- vect(df,
                 geom = c("x","y"),
                 crs = "epsg:26917")
  return(vector)
} # fn to separate sp data and create vector

KISO_vect <- sp_vect(frog_vect, "KISO")
BUAL_vect <- sp_vect(frog_vect, "BUAL")
THCY_vect <- sp_vect(frog_vect, "THCY")
RACA_vect <- sp_vect(frog_vect, "RACA")

KISO <- terra::extract(frog_vect$KISO)

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