# Olivia Guswiler / Alexis Rickert
# Lab Assignment 1
# 2024-09-10

# load package
library(terra)

# read CSV into environment as a data frame
frog_data <- read.csv("week_2/lab/lab_assignment/frog_ponds.csv")

# create SpatVector from data frame
frog_vect <- vect(frog_data,          # data frame
                  geom = c("x","y"),  # indicate cols containing spat. coords.
                  crs = "epsg:26912") # define coord. ref. sys.

# read feature maps into environment as SpatRasters
NLCD <- rast("week_2/lab/lab_assignment/frog_ponds_NLCD.tif")
DEM <- rast("week_2/lab/lab_assignment/frog_ponds_DEM.tif")

# change NLCD to only show wetlands
wet_matrix <- matrix(c(11,21,22,23,24,31,41,42,43,52,71,81,82,90,95, # create matrix defining
                          0,0,0,0,0,0,0,0,0,0,0,0,0,1,1),            #  wetland land cover
                        ncol = 2)

wetland <- classify(NLCD, wet_matrix) # apply matrix


# define the buffer
buffer <- buffer(frog_vect,    # where to apply the buffer
                 width = 160)  # buffer radius

# quick look at the data
plot(wetland,
     ext = ext(buffer),
     col = c("gray",
             "dodgerblue"))
plot(frog_vect,
     add = T,
     cex = 0.5,
     col = "red")
plot(buffer,
     add = T)

plot(DEM,
     ext = ext(frog_vect))
plot(frog_vect,
     add = T,
     cex = 0.5,
     col = "red")
plot(buffer,
     add = T)

# creating objects for the buffer
wetland_buffer <- extract(wetland,
                          buffer,
                          fun = "mean",
                          na.rm = T)

DEM_buffer <- extract(DEM,
                      buffer,
                      fun = "mean",
                      na.rm = T)

# adding the buffers to the vector with our data
frog_vect$wetland_160_buffer_mean <- wetland_buffer$Red
frog_vect$DEM_160_buffer_mean <- DEM_buffer$AZ_BAEA_DEM_snapped_rsmpld_NAD83_12N

# creating the box plots
boxplot(wetland_160_buffer_mean   # landscape feature
        ~ RACA,                   # species
        frog_vect,
        col = c("gray",
                "lightgreen"),
        names = c("absent", "present"),
        xlab = "",
        ylab = "wetland land cover within 160 m",
        main = expression(italic("Rana catesbeiana")
                          *", presence in wetland areas"))

boxplot(DEM_160_buffer_mean       
        ~ RACA,                   
        frog_vect,
        col = c("gray",
                "lightgreen"),
        names = c("absent", "present"),
        xlab = "",
        ylab = "elevation within 160 m",
        main = expression(italic("Rana catesbeiana")
                          *", presence across elevation (ft)"))

# analyzing the data
wilcox.test(wetland_160_buffer_mean
            ~ RACA,
            frog_vect)

t.test(DEM_160_buffer_mean
       ~ RACA, frog_vect,
       var.equal = T)
