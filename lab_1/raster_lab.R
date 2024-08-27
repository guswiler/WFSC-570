rm(list=ls()) # clean workspace.  Caution!!!!


# Lab 1: Introduction to spatial data in R: rasters ----

# In today's lab, we will begin working with spatial data in R. 
# You may have learned in a GIS class that there are two major types of spatial data: raster and vector. We will cover vector data a bit later but will start with raster data because most of the spatial data you will work with in wildlife habitat selection analyses will be raster data.

# Much of the resources in this lab come from R resources for spatial analyses available at https://rspatial.org/index.html


# Let's start by loading the terra package. The terra package replaced the older raster package and is a huge improvement in terms of computational speed. 

library(terra)

?terra

# rasters created by terra are called SpatRasters and can include multiple layers within a single SpatRaster object.

# Let's start by creating a SpatRaster from scratch using the rast function
?rast

# As you can see, there are many ways to create a raster. The simplest is by specifying the number of rows and columns.
## rasters are essentially grids

x <- rast(nrows=21, ncols=21)

# If we look at this SpatRaster, we see that we have an object of class SpatRaster with 21 rows and 21 columns and 1 layer (nlyr). Resolution refers to cell or pixel size, extent gives the minimum and maximum x- and y-coordiates, and by default our raster is in the WGS84 coordinate reference system.
class(x) ## attributes about the object (type and source package)
x
ncell(x) ## number of cells in a raster
res(x)   ## size of each cell (pixel) within the raster

# We can also plot our raster. What happens when we do that?

plot(x)

# We have an empty raster; no values have been assigned to the pixels. We can easily add them by drawing a vector of random numbers equal in length to the number of pixels.

## very easy to assign values to an empty raster. Just assign values.
# x[] <- rnorm(ncell(x))
values(x) <- rnorm(ncell(x)) ## rnorm provides an n of values with mean=0, sd=1 (also seen in Z-distributions)
x
plot(x)

# When we plot a raster in base R as we have done, we can add points, lines, and polygons to the plotted raster. This is often helpful if you have a raster of your study area (say land cover) and you want to quickly plot some UTM coordinates or a shapefile just to see how things overlap. We'll get into this in more detail in a later lab.

terra::points(x = c(-50,25,0),  ## specify x-coors for each point
              y = c(10,20,-47), ## specify y-coors
              pch = 21,         ## point symbol
              col = "blue",     ##outline or color
              bg = "green",     ## fill color if hollow shape
              cex = 2)          ## point size

# Lets talk about coordinate reference systems (CRS). CRS are used to tell you (and R) where on earth your raster is located. There are two basic types of CRS: angular and planar (planar is also called Cartesian). Angular CRS use longitude and latitude and are called angular because they estimate the angles between either the equatorial plane or a reference meridian and a line drawn from the point to the center of the earth. To estimate these angles one needs a model of the earth, or a datum.
# When using long/lat we are typically using the WGS84 datum. NAD83
# Just as a side note, it is not strictly correct to say my coordinates are in WGS84. What you usually mean to say is that my coordinates are in long/lat using or relative to WGS84.

# While recording coordinates using long/lat is very common, it is often more convenient for spatial analyses to have your coordinates in a planar CRS.
# One reason is that it makes spatial calculations much easier to have your coordinates on a symmetrical x/y grid. The distance between two long/lat points can be different depending on where you are on the Earth's surface (closer or further from the poles, for example).
# But if you are on a symmetrical x/y grid, you can use the Pythagorean theorem to calculate distance between two points.

# Planar CRS are referred to as projections (e.g., UTM, Mercator, Albers, Robinson, Lambert, Sinusoidal) in contrast to datum which refers to the model of the earth used by the CRS.

#### For this reason, I always encourage folks to collect their data in some type of planar CRS, usually UTM NAD83 or UTM WGS84. ####

# A planar CRS is defined by a projection, datum, and additional parameters that determine things like where the center of the map is. Rasters and vectors (e.g., polygon shapefiles) in R must be assigned a CRS. There is no on-the-fly projection as in ESRI ArcGIS products.

# Note: R used to specify CRS using the PROJ.4 notation (e.g., in the older raster and rgdal packages). This notation is not fully supported in newer R packages but that should not affect our work in this class.

# Let's look at the CRS for our raster created with the WGS84 datum
crs(x) ## everything needed to know when setting up a coordinate reference system.

# There is a lot here but notice the WGS84 and the datum of World Geodetic System 84 (WGS84).


# There is a resource to identify the CRS you are using and then use it in R and this is the list of EPSG codes (European Petroleum Survey Group). All commonly used CRS will have a EPSG code and you can use this code to look up your CRS. EPSG codes can be found here: https://spatialreference.org. The EPSG code for WGS84 is 4326.

## using these codes--v
# If we create an empty raster, we can assign the CRS as we create it. Lets do that but now use the CRS for NAD83 UTM Zone 12 N (https://spatialreference.org/ref/epsg/26912/).

xNAD83 <- rast(nrows = 100, ncol = 100,
               crs = "epsg:26912")

# Or we can assign a CRS after we create the raster.

xNAD83 <- rast(nrows = 100, ncol = 100)
crs(xNAD83) <- "epsg:26912"   ## changing the coordinate reference system
values(xNAD83) <- rnorm(ncell(xNAD83))  ## setting random values
xNAD83
plot(xNAD83)

# What happens if we have a raster in one CRS but want to project it to a different CRS? We can use terra's project() function but projecting rasters can be tricky because going from one CRS to another can distort the projected raster.
# Here's an example:

## create a blank raster
r <- rast(xmin=-110, xmax=-90, ## specifying the extent of the raster (where our corners are)
          ymin=40, ymax=60,
          ncols=40, nrows=40)
values(r) <- 1:ncell(r)
r
plot(r)


## now try some projection
newcrs <- "+proj=robin +datum=WGS84" ## just used as an example to illustrate the point
pr1 <- terra::project(r, newcrs)     ## changing "r" to a new projection
crs(pr1)
plot(pr1)   ## can see the distortion with the reprojection.

# For this reason, if I'm going to be reprojecting rasters I use ArcGIS.
# Let's switch over and see what that looks like.

# Working with rasters -----

# Now that we have some foundational knowledge of rasters lets bring in a real-world raster and explore some of terra's functionality for working with and manipulating rasters.

# We'll start with the 2021 NLCD land cover layer for Maricopa County

NLCD <- rast("lab_1/NLCD_2021_Maricopa.tif")
## you'll need to set your own file path that will look something like C:\Users\Olivia\Documents\R_Projects\WFSC-570\lab_1\NLCD_2021_Maricopa.tif when you paste it in, you'll need to change the \ to / for it to work in r.
## if you use setwd() you can set your working directory to direct it into the folder you're gathering files from so you can describe a shorter path like I did above.



### side note from Javan
file_paths <- paste0("C:/Users/Olivia/Documents/",
                     "R_Projects/WFSC-570/lab_1/NLCD_2021_Maricopa.tif")
## he was just using paste0() to connect all the bits of his filepath (everything between the commas) because he doesn't like long file paths.



# What do we see when we read this in? We have a single-layer SpatRaster with 30 x 30 unit pixels (the units are meters because we are in UTM) and a NAD83 UTM zone 12N CRS and values ranging from 70-108.

NLCD         ## stands for National Land Cover Data, may be a useful resource for large region analyses
ncell(NLCD)  ## total number of cells
crs(NLCD)    ## coord. ref. sys.
res(NLCD)    ## resolution (30x30m b/c UTM is in meters)

plot(NLCD)

## raster processing tools ----
# Crop -----

# We often have a raster that is larger than we need it, or we might want to look at a subset of our data in more detail. Let's crop our Maricopa County raster to the area around Lake Pleasant. 

extent <- ext(c(371141,396700,3740971,3762533)) ## setting a spatial extent using ext() based on our area of interest

plot(NLCD)
plot(extent, add=TRUE)  ## adds a small box to our plot showing the extent that we have specified

NLCD_crop <- crop(NLCD,extent) ## crops NLCD raster to our spatial extent
NLCD_crop  ## see how it has much fewer rows and cols than the original NLCD raster
## you can see the extent in this raster is diff than the extent specified for the crop. This is because r won't cut cells in the raster, so it may have a slightly different extent in the raster.
## b/c of this, make sure to include a buffer around your study area when cropping. Also will talk about buffers in relation to edge effects later on.
## later on, be sure that later rasters you use in analyses line up in their extent (use first one as template for all other raster cropping). So in future crops you can specify it like: NLCD_new <- crop(NLCD, NLCD_crop).

plot(NLCD_crop)

## Ended here on Tuesday 8/27

# Reclassify ------ 

# We often want to reclassify our rasters to group certain values together
# (e.g., convert a continuous surface into a multi-level categorical surface
# or recombine categories). This is particularly useful for categorical land
# cover surfaces.
# One of the first reclassifications we might want to try here is converting
# the values we see into the original land cover values found at
# https://www.mrlc.gov/data/legends/national-land-cover-database-class-legend-and-description

# To use the classify function in terra, we first need to create a matrix that 
# tells R what original values should be converted into which new values. For
# categorical land cover maps we can use a two-column matrix specifying the
# "is" and "becomes" values.

reclass <- matrix(c(70,222,217,235,171,179,104,28,181,204,223,220,171,184,108,
                    11,21,22,23,24,31,41,42,43,52,71,81,82,90,95),
                  ncol = 2)

NLCD <- classify(NLCD, reclass)
NLCD
plot(NLCD)

# Now lets say we only want a layer showing forest land covers. We can create
# a binary raster where 1 represents all three of the NLCD forest classes and
# 0 represents everything else.

forest_matrix <- matrix(c(11,21,22,23,24,31,41,42,43,52,71,81,82,90,95,
                          0,0,0,0,0,0,1,1,1,0,0,0,0,0,0),
                        ncol = 2)
FOREST <- classify(NLCD, forest_matrix)
FOREST
terra::unique(FOREST)
plot(FOREST)

table(as.matrix(FOREST))

# We can also use a three-column matrix to convert ranges of values, rather
# than individual values. This approach can get confusing depending on whether 
# you want the to-from values to be included in their respective rows. By 
# default, classify excludes the "to" value in your reclassification matrix
# So in the example below, 0 would not be included in the first row,
# 40 would not be included in the second row, and 44 would not be
# included in the third row. Which is fine for this illustration
# using an integer raster.

forest_matrixb <- matrix(c(0,40,44,
                           40,44,100,
                           0,1,0),
                         ncol = 3)
forest_matrixb
FORESTb <- classify(NLCD, forest_matrixb)
plot(FORESTb)
table(as.matrix(FORESTb))


# We can also calculate various summary statistics for our entire raster

global(FOREST, fun = "mean")
global(FOREST, fun = "sd")
global(FOREST, fun = "range")
terra::summary(FOREST)


# Euclidean distance to a feature -------

# In our habitat analyses we are often interested in using distance to a 
# feature as a covariate. This might be distance to nearest water, distance
# to forest edge, or distance to road. These distances are invariably 
# measured as Euclidean (straight line) distances, which may or may not
# be appropriate, but we can easily measure such distances using terra.

# Let's measure distance to nearest water using our cropped NLCD raster
# around Lake Pleasant. Since we have reclassified the original NLCD
# layer to the actual NLCD land cover values lets re-crop our raster.

NLCD_crop <- crop(NLCD,extent)
NLCD_crop
plot(NLCD_crop)

# To use the distance() function in terra we need to reclassify our raster
# further. Specifically, distance() measures the distance, for all cells 
# that are NA in the raster to the nearest cell that is not NA. So if
# we want to measure the distance to the nearest water we need to 
# reclassify our raster so that water pixels are one and all other 
# pixels are NA.


water_matrix <- matrix(c(0,11,
                         11,95,
                         1,NA),
                       ncol = 3)
water_matrix
WATER <- classify(NLCD_crop, water_matrix)
plot(WATER)
plot(WATER,colNA="lightblue")
table(as.matrix(WATER))

# Now we can run the distance() function

system.time(water_dist <- terra::distance(WATER))
plot(water_dist)

# We can also overlay our reclassified WATER raster
plot(WATER,add=T)

# Creating edges ------

# Edge environments, or ecotones, play a very important role in wildlife and
# community ecology and are often worth considering in wildlife habitat analyses.
# We can use R to create a binary "edge" raster where 1 represents an edge pixel
# and 0 represents all other pixels.

# We can use the boundaries() function but be warned that this function can take
# a while to run. It took 8.6 minutes to run this function on our FOREST raster,
# and there isn't much forest in Maricopa County! So lets crop a smaller raster
# to demonstrate how this function works.

FOREST_crop <- crop(FOREST, ext(c(423649,424285,3773796,3774313)))
plot(FOREST_crop,col=c("white","green"))

FOREST_crop[FOREST_crop==0] <- NA
plot(FOREST_crop,colNA="lightblue",
     col=c("white","green"))

system.time(forest_edge <- terra::boundaries(FOREST_crop,classes=F,inner=T,directions=8))
forest_edge[is.na(forest_edge)] <- 0
plot(forest_edge,colNA="lightblue",
     col=c("white","green"))

# Creating layers ---------

# Within terra we can combine multiple rasters of identical extents and
# resolutions into a single multi-layered SpatRaster object. This is often 
# useful when making model-based predictions with our rasters. 

DEM <- rast(paste0("C:/Users/jbauder/Box/Bauder_Coop_Lab/UA_Teaching/",
                    "WFSC 570 Habitat Analysis 3cr/ArcGIS/rasters/",
                    "DEM_Maricopa.tif"))

layers <- c(FOREST,DEM)
layers
plot(layers)

# Writing rasters -------------

# Saving rasters created using terra to disk is very easy using the writeRaster
# function. 

writeRaster(FOREST_smooth,
            paste0("C:/Users/jbauder/Box/Bauder_Coop_Lab/UA_Teaching/",
                   "WFSC 570 Habitat Analysis 3cr/ArcGIS/rasters/",
                   "FOREST_smoothb.tif"),
            overwrite = T)


