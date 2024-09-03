rm(list=ls()) # clean workspace.  Caution!!!!

# Lab 1 (Part 2): Introduction to spatial data in R: rasters

# In today's lab, we will begin working with spatial data in R. 
# You may have learned in a GIS class that there are two major types of spatial
# data: raster and vector. We will cover vector data a bit later but will start
# with raster data because most of the spatial data you will work with in 
# wildlife habitat selection analyses will be raster data.

# Much of the resources in this lab come from R resources for spatial analyses
# available at https://rspatial.org/index.html

# This script picks up where we left off on Tuesday August 26. The remainder
# of this lab will focus on showing the tools in terra that can be used
# to work with and manipulate raster objects.

# Let's start by loading the terra package. The terra package replaced the older
# raster package and is a huge improvement in terms of computational speed. 

library(terra)

?terra

# Working with rasters -----

# Now that we have some foundational knowledge of rasters lets bring in a 
# real-world raster and explore some of terra's functionality for working
# with and manipulating rasters.

# We'll start with the 2021 NLCD land cover layer for Maricopa County. 
# We will talk a bit more about NLCD data in a future lab but more
# information is available about NLCD data at this link:
# https://www.mrlc.gov 
# Importantly, here is the land cover legend and definitions:
# https://www.mrlc.gov/data/legends/national-land-cover-database-class-legend-and-description 

# To read in a raster, in this case a GeoTiff (.tif) file that already
# exists, we will use the rast() function and supply a file path.
# You can include the file path directly in rast() like this:
# NLCD <- rast("the_file_path_to_your_file")
# But if your file path is very long (buried deep within many subfolders)
# you may want to specify your file path as a character vector object
# and then provide this object to rast().

# The file path on my computer for this GeoTiff file is as follows:

file_paths <- paste0("C:/Users/jbauder/Box/Bauder_Coop_Lab/UA_Teaching/",
                     "WFSC 570 Habitat Analysis 3cr/ArcGIS/rasters/",
                     "NLCD_2021_Maricopa_for_lab.tif")

# You will need to change this file path to the location on your computer
# where you have stored this file. I would suggest creating a folder
# specifically for this labs in this class and saving all the files
# from labs within this folder. It often helps to keep the file path simple:
# (e.g., C:/School/WFSC570/Labs)

# When you have created your file path object, it is often nice to use 
# the file.exists() function to confirm your file path is correct

file.exists(file_paths)

NLCD <- rast(file_paths)

# What can you tell me about this SpatRaster object?

# Now plot our raster

plot(NLCD)

# The legend makes it appear that terra is thinking about the pixel values
# as continuous values, like elevation or percent canopy cover. But we know
# that our pixel values, while they are numbers, are in fact discrete 
# categories. It might therefore be more useful to create a plot where
# the legend shows the unique and discrete pixel values. We can do this
# with the type= argument within plot and setting type="classes"

plot(NLCD, type="classes")

# Crop -----

# We often have a raster that is larger than we need it, or we might want to 
# look at a subset of our data in more detail. Let's crop our Maricopa County
# raster to the area around Lake Pleasant. We do this using terra's crop() 
# function. crop() has two primary arguments: a SpatRaster object we wish to
# crop (in this case it is our NLCD SpatRaster) and an object that has
# a spatial extent or SpatExtent attribute. Remember that the extent of a
# spatial object consists of four numbers: the maximum and minimum values along
# the x and the y axes. Every SpatRaster (and SpatVector) has SpatExtent.
# For example, lets use the ext() function to get the SpatExtent from NLCD.

NLCD_extent <- ext(NLCD)
NLCD_extent
class(NLCD_extent)

# So to crop NLCD to some area around Lake Pleasant we just need to specify 
# the four numbers that will comprise our spatial extent. It might be
# helpful here to list them out as their own objects and then combine
# them to create a SpatExtent

x_min <- 371141
x_max <- 396700
y_min <- 3740971
y_max <- 3762533

lake_pleasant_extent <- c(x_min,x_max,y_min,y_max)

extent <- ext(lake_pleasant_extent)
class(extent)

# Since SpatExtent is a spatial object, we can plot it.

plot(extent)

# That doesn't tell us much, so lets plot NLCD and add our new SpatExtent
# to that plot

plot(NLCD, type = "classes")
plot(extent,add=T)

# Now that we have our spatial extent of the area we would like to crop
# lets use the crop() function to do just that

NLCD_crop <- crop(NLCD,extent)

plot(NLCD_crop, type = "classes")

# Reclassify ------

# We often want to reclassify our rasters to group certain values together
# (e.g., convert a continuous surface into a multi-level categorical surface
# or recombine categories). This is particularly useful for categorical land
# cover surfaces.

# To use the classify function in terra, we first need to create a matrix that 
# tells R what original values should be converted into which new values. For
# categorical land cover maps we can use a two-column matrix specifying the
# "is" and "becomes" values.

# Lets say we only want a layer showing forest land covers. We can create
# a binary raster where 1 represents all three of the NLCD forest classes and
# 0 represents everything else.

forest_matrix <- matrix(c(11,21,22,23,24,31,41,42,43,52,71,81,82,90,95,
                          0,0,0,0,0,0,1,1,1,0,0,0,0,0,0),
                        ncol = 2)
forest_matrix

# When we look at this matrix we see that only values 41, 42, and 43 will
# become 1's while all other values will become 0's

FOREST <- classify(NLCD, forest_matrix)
FOREST

# How much forest land cover is around Maricopa County?

plot(FOREST)

# What happens when you run this:
names(FOREST)

# Why do we see "Red" here? Long story, but if you look inside the NLCD raster
# files you download, you will see that they include values for Red, Blue, Green,
# Opacity, and NLCD_Land (land cover values). For some reason, when you bring
# a NLCD layer into R through terra, the SpatRaster values are those for the Red
# band but when you plot it, you will see the original land cover values. I've
# modified the values so they are the original land cover values but terra
# still thinks those are values from the "Red" band. We can change the name
# for these values very easily by:

names(FOREST) <- "Forest"

# We can confirm that our new FOREST raster only includes 0's and 1's by
# using terra's unique() function. Because base R has its own unique()
# function, we can use tera:: to tell R to use the unique() function 
# within terra

terra::unique(FOREST)

# Finally, terra has a function that will calculate the number of pixels 
# with a given unique value: freq(). I wouldn't run this function on a 
# raster that is intrinsically continuous (like a digital elevation model)
# but for categorial land cover maps this works very well.

freq(FOREST)

# We can also calculate various summary statistics for our entire raster

global(FOREST, fun = "mean") # Calculates the mean value of the raster
global(FOREST, fun = "sd") # Calculates the standard deviation of the raster
global(FOREST, fun = "range") # Calculates the minimum and maximum raster values
terra::summary(FOREST) # Summarizes the raster values at different quantiles

# Euclidean distance to a feature -------

# In our habitat analyses we are often interested in using distance to a 
# feature as a covariate. This might be distance to nearest water, distance
# to forest edge, or distance to road. These distances are invariably 
# measured as Euclidean (straight line) distances, which may or may not
# be appropriate, but we can easily measure such distances using terra.

# Let's measure Euclidean distance to nearest water using our cropped NLCD raster
# around Lake Pleasant. S

# To use the distance() function in terra we need to reclassify our raster
# further. Specifically, distance() measures the distance, for all pixels 
# that are NA in the raster to the nearest cell that is not NA. So if
# we want to measure the distance to the nearest water we need to 
# reclassify our raster so that water pixels are one and all other 
# pixels are NA.

# This matrix uses a different format. Instead of listing out every value of NLCD
# and what each value should be reclassified to, we can provide a range of values
# and what values within each range should become.

water_matrix <- matrix(c(0,11,
                         11,95,
                         1,NA),
                       ncol = 3)
water_matrix

WATER <- classify(NLCD_crop, water_matrix)

plot(WATER)

# We only see pixels with values of 1 appear on our plot. To verify that the rest
# of our raster is NA we can use the plotNA= argument in the plot() function
# to plot all NA pixels as a certain color. 

plot(WATER,colNA="lightblue")

freq(WATER)

# Now we can run the distance() function. Here we will use the system.time() function
# to see how long this will take. The system.time() function works by placing an
# entire function within the parentheses which allows system.time() to print the
# time taken to run the function to the console.

system.time(water_dist <- terra::distance(WATER))

# Notice it did not take very long for our small raster.

plot(water_dist)

# We can also overlay our reclassified WATER raster
plot(WATER,add=T,col="blue")

# Creating edges ------

# Edge environments, or ecotones, play a very important role in wildlife and
# community ecology and are often worth considering in wildlife habitat analyses.
# We can use R to create a binary "edge" raster where 1 represents an edge pixel
# and 0 represents all other pixels.

# We can use the boundaries() function but be warned that this function can take
# a while to run. It took 8.6 minutes to run this function on our FOREST raster,
# and there isn't much forest in Maricopa County! So lets crop a smaller raster
# to demonstrate how this function works.

FOREST_crop <- crop(FOREST, # Our raster to crop
                    ext(c(423649,424285,3773796,3774313))) # Our SpatExtent

# Since we will be plotting a binary raster (only two values, 0 or 1), we can
# manually specify the colors to plot for each value

plot(FOREST_crop,
     col=c("white","green"))

# The default arguments for boundaries() calculate the boundary between non-NA and
# NA pixels. This works well in the case of a binary raster, we just need to convert
# our 0's (non-forest) pixels to NA and then boundaries() will give us the edge
# pixels of our remaining pixels with values of 1 (i.e., our forest pixels)

# We can easily do this by indexing, that is identifying the pixels that meet a certain
# condition (in this case, pixles that are zero) and then reassigning their values.

FOREST_crop[FOREST_crop==0] <- NA
plot(FOREST_crop,colNA="lightblue",
     col=c("white","green"))

system.time(forest_edge <- terra::boundaries(FOREST_crop,classes=F,inner=T,directions=8))
plot(forest_edge,colNA="lightblue",
     col=c("white","green"))

# If we plot the SpatRaster returned by boundaries() we see that we have our 1's
# representing the edge pixels but we also have some 0 pixel values in addition
# to our NA pixels. These 0 pixels represent pixels that were forest (where 1's)
# in our original FOREST_crop raster but are not edge pixels.

# At this point we probably want to convert all NA pixels back to 0's since we
# probably will want a binary edge raster (i.e., 1 = forest edge, 0 = everything else)
# for subsequent analyses. We can use our same indexing approach, this time with
# the is.na() function which asks whether or not each pixel is NA.

forest_edge[is.na(forest_edge)] <- 0
plot(forest_edge,colNA="lightblue",
     col=c("white","green"))

# Creating layers ---------

# Within terra we can combine multiple rasters of identical extents and
# resolutions into a single multi-layered SpatRaster object. This is often 
# useful when making model-based predictions with our rasters.

# Lets read in a GeoTiff file (.tif) for a digital elevation model (DEM).
# To combine multiple rasters into a single multi-layered SpatRaster object
# the rasters must be identical in dimension, extent, and resolution.
# I ensured that our NLCD and our DEM rasters would line up by first 
# creating them in ArcPro. You could do this using the terra package in R
# but that is beyond the scope of this lab.

dem_file_path <- paste0("C:/Users/jbauder/Box/Bauder_Coop_Lab/UA_Teaching/",
                        "WFSC 570 Habitat Analysis 3cr/ArcGIS/rasters/",
                        "DEM_Maricopa.tif")

DEM <- rast(dem_file_path)

# Creating a multi-layered SpatRaster is as simple as:
layers <- c(FOREST,DEM)
layers
plot(layers)

# Writing rasters -------------

# Saving rasters created using terra to disk is very easy using the writeRaster
# function. 

output_raster_path <- paste0("C:/Users/jbauder/Box/Bauder_Coop_Lab/UA_Teaching/",
                             "WFSC 570 Habitat Analysis 3cr/ArcGIS/rasters/",
                             "FOREST_and_DEM.tif")

writeRaster(FOREST_smooth, # The raster to save
            output_raster_path, # The file path for our saved raster
            overwrite = T) # Tells R to over-write or copy over any previous versions
                              # of the file. 


