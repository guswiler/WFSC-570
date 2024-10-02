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

# Now that we have some foundational knowledge of rasters lets bring in a real-world raster and explore some of terra's functionality for working with and manipulating rasters.

# We'll start with the 2021 NLCD (national land cover data) land cover layer for Maricopa County. 
# We will talk a bit more about NLCD data in a future lab but more information is available about NLCD data at this link: https://www.mrlc.gov 
# Importantly, here is the land cover legend and definitions: https://www.mrlc.gov/data/legends/national-land-cover-database-class-legend-and-description 

# To read in a raster, in this case a GeoTiff (.tif) file that already exists, we will use the rast() function and supply a file path.
# You can include the file path directly in rast() like this: NLCD <- rast("the_file_path_to_your_file"). But if your file path is very long (buried deep within many subfolders) you may want to specify your file path as a character vector object and then provide this object to rast().

# The file path on my computer for this GeoTiff file is as follows:

file_paths <- "week_1/lab/NLCD_2021_Maricopa_for_lab.tif"

# You will need to change this file path to the location on your computer where you have stored this file. I would suggest creating a folder specifically for this labs in this class and saving all the files from labs within this folder. It often helps to keep the file path simple: (e.g., C:/School/WFSC570/Labs)

# When you have created your file path object, it is often nice to use the file.exists() function to confirm your file path is correct

file.exists(file_paths) # TRUE or FALSE

NLCD <- rast(file_paths)

# What can you tell me about this SpatRaster object?

NLCD

# Now plot our raster

plot(NLCD)

# The legend makes it appear that terra is thinking about the pixel values
# as continuous values, like elevation or percent canopy cover.
# But we know that our pixel values, while they are numbers, are in fact discrete categories. 
# It might therefore be more useful to create a plot where the legend shows the unique and discrete pixel values. We can do this
# with the type= argument within plot and setting type="classes"

plot(NLCD, type="classes")

# NLCD website has legend for what these discrete classes indicate.

# Crop ----- Pulling out a small chunk of our raster

# We often have a raster that is larger than we need it, or we might want to  look at a subset of our data in more detail.
# Let's crop our Maricopa County raster to the area around Lake Pleasant. We do this using terra's crop() function.
# crop() has two primary arguments:
  # a SpatRaster object we wish to crop (in this case it is our NLCD SpatRaster)
  # an object that has a spatial extent or SpatExtent attribute.
# Remember that the extent of a spatial object consists of four numbers: the maximum and minimum values along the x and the y axes. Every SpatRaster (and SpatVector) has SpatExtent.
# For example, lets use the ext() function to get the SpatExtent from NLCD.

NLCD_extent <- ext(NLCD) # defining extent based on the extent of NLCD
NLCD_extent
class(NLCD_extent)      # can see that in terra, extents are their own type of 
                        # object: SpatExtent

# So to crop NLCD to some area around Lake Pleasant we just need to specify the four numbers that will comprise our spatial extent.
# It might be helpful here to list them out as their own objects and then combine them to create a SpatExtent

# defining the four corners of our Lake Pleasant area
x_min <- 371141
x_max <- 396700
y_min <- 3740971
y_max <- 3762533

# combining these values into a list that terra::ext() will understand
lake_pleasant_extent <- c(x_min,x_max,y_min,y_max)

extent <- ext(lake_pleasant_extent)

# Since SpatExtent is a spatial object, we can plot it.

plot(extent)

# That doesn't tell us much, because it's just an area of space. So lets plot NLCD and add our new SpatExtent to that plot

plot(NLCD, type = "classes")
plot(extent,add=T) # add the extent to our plot and we can see it on our map

# Now that we have our spatial extent of the area we would like to crop lets use the crop() function to do just that

NLCD_crop <- crop(NLCD,extent) # crop NLCD to lake pleasant extent

plot(NLCD_crop, type = "classes")

# Reclassify ------ 
# Useful when we're interested in a smaller area, say a forested area and we are only interested in everything that is forest compared to everything that is not. So forested or not forested

# We often want to reclassify our rasters to group certain values together (e.g., convert a continuous surface into a multi-level categorical surface or recombine categories). This is particularly useful for categorical land cover surfaces.

# To use the classify function in terra, we first need to create a matrix that tells R what original values should be converted into which new values.
# For categorical land cover maps we can use a two-column matrix specifying the "is" and "becomes" values.

# Lets say we only want a layer showing forest land covers.
# We can create a binary raster where 1 represents all three of the NLCD forest classes and 0 represents everything else.

forest_matrix <- matrix(c(11,21,22,23,24,31,41,42,43,52,71,81,82,90,95,
                          0,0,0,0,0,0,1,1,1,0,0,0,0,0,0),
                        ncol = 2)
forest_matrix
# col1 is what our values currently are, col2 is what we want these to be reclassified as.

# When we look at this matrix we see that only values 41, 42, and 43 will
# become 1's while all other values will become 0's

FOREST <- classify(NLCD, forest_matrix) # applying our 
FOREST

# How much forest land cover is around Maricopa County?
plot(FOREST) # lol very little

# What happens when you run this:
names(FOREST) # output is "Red"

# Why do we see "Red" here?
# Long story, but if you look inside the NLCD raster files you download, you will see that they include values for Red, Blue, Green, Opacity, and NLCD_Land (land cover values).
# For some reason, when you bring a NLCD layer into R through terra, the SpatRaster values are those for the Red band but when you plot it, you will see the original land cover values. I've modified the values so they are the original land cover values but terra still thinks those are values from the "Red" band.
# We can change the name for these values very easily by:

names(FOREST) <- "Forest" # change name to more accurate label

# We can confirm that our new FOREST raster only includes 0's and 1's by
# using terra's unique() function. Because base R has its own unique()
# function, we can use terra:: to tell R to use the unique() function 
# within terra

terra::unique(FOREST)

# Finally, terra has a function that will calculate the number of pixels with a given unique value: freq(). I wouldn't run this function on a raster that is intrinsically continuous (like a digital elevation model)
# but for categorial land cover maps this works very well.

freq(FOREST) # counts the number of pixels assoc. with 0, 1; here not-forest and forest

# We can also calculate various summary statistics for our entire raster

# Calculates the mean value of the raster
global(FOREST, fun = "mean") # the proportion of forest in our area of interest
# Calculates the standard deviation of the raster
global(FOREST, fun = "sd")
# Calculates the minimum and maximum raster values
global(FOREST, fun = "range")
# Summarizes the raster values at different quantiles
terra::summary(FOREST) 

# Euclidean distance to a feature -------
# examples: distance to road, distance to water, etc.
# euclidian ditance - straight line distance, does not take into account topography.

# In our habitat analyses we are often interested in using distance to a feature as a covariate.
# This might be distance to nearest water, distance to forest edge, or distance to road.
# These distances are invariably measured as Euclidean (straight line) distances, which may or may not be appropriate, but we can easily measure such distances using terra.

# Let's measure Euclidean distance to nearest water using our cropped NLCD raster
# around Lake Pleasant. S

# To use the distance() function in terra we need to reclassify our raster further.
# Specifically, distance() measures the distance, for all pixels that are NA in the raster to the nearest cell that is not NA.
# So if we want to measure the distance to the nearest water we need to reclassify our raster so that water pixels are one and all other pixels are NA.

# This matrix uses a different format. Instead of listing out every value of NLCD
# and what each value should be reclassified to, we can provide a range of values
# and what values within each range should become.

water_matrix <- matrix(c(0,11,
                         11,95,
                         1,NA),
                       ncol = 3)
water_matrix

WATER <- classify(NLCD_crop, water_matrix) # reclassify our raster to show what is water and what is not water.

plot(WATER, col="dodgerblue")

# We only see pixels with values of 1 appear on our plot. To verify that the rest
# of our raster is NA we can use the plotNA= argument in the plot() function
# to plot all NA pixels as a certain color. 

plot(WATER,
     col="dodgerblue",
     colNA="grey") # specify color of pixels that are NA

freq(WATER)

# Now we can run the distance() function. Here we will use the system.time() function
# to see how long this will take. The system.time() function works by placing an
# entire function within the parentheses which allows system.time() to print the
# time taken to run the function to the console.

system.time(  # this is just here to tell us how long it takes to run this line of code
  water_dist <- terra::distance(WATER)) # calculate how far each pixel is from what we have defined as water

plot(water_dist)
# We can also overlay our reclassified WATER raster
plot(WATER,add=T,col="dodgerblue")

# Creating edges ------
# example, we want to see distance from forest edge for nesting songbirds, b/c greater exposure to predation closer to forest edge 

# Edge environments, or ecotones, play a very important role in wildlife and
# community ecology and are often worth considering in wildlife habitat analyses.
# We can use R to create a binary "edge" raster where 1 represents an edge pixel
# and 0 represents all other pixels.

# We can use the boundaries() function but be warned that this function can take
# a while to run. It took 8.6 minutes to run this function on our FOREST raster,
# and there isn't much forest in Maricopa County!
# So lets crop a smaller raster to demonstrate how this function works.

FOREST_crop <- crop(FOREST, # Our raster to crop
                    ext(c(423649,424285,3773796,3774313))) # Our SpatExtent

# Since we will be plotting a binary raster (only two values, 0 or 1), we can
# manually specify the colors to plot for each value

plot(FOREST_crop,
     col=c("white","darkgreen"))

# The default arguments for boundaries() calculate the boundary between non-NA and NA pixels.
# This works well in the case of a binary raster, we just need to convert our 0's (non-forest) pixels to NA and then boundaries() will give us the edge pixels of our remaining pixels with values of 1 (i.e., our forest pixels)

# We can easily do this by indexing, that is:
    # identifying the pixels that meet a certain condition (in this case, pixels that are zero) and then reassigning their values.

# convert everything that is not LC of interest to NA
FOREST_crop[FOREST_crop==0] <- NA 
plot(FOREST_crop,colNA="grey",
     col=c("white","darkgreen"))

# use boundaries() to look at the pixels that have values, and find what is on the edge
forest_edge <- terra::boundaries(FOREST_crop,
                                 classes=F,     # won't get into 
                                 inner=T,       # these for now
                                 directions=8)
plot(forest_edge,colNA="grey",
     col=c("darkgreen",      # pixels that are not edge
           "lightgreen"))    # pixels that are edge

# If we plot the SpatRaster returned by boundaries() we see that we have our 1's
# representing the edge pixels but we also have some 0 pixel values in addition
# to our NA pixels. These 0 pixels represent pixels that were forest (where 1's)
# in our original FOREST_crop raster but are not edge pixels.

# At this point we probably want to convert all NA pixels back to 0's since we
# probably will want a binary edge raster (i.e., 1 = forest edge, 0 = everything else)
# for subsequent analyses. We can use our same indexing approach, this time with
# the is.na() function which asks whether or not each pixel is NA.

# does the same thing
forest_edge[is.na(forest_edge)] <- 0
plot(forest_edge,colNA="grey",
     col=c("lightgreen","darkgreen"))

# Creating layers ---------

# Within terra we can combine multiple rasters of identical extents and resolutions into a single multi-layered SpatRaster object.
# This is often useful when making model-based predictions with our rasters.

# Lets read in a GeoTiff file (.tif) for a digital elevation model (DEM).
# To combine multiple rasters into a single multi-layered SpatRaster object the rasters must be identical in:
    # dimension
    # extent
    # resolution
# I ensured that our NLCD and our DEM rasters would line up by first creating them in ArcPro. You could do this using the terra package in R but that is beyond the scope of this lab.

# create a raster of our digital elevation model
DEM <- rast("week_1/lab/DEM_Maricopa.tif") 

# Creating a multi-layered SpatRaster is as simple as combining the rasters you want layered
layers <- c(FOREST,DEM)
layers
plot(layers)

# Writing rasters -------------

# Saving rasters created using terra to disk is very easy using writeRaster() 

# defining file path and file name
output_raster_path <- "week_1/lab/outputs/FOREST_and_DEM.tif"

writeRaster(forest_edge,        # The raster to save
            output_raster_path, # The file path for our saved raster
            overwrite = T)      # Tells R to over-write or copy over any previous versions of the file. 

file.exists("week_1/lab/outputs/FOREST_and_DEM.tif")
