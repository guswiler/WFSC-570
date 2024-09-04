rm(list=ls()) # clean workspace.  Caution!!!!

# Lab 2: Introduction to spatial data in R: vectors

# In previous labs, we discussed how to work with rasters in R. Today, we will begin discussing the second type of spatial data: vectors. 
# Vectors are spatial data whose geometry is defined by one or more pairs of spatial locations (e.g., northing and easting in UTMs or longitude and latitude in WGS84).
# Vectors include the shapefiles we may be familiar with from other GIS classes including points, lines, and polygons. 
# Fortunately, the R package terra allows us to work with vector data.

# Much of the resources in this lab come from R resources for spatial analyses available at https://rspatial.org/index.html

# Let's start by loading the terra package. If we scroll further down the
# help page for terra we'll see a list of functions for dealing with vector
# data which terra calls SpatVector objects (in contrast to the SpatRaster
# objects we used for raster data).

library(terra)

# Let's start by creating a SpatVector from scratch using the vect function
?vect

# As with rasters, there are many different ways to create a SpatVector object.
# However, in wildlife habitat analyses, we will most commonly be working with:
    # point data from observations of species or individuals
    # surveys at specific sites
    # or animal tracking data.
# These data will already be in a text file, an Excel spreadsheet, or some other format that we can easily read into R and convert into a SpatVector.

# For this lab, we will use data that came from a radio-tracked eastern indigo snake (Becky) near Cape Canaveral, Florida, that was located about once per week from 1998-2001 (yes, these are old data!).
# The data are stored in a comma-separated value file or CSV file. CSV files are text files used to store structured data, like you would see in an Excel file. It is partly personal preference but I like to keep data that I bring into R stored in CSV files.

# If you have Excel on your computer, open the CSV file in Excel and take a look at it.

# Lets read in Becky's data as a CSV file using the read.csv() function. We will first specify the CSV file's path as a separate object.

becky_data <- read.csv("lab/becky.csv")

# We can see how our new data object (becky_data) looks by going to the environment tab, or we can look at it from the console using the following functions.

# When reading in new data give it a look with these functions to make sure it shows up the way it should.

# What class is our new object "becky_data"
class(becky_data)   # data frame
# Display the first six lines (by default) of the data
head(becky_data)    
# Display the last six lines (by default) of the data
tail(becky_data)  
# View the structure of our data
str(becky_data)     
# Summaries of each column
summary(becky_data) 

# For today's lab, we are most interested in the x and the y columns, which are the x/y UTM coordinates in NAD83 Zone 17N.
# Can you find the EPSG code for this coordinate reference system at https://spatialreference.org?

# Since we have x/y coordinates, we can plot each point using the base R plot() function.
# We just need to provide a 2-column data frame or matrix with the x and y coordinates. 
# We can do this using indexing. Lets go over indexing a bit more in detail here because it is a very useful R skill to have.

# Indexing is done with square brackets ([,]).
# For 2-dimensional objects, like data frames or matrices, we use a pair of square brackets with a comma. 
# Values or objects to the left of the comma refer to rows and values or objects to the right of the comma refer to columns.

# We can illustrate this with our becky_data data frame.

# First row
becky_data[1,]

# 50th row
becky_data[50,]

# First column
becky_data[,1]

# Fourth column
becky_data[,4]

# We can do indexing with more than just row or column numbers. We can use column names:

becky_data[,"date"]

becky_data[,c("date","dist")]

# What column names would we use for indexing if we only wanted to pull out the x/y coordinates? 

becky_data[,c('x','y')] # we want all coords. so specify x and y indexed cols

# Once we have our x/y coordinates, we can plot them using R's base plotting function.

plot(becky_data[,c('x','y')])

# Now lets convert our data frame with all of Becky's attribute information into a SpatVector object.
# There are many different options for creating SpatVector objects depending on the information and object type you already have. We have a data frame so look at:
# ## S4 method for signature 'data.frame'
#     vect(x, geom=c("lon", "lat"), crs="", keepgeom=FALSE)

# There are three main arguments: 
  # 1. x is our data frame containing all the attribute data we wish to include in our SpatVector object (here, x = becky_data)
  # 2. geom is the geometry data, that is, our spatial coordinate data. In our case, they are the x/y columns with the UTM coordinates although you can see that the function's default is to look for columns named "lat" and "long."
  # 3. Finally, we have crs or coordinate reference system.
# We can ignore the keepgeom= argument. 

becky_vect <- vect(x = becky_data, # df to create vector from
              geom = c("x","y"),   # geometry: telling the fn what cols give our x/y coords
              crs = "epsg:26917")  # defining coord. ref. sys.
becky_vect

# We can plot a SpatVector object just using plot() which will use terra's plot function.

plot(becky_vect)

# Getting our point location data into R is only one step in using R to extract and format the data we will use in our habitat analyses. 
# One of the most important steps is collecting the environmental data at each of our point locations. 

# terra once again allows us to easily accomplish this using the extract() function. 
# Looking at ?extract, we see that this function will extract values from a SpatRaster for a set of locations.

?extract

# Lets read in some rasters from Becky's study area that represent different landscape features.
# We will talk more about buffers when we talk about scale but for now you can think about these pixel values as representing the amount of undeveloped upland land cover around that pixel.


# This raster represents the proportion of undeveloped upland land cover within a 100-m radius buffer
upland <- rast("lab/becky_Upland_100.tif")
upland

plot(upland,             # raster
     colNA="dodgerblue") # NA pixel color
# on this scale: 1 = completely undeveloped, 0 = completely developed
# The NA values in this raster represent open water land cover.


# We can plot Becky's points a couple different ways:
    # We could use the points() function and the x/y columns in the data frame
points(becky_data[,c("x","y")],
       pch = 21,
       bg = "lightgreen")
    # or we could use terra's plot() function to add our new SpatVector of Becky's points.
plot(becky_vect,
     add = T, 
     pch = 21,         # Point shape (a hollow circle)
     bg = "lightgrey") # Point background color


# We can extract the pixel values directly at each point. 
upland_values <- extract(upland, becky_vect)
head(upland_values)   # we now have a df of the values from the raster at each of these points.
                      # output is the pixel value for the pixel in which your point falls


# By default, the extract() function returns a data frame with two columns:
    # one is point ID (just a sequence from one to the number of points) 
    # and the other is our raster values at each point (the value of the pixel each point falls within)

# extract() gives us a few different options for omitting or including different information.
# For example:
head(extract(upland,
             becky_vect,
             ID = F,     # we can omit the ID column with ID = F
             xy = T))    # and include each points x/y coordinates with xy = T


# Just for fun, lets create a histogram of our extracted values
hist(upland_values$becky_Upland_100,  # data
     main = "Becky's Association with Undeveloped Upland Land Cover",
     xlab = "Proportion of Land Cover Type")


# We can now append these values directly to our SpatVector "becky_vect" by creating a new column
becky_vect$Upland_100 <- upland_values$becky_Upland_100
head(becky_vect) # see these values are now added to the original vector we initially created


# Within extract() we can accomplish the same thing by using bind = TRUE
# Quickest way to accomplish this, but you can't specify your own column name
extract(upland,         # upland raster
        becky_vect,     # becky vector
        bind = T)       # bind the raster values to the vector at each point


# Remember that terra allows us to combine multiple rasters (of identical extent and dimensions) into a multi-layer SpatRaster object.
# Lets do this with three other land cover layers from Becky's study area: urban,
# wetland, and the SD of Winter NDVI.
# Rather than reading in three additional SpatRaster objects we can create a multi-layer SpatRaster object directly by supplying all the file paths. 


land_cover <- rast(c("lab/becky_Upland_100.tif",      # Undeveloped upland land cover
                     "lab/becky_Urban_100.tif",       # Urban land cover
                     "lab/becky_Wetlands_100.tif",    # Wetland land cover
                     "lab/becky_Win_SDNDVI_100.tif")) # SD of winter NDVI within a 100-m radius buffer

land_cover
plot(land_cover, colNA = "dodgerblue")


# We can then provide this multi-layer SpatRaster to the extract function and it will extract the values of all four layers at each of our points.

LC_values <- extract(land_cover,    # our layered raster
                     becky_vect)    # vector
head(LC_values)

# We can then treat our SpatVector object just like a df and cbind our SpatVector object with our data frame of newly extracted land cover values.
# Lets set bind=T so we can append these values directly into our SpatVector object "becky_vect"

becky_vect <- extract(land_cover, # 4 layered SpatRaster
                      becky_vect, # SpatVector
                      bind = T)   # bind the raster values to the SpatVector
becky_vect
head(becky_vect)

# We can plot Becky's points and include one of our land cover columns to color each point by a binned value of that column

plot(becky_vect,           # vector
     "becky_Wetlands_100") # land cover raster to bin the points by



# looking at just single points isn't fully informative, thinking about animal movement, it is experiencing the landscape outside of the pixel point that you aren't capturing.
# so BUFFERS!


# What do you do if your landscape or environmental data is not already represented as proportion or amount of some feature within a given buffer but you want to measure that feature within a buffer around each of your points?
# terra has a function called buffer() which will create circular buffers around each point as a new multi-geometry SpatVector polygon (think of this as creating a polygon shapefile).
# These buffers can be used to extract landscape or environmental features around your points.


# buffer() has multiple arguments but here we only need to concern ourselves with two: the SpatVector (or SpatRaster) object we wish to buffer and the width (in map units) of the buffer. Lets create buffers with 100-m radii.

becky_buffers <- buffer(becky_vect,  # vector
                        width = 100) # radius of your buffer
becky_buffers


# Lets plot our buffers to see what they look like.
# When we plot the SpatRaster for the map background, lets change the extent of the plot to "zoom in" to Becky's points.
# We do this by providing a different extent (SpatExtent) within the plot function. We will use the SpatExtent from the SpatVector "becky_vect."

plot(land_cover$becky_Urban_100,  # plot raster of urban landcover and
     ext = ext(becky_vect))       # define extent to only show area that encompasses becky_vect points
plot(becky_vect, add = T)         # add points
plot(becky_buffers, add = T)      # add buffers


# We can now extract SpatRaster values from these buffers.

# NOT WHAT YOU DO
# What happens if we extract raster values from a polygon?
extract(land_cover$becky_Urban_100, # raster
        becky_buffers[1])           # buffer
# when you extract buffers, you're getting every single pixel within those polygons.
# if you want to run analyses, you need to summarize


# We need some way to summarize these values, say, by taking their mean.
# Fortunately, extract() will let us do that. There is an argument fun= for which we can provide a function to summarize those extracted pixel values.
# Lets use the mean here and also include na.rm=T to deal with the few NA pixels you might have noticed.

extract(land_cover$becky_Upland_100, # raster
        becky_buffers,               # buffers
        fun = "mean",                # calc the mean of each buffer 
        na.rm = T)                   # remove NA values

# We still have a lot of values but if you look closely you'll see one mean value for each of Becky's 93 buffered points.
# Because we already have a column in becky_vect named "becky_Upland_100," lets put these mean buffer values into a new object and use that to create a new column within becky_vect.

buffer_values <- extract(land_cover$becky_Upland_100,
                         becky_buffers,
                         fun = "mean",
                         na.rm = T)
buffer_values

# now adding these values to the vector
becky_vect$Upland_100_buffer_mean <- buffer_values$becky_Upland_100

hist(becky_vect$Upland_100_buffer_mean)

# Now that we have some landscape data for Becky's points lets see if the landscape features she used changed between summer and winter seasons.
# There are many ways you could perform such an analysis but lets use a simple approach for now: the t-test, which is a very common test for differences between two means.

# Is Becky's use of wetland land cover different between winter and summer? Before we actually run a t-test maybe we should graphically display these data. A boxplot is a great way to do this, and we can use base R's boxplot() function in formula notation:

# boxplot(y-axis value ~ x-axis value, data)

boxplot(becky_Wetlands_100 # landscape feature
        ~ season,          # seasons to compare (summer v winter)
        becky_vect,        # vector
        col = c("gold", "lightblue2"),
        ylab = "Wetland Cover Association",
        xlab = "Season")

# The t.test() function can use the same formula notation. 
# testing for statistical diff between two group means. Here mean value of wetlands around points in winter vs summer

t.test(becky_Wetlands_100 
       ~ season,
       becky_vect)

# The t-test does make some assumptions that may be more-or-less violated if our data are highly skewed. If we have reason to suspect this, we could always use the non-parametric equivalent of the t-test: the Mann-Whitney U test. 

## Check the SD of groups before using non-parametric tests

wilcox.test(becky_Wetlands_100 ~ season, becky_vect)
