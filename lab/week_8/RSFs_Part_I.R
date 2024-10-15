rm(list=ls()) # clean workspace.  Caution!!!!

# Lab 5: Used-Available Data and Resource Selection Functions

library(terra)

# In this lab we will start looking into a class of wildlife-habitat models
# called resource selection functions or habitat selection functions. These
# are a very popular group of models that is specifically designed to used
# used-available data to examine patterns of wildlife habitat selection.
# Most resource selection functions (RSFs) are fit using some type of 
# telemetry data, either VHF radio telemetry or GPS telemetry. These data
# are treated as used-available data because the telemetry locations give us
# information about where animals have been and therefore what habitat
# features they have used. These used locations are then compared against 
# what is available. Because we have not visited every possible location in 
# our study area to determine if that area was used or not used, we do not
# have used-unused data. Furthermore, over the course of monitoring an
# individual's movements using telemetry, locations that were unused at the 
# beginning of the study may become used at a later point in time. 

# Comparing use to availability lets us make inferences about selection, that
# is, disproportionate use. Features that are used disproportionately to their
# availability can provide insights into the relative importance of different 
# habitat features. It is important to remember the quote from the Manly et al.
# chapter that researchers should "proceed cautiously when using the results
# of selection studies to determine the importance of resources." That being
# said, studies of habitat selection per se can still be very informative.

# The data from this lab, which we will be using for at least two labs, come from
# the Fieberg et al. (2021) paper. These are GPS telemetry data from a fisher
# that were programmed to collect a location about every five minutes. 
# Accompanying these data are a digital elevation model, a raster measuring
# human population density, and land cover rasters. I have reclassified the
# land cover raster into three binary rasters representing forest cover,
# grassland cover, and wetland cover. 

# First, lets read in the data and have a look at it ----

# telemetry
telem_data <- read.csv("lab/week_8/fisher_telemetry_data_df.csv")

head(telem_data)
str(telem_data)

plot(telem_data[,c("x_","y_")]) # indexing to pull out x and y cols

# land cover
fisher_LC <- rast("lab/week_8/Fisher_land_cover.tif")
plot(fisher_LC) # 3 plots because multi-layered
# same resolution, extent, dimensions, but each convey different information
# very forested landscape

# elevation in meters
elevation <- rast("lab/week_8/Fisher_elevation.tif")
plot(elevation)
points(telem_data[,c("x_","y_")],
       cex = 0.2)

# human population density
popden <- rast("lab/week_8/Fisher_population_density.tif")
plot(popden)
points(telem_data[,c("x_","y_")],
       cex = 0.2,
       col = "white")

# Lets now determine habitat use, that is, what are the values of land cover,
# elevation, and human population density that Lupe is using. To do this,
# we need to extract our raster values at each of Lupe's telemetry locations.
# We can use terra's handy extract() function for this. But to use terra's
# extract function we must first create a SpatVector object with which to
# perform the extraction. Lets use our telemetry data to create this and
# pull in the coordinate reference system of one of our rasters.

# need to create SpatVector of locations to use with extract function
used_vect <- vect(data.frame(x=telem_data$x_,y=telem_data$y_),
                  geom = c("x","y"),
                  crs = crs(elevation))

# For now we can just extract the raster values at each of Lupe's locations. 
# Lets create a histogram of each set of extracted data just to see what
# types of environments Lupe is using.

# extracting pixel values at our point location
used_elev <- extract(elevation, used_vect) # extract from elev, using used_vect points
head(used_elev)
hist(used_elev$elevation)
summary(used_elev$elevation)

used_popden <- extract(popden, used_vect)
head(used_popden)
hist(used_popden$popden)
summary(used_popden$popden)

# You may have noticed how our land cover data is presented as a multi-layered
# SpatRaster. We can use this to illustrate one of the advantages of multi-
# layered SpatRasters: extracting across multiple layers all at once.

used_LC <- extract(fisher_LC, used_vect)
head(used_LC)

apply(used_LC[,2:4],2,mean)
summary(used_LC[,2:4]) # summary of only cols 2 through 4



# We see some pretty disparate numbers here? Any ideas why?
    # higher proportion of forest habitat available than other types


# Lets combine all our used data into a single data frame.
used_data <- data.frame(Elevation = used_elev$elevation,
                        PopDen = used_popden$popden,
                        Forest = used_LC$Forest,
                        Grassland = used_LC$Grassland,
                        Wetland = used_LC$Wetland)

head(used_data)



# We now have data for one of two parts of a resource selection function. ----
# The second part is the available data.
# Based on what we have discussed so far what are some ways in which we might measure habitat availability?
    # Compare home range use to what is available in study area

# For now we will measure habitat availability across Lupe's home range. ----

# What order of selection would this be?
    # 3rd order

# minimum convex polygon home range estimate
fisher_MCP <- readRDS("lab/week_8/Fisher_MCP_home_range_SpatVector.rds")
class(fisher_MCP)

plot(elevation)
plot(fisher_MCP,add=T)

# We can use terra's spatSample function to select random points from across
# Lupe's minimum convext polygon (MCP) home range.
# How many random points do we need?
  # It depends but the standard advice is a lot.

# Lets select 10 random points for every used point (so 30040)
system.time(avail_vect <- spatSample(fisher_MCP, 
                                     size = nrow(telem_data)*100,
                                     method="random"))
avail_vect
nrow(avail_vect)

# plotting home range with available and used points
plot(fisher_MCP, lwd = 5, border = "darkblue")
plot(avail_vect, add = T, cex = 0.25, pch = 16)
points(telem_data[,c("x_","y_")], cex = 0.25, pch = 21, bg = "red")



# Now that we have our available points we can extract our available habitat data ----
# the same way we extracted our used habitat data.

avail_elev <- extract(elevation, avail_vect)
head(avail_elev)
hist(avail_elev$elevation)

avail_popden <- extract(popden, avail_vect)
head(avail_popden)
hist(avail_popden$popden)

avail_LC <- extract(fisher_LC, avail_vect)
head(avail_LC)
summary(avail_LC[,2:4])

avail_data <- data.frame(Elevation = avail_elev$elevation,
                         PopDen = avail_popden$popden,
                         Forest = avail_LC$Forest,
                         Grassland = avail_LC$Grassland,
                         Wetland = avail_LC$Wetland)
head(avail_data)

# Now lets combine our used and available data and make some comparisons ----
# between these two datasets to see if we notice any patterns. This is also
# a good time to add a couple extra columns to our used and available
# data frames. One column will represent our response variable. We will call 
# that y and code it 1 = used and 0 = available. We will talk more about
# why these data are NOT BINARY DATA! But for now lets hold questions and
# instead start by illustrate how we fit our resource selection function.

avail_data$y <- 0
used_data$y <- 1

# we are treating response as binary here, but we are not modeling as binary

# The second column we will add will be a column with a value by which to 
# weight each data point. Again, we'll come back to this later but for now
# we will give each used point a weight of 1 and each available point a weight
# of 5000.

avail_data$w <- 5000
used_data$w <- 1

# Now lets combine these data using the rbind() function.

all_data <- rbind(used_data, avail_data)

# looking at number of used and available points
table(all_data$y)

# Lets use box plots to compare elevation and human population density between
# used and available locations. 

boxplot(Elevation ~ y, all_data)
boxplot(PopDen ~ y, all_data)
# Notice any patterns?
    # they are proportional

# To compare usage vs. availability with our categorical land covers we will compare the proportion of points within each category.

# calculate proportions of used and available
apply(used_data[,c("Forest","Grassland","Wetland")],2,mean)
apply(avail_data[,c("Forest","Grassland","Wetland")],2,mean)

# We can quickly calculate some selection ratios too
apply(used_data[,c("Forest","Grassland","Wetland")],2,mean)/apply(avail_data[,c("Forest","Grassland","Wetland")],2,mean)
    #       Forest Grassland   Wetland 
    #    1.0093214 0.2189289 1.2859857 
    # proportional     avoid    select

# Now we can fit our resource selection function.
# We will do this using a binomial GLM but again we ARE NOT MODELING BINOMIAL DATA!
# We will come back to the why behind this but for now I want to illustrate the model fitting process and look at the coefficients. 


RSF_global <- glm(y ~ Elevation + PopDen + 
                    Wetland + Grassland, 
                  data = all_data, 
                  weight = w,
                  family = binomial(link = "logit"))

summary(RSF_global)


# Which covariate appears to have the greater effect size? Is this correct?

# don't forget to zscore standardize our continuous estimates

RSF_globalz <- glm(y ~ scale(Elevation) + scale(PopDen) + Wetland + Grassland, 
                   data = all_data, weight = w,
                   family = binomial(link = "logit"))

summary(RSF_globalz)
#                     Estimate Std. Error  z value Pr(>|z|)    
# (Intercept)        -13.16642    0.01945 -676.780  < 2e-16 ***
#   scale(Elevation)   0.29788    0.01654   18.010  < 2e-16 ***
#   scale(PopDen)     -0.18259    0.02107   -8.667  < 2e-16 ***
#   Wetland            0.23512    0.10848    2.167   0.0302 *  
#   Grassland         -1.45830    0.27799   -5.246 1.56e-07 ***

# Now what can we say about our covariate effect sizes? 
# selecting for higher elevations, avoiding areas of higher human pop density
  # what does a positive slope indicate? selection
  # negative slope? avoidance

# what about categorical covariates? Here forest and wetland
