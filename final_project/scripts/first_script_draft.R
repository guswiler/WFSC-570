library(ctmm)

# Loading data from movebank
meso <- as.telemetry("prugh_movebank_data.csv")

names(meso)

### will need to split by species and site at some point prior to working with data ####


# plot data with list-sorted rainbow of colors
COL <- color(meso, by='individual')
plot(meso, col = COL)

#! center the projection on the geometric median of the data
projection(meso) <- median(meso)

# now North=up, which is fine for this dataset
plot(meso,col=COL,main="Azimuthal-equidistant projection")
compass()

#select a single individual and plot the data by time
DATA <- meso$MVBOB90M

plot(DATA)

# color by time
COL <- color(DATA,by='time')
plot(DATA,col=COL)


#! calculate a variogram object (named SVF) from the telemetry object
SVF <- variogram(DATA)
plot(SVF,main="Variogram")

GUESS <- ctmm.guess(DATA,interactive=FALSE)
# fit a bunch of models, tell me what models are being fit, return all models, and use all but one CPU core
FITS <- ctmm.select(DATA,GUESS,trace=3,verbose=TRUE,cores=-1)


summary(FITS)

summary(FITS$`OUF anisotropic`)


#### The following code chunk was written prior to attempting analyses with ctmm, I will come back to this is needed. ####
# 
# library(terra)
# library(tidyverse)
#
# ## Pull data into R and separate by study site
# 
# # read in GPS data as df
# data_gps <- read_csv("prugh_movebank_data.csv") # gps locations
# data_ref <- read_csv("prugh_movebank_ref.csv")  # collared individual ref data
# 
# # remove irrelevant information
# data_ref <- data_ref %>% 
#   select("study-site","animal-id",
#          "animal-sex","animal-life-stage","animal-mass",
#          "animal-mortality-type","animal-death-comments",
#          "deployment-end-type","deployment-end-comments") %>% 
#   rename("individual-local-identifier" = "animal-id")
# 
# # join reference data with GPS data
# data_gps <- data_gps %>% 
#   left_join(data_ref, by = "individual-local-identifier")
# 
# # separate into two study sites
# data_ok <- data_gps %>% 
#   filter(`study-site` == "Okanogan")
# 
# data_ne <- data_gps %>% 
#   filter(`study-site` == "Northeast")
# 
# 
# 
# ## Create SpatVector objects
# ok_vect <- vect(data_ok,                                  # df to create vector from
#                 geom = c("location-long","location-lat"), # define cols w/ x/y coords
#                 crs = "epsg:4269")                        # define coord. ref. sys.
# ne_vect <- vect(data_ne,                              
#                 geom = c("location-long","location-lat"), 
#                 crs = "epsg:4269")
# 
# plot(ok_vect)
# plot(ne_vect)