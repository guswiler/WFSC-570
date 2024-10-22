base_dir <- paste0("C:/Users/jbauder/Box/Bauder_Coop_Lab/UA_Teaching/",
                   "WFSC 570 Habitat Analysis 3cr/Labs/RSFs/")
setwd(base_dir)

library(terra)

telem_data <- read.csv("fisher_telemetry_data_df.csv")

head(telem_data)
str(telem_data)

plot(telem_data[,c("x_","y_")])

fisher_LC <- rast('Fisher_land_cover_rsmpld.tif')
plot(fisher_LC,colNA="blue")

elevation <- rast("Fisher_elevation.tif")
plot(elevation)
points(telem_data[,c("x_","y_")])

popden <- rast("Fisher_population_density_rsmpld.tif")
plot(popden,colNA="blue")
points(telem_data[,c("x_","y_")],
       pch = 21, bg = "lightgrey")



# Create a forest edge raster for Lupe ---------------

For_Prep <- fisher_LC$Forest
For_Prep[For_Prep==0] <- NA
plot(For_Prep,colNA="lightblue",
     col=c("white","green"))

system.time(forest_edge <- terra::boundaries(For_Prep,classes=F,inner=T,directions=8))
forest_edge[is.na(forest_edge)] <- 0
plot(forest_edge,colNA="lightblue",
     col=c("white","green"))

writeRaster(forest_edge,
            "Fisher_forest_edge_rsmpld.tif",
            overwrite=T)

used_vect <- vect(data.frame(x=telem_data$x_,y=telem_data$y_),
                  geom = c("x","y"),
                  crs = crs(elevation))
used_vect$y <-1 
used_vect$w <- 1
used_vect$X <- geom(used_vect)[,3]
used_vect$Y <- geom(used_vect)[,4]
fisher_MCP <- readRDS("Fisher_MCP_home_range_SpatVector.rds")
class(fisher_MCP)

plot(forest_edge,col=c("white","green"))
plot(fisher_MCP,add=T)

system.time(avail_vect <- spatSample(fisher_MCP, 
                                     size = nrow(telem_data)*100,
                                     method="random"))
avail_vect$y <-0 
avail_vect$w <- 5000
avail_vect$X <- geom(avail_vect)[,3]
avail_vect$Y <- geom(avail_vect)[,4]

all_pts_vect <- rbind(used_vect,avail_vect)
head(all_pts_vect)
tail(all_pts_vect)

# Extract values -------------

grass <- fisher_LC$Grassland
wetland <- fisher_LC$Wetland

all_pts_vect$pt_elev <- extract(elevation, all_pts_vect)[,2]

buffers <- seq(100,2000,by=100)

for(i in 1:length(buffers)){
  
  buff_i <- buffers[i]
  
  cat("Starting buffer",buff_i,"\n")
  system.time(forest_i <- focal(forest_edge, w = focalMat(forest_edge, 
                                                     d = buff_i, 
                                                     type=c('circle')), 
                                fun = "mean"))
  plot(forest_i,colNA="grey")
  forest_i_vals <- extract(forest_i,all_pts_vect)
  colnames(forest_i_vals)[2] <- paste0("ForEdge_",buff_i)
  all_pts_vect$TMP <- forest_i_vals[,2]
  names(all_pts_vect)[which(names(all_pts_vect)=="TMP")]<- paste0("ForEdge_",buff_i)
  
  system.time(elev_i <- focal(elevation, w = focalMat(elevation, 
                                                   d = buff_i, 
                                                   type=c('circle')), 
                               fun = "mean"))
  plot(elev_i,colNA="grey")
  elev_i_vals <- extract(elev_i,all_pts_vect)
  colnames(elev_i_vals)[2] <- paste0("Elev_",buff_i)
  all_pts_vect$TMP <- elev_i_vals[,2]
  names(all_pts_vect)[which(names(all_pts_vect)=="TMP")]<- paste0("Elev_",buff_i)
  
  system.time(grass_i <- focal(grass, w = focalMat(grass, 
                                                   d = buff_i, 
                                                   type=c('circle')), 
                               fun = "mean"))
  plot(grass_i,colNA="grey")
  grass_i_vals <- extract(grass_i,all_pts_vect)
  colnames(grass_i_vals)[2] <- paste0("Grass_",buff_i)
  all_pts_vect$TMP <- grass_i_vals[,2]
  names(all_pts_vect)[which(names(all_pts_vect)=="TMP")]<- paste0("Grass_",buff_i)
  
  system.time(wet_i <- focal(wetland, w = focalMat(wetland, 
                                                   d = buff_i, 
                                                   type=c('circle')), 
                               fun = "mean"))
  plot(wet_i,colNA="grey")
  wet_i_vals <- extract(wet_i,all_pts_vect)
  colnames(wet_i_vals)[2] <- paste0("Wet_",buff_i)
  all_pts_vect$TMP <- wet_i_vals[,2]
  names(all_pts_vect)[which(names(all_pts_vect)=="TMP")]<- paste0("Wet_",buff_i)
  
}

head(all_pts_vect)

all_pts_vect$PopDen <- extract(popden, all_pts_vect)[,2]
all_LC_values <- extract(fisher_LC, all_pts_vect)
all_pts_vect$Forest <- all_LC_values$Forest
all_pts_vect$Grassland <- all_LC_values$Grassland
all_pts_vect$Wetland <- all_LC_values$Wetland

head(all_pts_vect)

saveRDS(all_pts_vect,"All_Lupe_data_multiscale.rds")

test <- as.data.frame(all_pts_vect)
test <- test[,-(which(colnames(test)%in%c("id","area"))),]
TPI <- test$pt_elev - test[,grep("Elev_",colnames(test))]

colnames(TPI) <- paste0("TPI_",buffers)

test <- cbind(test,TPI)
saveRDS(test,"All_Lupe_data_multiscale_df.rds")
write.csv(test,"All_Lupe_data_multiscale_df.csv",row.names = F)

# Calculate scales of effect

covs <- c("ForEdge","Elev","TPI","Grass","Wet")
buffers <- seq(100,2000,by=100)

col_names <- c("Covariate","Scale","AIC","Delta_AIC","AICw")
scales <- as.data.frame(matrix(NA, nrow = length(covs)*length(buffers),
                                  ncol = length(col_names)))
colnames(scales) <- col_names
i.row <- 1

for(i in 1:length(covs)){
  cov_i <- covs[i]
  cat("Starting",cov_i,"\n")
  data_i <- test[,c("y","w",paste0(cov_i,"_",buffers))]
  cov_cols <- paste0(cov_i,"_",buffers)
  
  for(j in cov_cols){
    tmp_col <- j
    tmp_data <- data_i[,c("y","w",tmp_col)]
    tmp_model <- glm(tmp_data[,1]~tmp_data[,3],weights=tmp_data[,2],family=binomial)
    scales$Covariate[i.row] <- cov_i
    scales$AIC[i.row] <- AIC(tmp_model)
    scales$Scale[i.row] <- as.numeric(strsplit(j,"_")[[1]][2])
    cat("Finished scale",as.numeric(strsplit(j,"_")[[1]][2]),"\n")
    i.row <- i.row + 1
  }
  min_i <- min(scales$AIC[which(scales$Covariate==cov_i)])
  delta_i <- scales$AIC[which(scales$Covariate==cov_i)] - min_i
  w_i <- Weights(scales$AIC[which(scales$Covariate==cov_i)])
  scales$AICw[which(scales$Covariate==cov_i)] <- w_i
  scales$Delta_AIC[which(scales$Covariate==cov_i)] <- delta_i
  
}

write.csv(scales,"scale_data_for_Lupe.csv",row.names = F)

library(ggplot2)

scale_plot <- ggplot(data=scales,
                       aes(x=Scale,y=Delta_AIC))+
  geom_hline(yintercept = 0) +
  geom_line(color="black")+
  geom_point(color="black")+
  facet_wrap(~Covariate)+
  theme_bw()
scale_plot

msRSF_global <- glm(y ~ scale(PopDen) + 
                      scale(Wet_100) + scale(Grass_100) + 
                      scale(ForEdge_800) +
                      scale(TPI_500), 
                        data = test, weight = w,
                        family = binomial(link = "logit"))
summary(msRSF_global)
