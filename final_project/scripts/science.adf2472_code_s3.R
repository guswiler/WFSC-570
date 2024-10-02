##Code written by L. Prugh to calculate cause-specific mortality rates of 
  #radiocollared coyotes and bobcats in Washington, and to create Fig. 4.
##Accompanies the paper: LR Prugh, CX Cunningham, RM Windell, BN Kertson, 
  #TR Ganz, SL Walker, AJ Wirsing. The paradox of the lethal human shield 
  #outside protected areas. 

library(etm) #for running cumulative incidence function (CIF) models
library(survival) #for splitting dataset into animal-years
library(dplyr) #for manipulating dataframes
library(ggplot2) #for plotting data (Fig. 4)
library(cowplot) #for adding silhouettes 
library(magick) #for adding silhouettes 

##Dataset with 1 row per individual
MesoSurv<-read.csv("MesoFatesS1.csv",header=T) #this is the Data S1 file
MesoSurv$Fate<-recode(MesoSurv$EndFate,"Unknown"=0,"Dead"=1,"Alive"=0)

##Remove individuals with unknown cause of death 
MesoSurv<-MesoSurv[MesoSurv$Cause != "Unknown",]  

#Create "entry" column for CIF model and make sure Cause is a factor
MesoSurv$Cause <- as.factor(paste(MesoSurv$Cause))
MesoSurv$Entry<-0

##Create cutpoint at yearly timestep to have 1 row per animal-year
##The function automatically labels Cause in newly formed rows as "censor" since
  #the animals survived the full year.
##Strangely, it also replaces the first Cause (alphabetically) in pre-existing 
  #rows with "censor". So "Censor" needs to be the first Cause alphabetically 
  #in the MesoSurv file
Surv2 <- survSplit(Surv(DaysAlive, Cause) ~., MesoSurv,
                   cut=(c(365.25,365.25*2)), episode ="timegroup")

##Recode causes for etm package. Note the "censor" category was created automatically
  #by the survSplit function and is now lower case
Surv2$CauseCode<-recode(Surv2$Cause,"censor"=0,"Human"=1,"Predator"=2, "Other"=3)

#Calculate days alive for each animal-year
Surv2$DaysYr<-Surv2$DaysAlive - Surv2$tstart

#Create subsets for each species
Bob<-subset(Surv2, Species == "Bobcat")
Coy<-subset(Surv2, Species == "Coyote")

##Model estimating cause-specific mortality for bobcats, average annual
cif.bob<-etmCIF(survival::Surv(Entry, DaysYr, CauseCode != 0) ~ 1, data = Bob,
etype = CauseCode, failcode = c(1:3))
cif.bob
summary(cif.bob)
plot(cif.bob, ylab = "Mortality rate", xlab = "Time (days)", 
     curvlab = c("human", "large carnivore", "other"), ylim = c(0, 0.4), lwd = 2)

##Model estimating cause-specific mortality for coyotes, average annual
cif.coy<-etmCIF(survival::Surv(Entry, DaysYr, CauseCode != 0) ~ 1, data = Coy,
                etype = CauseCode, failcode = c(1:3))
cif.coy
summary(cif.coy)
plot(cif.coy, ylab = "Mortality rate", xlab = "Time (days)", 
	curvlab = c("human", "large carnivore", "other"), ylim = c(0, 0.4), lwd = 2)

##Model estimating cause-specific mortality for bobcats, by study area
##Used to create Table S4
bob.area<-etmCIF(survival::Surv(Entry, DaysYr, CauseCode != 0) ~ StudyArea, data = Bob,
                etype = CauseCode, failcode = c(1:3))
bob.area
summary(bob.area)

##Model estimating cause-specific mortality for coyotes, by study area
  ##because no coyotes died in the "Other" category in the Northeast, we need to 
  #run separate models for each study area or you get an error
Coy.NE<-subset(Coy, StudyArea == "Northeast") #only human and predator morts
Coy.OK<-subset(Coy, StudyArea == "Okanogan") #All 3 CODs

NEcoy.mort<-etmCIF(survival::Surv(Entry, DaysYr, CauseCode != 0) ~ 1, data = Coy.NE,
            etype = CauseCode, failcode = c(1:2))
OKcoy.mort<-etmCIF(survival::Surv(Entry, DaysYr, CauseCode != 0) ~ 1, data = Coy.OK,
            etype = CauseCode, failcode = c(1:3))
NEcoy.mort
OKcoy.mort

#******************************************************************
##The following code is for creating Fig. 4 (paired barplot of annual mortality rates)

##create dataframe with values from cif.coy and cif.bob models above
cause <- c("Human", "Large carnivore", "Other", "Human", "Large carnivore", "Other")
species <- c("Bobcat" , "Bobcat" , "Bobcat" , "Coyote", "Coyote", "Coyote")
mort <-  c(0.24712834, 0.06512321, 0.09283609, 0.30280382, 0.10492857, 0.03927152)
se <- c(0.06595270, 0.03669520, 0.04503728, 0.06819396, 0.04445489, 0.02722964)
meso.mort <- data.frame(cause,species,mort,se)

##Set levels so bars can be changed from the default order
meso.mort$species <- factor(meso.mort$species, levels = c("Coyote", "Bobcat"))
meso.mort$cause <- factor(meso.mort$cause, levels = c("Human","Large carnivore","Other"))

##Barplot of mortality rate by species 
coy_sil <- image_read("coyote-silhouette.png")
bob_sil <- image_read("bobcat-silhouette.png")

theme_set(theme_cowplot())
mort.plot<-ggplot(meso.mort, aes(x=species, y=mort, fill=cause)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymax = mort+se, ymin=mort-se), width=.2,
                position=position_dodge(.9))+
  scale_y_continuous(expand = c(0, 0), limits = c(0,0.4))+
  labs(x="Species", y = "Annual mortality rate", fill = "Cause")+
  scale_fill_manual(values=c('chocolate1','brown','lightskyblue')) +
  theme(legend.position = c(0.7,0.6),
        plot.margin = margin(t = 15, r = 50, b = 5, l = 5, unit = "pt"))

Fig4<- ggdraw() +
  draw_plot(mort.plot) +
  draw_image(coy_sil,  x = -0.18, y = 0.45, scale = 0.18)+
  draw_image(bob_sil,  x = 0.15, y = 0.45, scale = 0.15)

# export pdf
pdf(file = "Fig4.pdf", width = 4.25, height = 4.25) 
Fig4
dev.off()

