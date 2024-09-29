library(terra)

vect <- read.csv("prugh_movebank_data.csv")
plot(vect)

df <- data.frame(read.csv("prugh_movebank_data.csv"))
head(df)
