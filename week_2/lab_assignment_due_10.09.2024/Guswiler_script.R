# Lab Assignment 1
# Olivia Guswiler
# 10-09-2024

## To Do:
# Create a SpatVector using the CSV file of site locations and data
# Create at least two SpatRaster objects representing two landscape features that could be used as covariates in an analysis of the habitat associations of at least one species. You get to choose both the landscape features and the species
# Extract values for each landscape feature at some buffer around each site. You may use the same buffer for each feature or a different buffer for each feature
# Create boxplots describing the distributions of each feature at sites where the species was detected and sites where the species was not detected
# Use a t-test (R function t.test()) or a Mann-Whitney test (R function wilcox.test()) to compare the values of each landscape feature between sites where your focal species was and was not detected
#----

library(terra)  # load package into r
