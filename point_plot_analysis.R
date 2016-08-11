# *************************************************************************************************************************
# This analysis is part of the LINZ wilding project
# This programme will plot the point cloud sampling examples and extract point clouds per canopy segment
# J.Dash
# Jonathan.dash@scionresearch.com
# **************************************************************************************************************************

library(sp)
library(rLiDAR)
library(raster)
library(rgeos)
library(rgdal)

# #project setup
setwd('Q:\\Forest Industry Informatics\\Projects\\LINZ')
output.dir<-'.\\outputs\\'
proj.nztm<-"+proj=tmerc +lat_0=0 +lon_0=173 +k=0.9996 +x_0=1600000 +y_0=10000000 +ellps=GRS80 +units=m +no_defs"
shape.dir<-'Q:\\Forest Industry Informatics\\Projects\\LINZ\\Data\\plot_shapes\\' #Set shapefile directory
las.dir<-'Z:\\Geraldine_Lidar\\10_Classified_Normalised_Wildings\\coloured_wilding_classified_clipped\\'
las.dir.10<-'Z:\\Geraldine_Lidar\\Thinned_Data\\thin_10p\\10_Classified_Normalised_Wildings\\coloured_wilding_classified_clipped\\'
las.dir.5<-'Z:\\Geraldine_Lidar\\Thinned_Data\\thin_5p\\10_Classified_Normalised_Wildings\\coloured_wilding_classified_clipped\\'
las.dir.2<-'Z:\\Geraldine_Lidar\\Thinned_Data\\thin_2p\\10_Classified_Normalised_Wildings\\coloured_wilding_classified_clipped\\'
las.dir.1<-'Z:\\Geraldine_Lidar\\Thinned_Data\\thin_1p\\10_Classified_Normalised_Wildings\\coloured_wilding_classified_clipped\\'

