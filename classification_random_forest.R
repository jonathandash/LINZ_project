# *************************************************************************************************************************
# This analysis is part of the LINZ wilding project
# This programme will fit a random forest for point classification to each combination of lidar + colour + intensity
# THis analysis will guide classification procedures with RF
# Jonathan.dash@scionresearch.com
# **************************************************************************************************************************

rm(list=ls())
gc()
memory.size()

library(sp)
library(rLiDAR)
library(raster)
library(rgeos)
library(rgdal)

# Project setup
# These are locations on the Scion network
setwd('Q:\\Forest Industry Informatics\\Projects\\LINZ')
output.dir<-'.\\outputs\\'
proj.nztm<-"+proj=tmerc +lat_0=0 +lon_0=173 +k=0.9996 +x_0=1600000 +y_0=10000000 +ellps=GRS80 +units=m +no_defs"
shape.dir<-'Q:\\Forest Industry Informatics\\Projects\\LINZ\\Data\\plot_shapes\\' #Set shapefile directory
las.dir<-'Z:\\Geraldine_Lidar\\10_Classified_Normalised_Wildings\\coloured_wilding_classified_clipped\\'
las.dir.10<-'Z:\\Geraldine_Lidar\\Thinned_Data\\thin_10p\\10_Classified_Normalised_Wildings\\coloured_wilding_classified_clipped\\'
las.dir.5<-'Z:\\Geraldine_Lidar\\Thinned_Data\\thin_5p\\10_Classified_Normalised_Wildings\\coloured_wilding_classified_clipped\\'
las.dir.2<-'Z:\\Geraldine_Lidar\\Thinned_Data\\thin_2p\\10_Classified_Normalised_Wildings\\coloured_wilding_classified_clipped\\'
las.dir.1<-'Z:\\Geraldine_Lidar\\Thinned_Data\\thin_1p\\10_Classified_Normalised_Wildings\\coloured_wilding_classified_clipped\\'
canopy.shape.dir<-'Q:\\Forest Industry Informatics\\Projects\\LINZ\\Data\\canopy_shapes\\' #Set canopy shapefile directory



######
#********************************************************************************
# Read all lasfiles into a dataframe and classify with random forest
#********************************************************************************

# read lasfiles
setwd(las.dir)
plist=list.files(las.dir, pattern="las$", full.names=FALSE)
for(i in plist) { assign(unlist(strsplit(i, "[.]"))[1], readLAS(i, short=FALSE)) }


## List all spatial polygons dataframe in environemnt 
ClassFilter <- function(x) inherits(get(x), 'matrix' ) # filter function for 
mats <- Filter( ClassFilter, ls() )

# merge all the points into a datframe
all.points<-data.frame()

for (i in 1:length(mats))
{
  p<-as.data.frame(get(mats[i]))
  p$plot<-mats[i]
  all.points<-rbind(all.points, p)
}

# Make a binomial response variable to use in the classification.
all.points$wld.key<-ifelse(all.points$Classification == 10, 'W', 'N')
all.points$wld.key<-as.factor(all.points$wld.key)
all.points$plot<-as.factor(all.points$plot)
str(all.points)

#####

#***********************************************************************************
# Fit a base level random forest to all.points
#***********************************************************************************

library(ranger) # ranger seems to be the most efficieent implementation of RF for this problem

rf1.rang<-ranger(wld.key~Z, data = all.points, importance = "permutation", write.forest=TRUE)
rf2.rang<-ranger(wld.key~Z + Intensity, data = all.points, importance = "permutation")
rf3.rang<-ranger(wld.key~Z + R, data = all.points, importance = "permutation")
rf4.rang<-ranger(wld.key~Z+Intensity + R, data = all.points, importance = "permutation")
rf5.rang<-ranger(wld.key~Z+Intensity + R + B, data = all.points, importance = "permutation")
rf6.rang<-ranger(wld.key~Z+Intensity + R + G + B, data = all.points, importance = "permutation")
rf7.rang<-ranger(wld.key~ R + G + B, data = all.points, importance = "permutation")
rf8.rang<-ranger(wld.key~ Z + R + G + B, data = all.points, importance = "permutation")




