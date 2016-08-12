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

#**********************
# Make a figure
#***********************



setwd(las.dir)
plist=list.files(las.dir, pattern="las$", full.names=FALSE)
#for(i in plist) { assign(unlist(strsplit(i, "[.]"))[1], readLAS(i, short=FALSE)) }

library(stringr)
drp.las<-str_sub(plist, 1, -5)

plt<-'X15' ### Choose a plot

for(i in 1:length(drp.las))
{
  plt<-drp.las[i]
# Make a figure for presentaion
setwd(canopy.shape.dir) # Get the canopy shapes

try(cpy<-paste('b.Tr', plt, '.shp', sep=""))

plt.las<-paste(plt, '.las', sep="")

try(cpy.shp<-shapefile(cpy))


#Get full point cloud dataset
setwd(las.dir)
p.all<-readLAS(plt.las, short = FALSE)
p.all<-as.data.frame(p.all)
coordinates(p.all)<-~X+Y #Make the las file a spatial points dataframe

#Get 10 point cloud dataset
setwd(las.dir.10)
p.10<-readLAS(plt.las, short = FALSE)
p.10<-as.data.frame(p.10)
coordinates(p.10)<-~X+Y #Make the las file a spatial points dataframe

#Get 5 point cloud dataset
setwd(las.dir.5)
p.5<-readLAS(plt.las, short = FALSE)
p.5<-as.data.frame(p.5)
coordinates(p.5)<-~X+Y #Make the las file a spatial points dataframe

#Get 2 point cloud dataset
setwd(las.dir.2)
p.2<-readLAS(plt.las, short = FALSE)
p.2<-as.data.frame(p.2)
coordinates(p.2)<-~X+Y #Make the las file a spatial points dataframe

#Get 1 point cloud dataset
setwd(las.dir.1)
p.1<-readLAS(plt.las, short = FALSE)
p.1<-as.data.frame(p.1)
coordinates(p.1)<-~X+Y #Make the las file a spatial points dataframe


tiff(paste('Q:\\Forest Industry Informatics\\Projects\\LINZ\\Reports\\images_for report\\', plt, 'density_comp.tif', sep=""), w=17, h=7, units="cm", res=300)
#cairo_pdf('Q:\\Forest Industry Informatics\\Projects\\LINZ\\Reports\\images_for report\\plot17_density_comp.pdf', width=7.08, height=2.91, family = "arial")
layout(matrix(1:5, nrow = 1))
#par(mar = c(0, 1.5, 0, 0), oma = c(4, 4, 3, 1), tcl = 0.35, mgp = c(2, 0.4, 0))
op <- par(mar = c(0, 0,2,0), oma = c(1,1,1,1), tcl = 0.35, mgp = c(2, 0.4, 0))
plot(p.all, pch=20, cex = 0.1, col="grey", main = expression(21~pl ~ m^2))
plot(cpy.shp, add=T, border = "red")
#axis(2, labels=T)
#axis(1, labels=T)
box()

plot(p.10, pch=20, cex = 0.01, col="grey", main = expression(10~pl ~ m^2))
plot(cpy.shp, add=T, border = "red")
#axis(2, labels = F)
#axis(1, labels = F)
box()

plot(p.5, pch=20, cex = 0.01, col="grey", main = expression(5~pl ~ m^2))
plot(cpy.shp, add=T, border = "red")
#axis(2, labels = F)
#axis(1, labels = F)
box()

plot(p.2, pch=20, cex = 0.01, col="grey", main = expression(2~pl ~ m^2))
plot(cpy.shp, add=T, border = "red")
#axis(2, labels = F)
#axis(1, labels = F)
box()

plot(p.1, pch=20, cex = 0.01, col="grey", main = expression(1~pl ~ m^2))
plot(cpy.shp, add=T, border = "red")
#axis(2, labels = F)
#axis(1, labels = F)
box()
dev.off()
}

