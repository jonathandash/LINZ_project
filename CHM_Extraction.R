# *************************************************************************************************************************
# This analysis is part of the LINZ wilding project
# Can we extract voroni polygons from the CHM
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



# Full chm
full.chm<-raster("Z:\\Geraldine_Lidar\\10_Classified_Normalised_Wildings\\wilding_area_full_chm.tif")
plot(full.chm)
str(full.chm)


# read shapefiles
setwd(shape.dir)
slist=list.files(shape.dir, pattern="shp$", full.names=FALSE)
for(i in slist) { assign(unlist(strsplit(i, "[.]"))[1], shapefile(i)) }



## List all spatial polygons dataframe in environemnt 
ClassFilter <- function(x) inherits(get(x), 'SpatialPolygonsDataFrame' ) # filter function for 
Objs <- Filter( ClassFilter, ls() )


# Clip all plots 
for (j in 1:length(Objs))
{
  try(assign(paste('clipped.',Objs[j], sep=""), crop(full.chm, get(Objs[j]))))
}


# #List all objects in the environment with clipped in the name
list.1 <- mget(grep("clipped",ls(),value=TRUE))
#list.3[[1]] <- NULL
IDs<-names(list.1) # Get the object names from the list... seems to be easier to work with this
length(IDs)


# loop through all the plots and mask them so that they are circular
for(k in 1:length(IDs))
{
  try(assign(paste('masked.',Objs[k], sep=""), mask(get(IDs[k]), get(Objs[k]))) )
}

chm<-masked.plot10
str(masked.plot35)
plot(chm)


# edit(FindTreesCHM)
# edit(xyFromCell)
# edit(focal)
# 
# fws<-3
# minht<-0.5
# 


# #smooth CHM
# s.my.chm<-CHMsmoothing(masked.plot10, "Gaussian", 5)
# #Identify trees
# tree.list<-FindTreesCHM(full.chm, 5, 1)
# tree.list<-FindTreesCHM(s.my.chm, 5, 0.5)
# summary(tree.list)
# 
# #Plot it up
# plot(s.my.chm)
# XY<-SpatialPoints(tree.list[,1:2])
# plot(XY, add=TRUE, col="red")
# 
# #Get 2D canopy boundaries
# can.bound<-ForestCAS(chm=s.my.chm, loc=tree.list, maxcrown=5, exclusion=0.5)
# boundaryTrees<-can.bound[[1]]
# plot(boundaryTrees, add=T, border='red', bg='transparent')

###############################
# Read Rod and Dave's tree locations

source ('geraldine.field.data.R')

#plot(tree.out)
str(tree.out)
names(tree.out)

tree.list<-tree.out[c('tree.x', 'tree.y', 'HT.m.', 'Plot.No.')]
names(tree.list)<-c("x", "y", "height", 'Plot.No.')
#class(tree.list)
#str(tree.list)

tr.plots<-unique(tree.list$Plot.No.)

for ( i in 1:length(tr.plots))
{
  
trees<-subset(tree.list, Plot.No. == tr.plots[i])
trees<-trees[c("x", "y", "height")]
trees<-subset(trees, height>0.5)

test<-if(length(grep("X",tr.plots[i]))>0) print("found") else print("Not found")

if(test == "Not found")
{
  chm<- get(paste('masked.plot', tr.plots[i], sep=""))
}

if(test == "found")
{
  chm<- get(paste('masked.', tr.plots[i], sep=""))
}



try(can.bound<-ForestCAS(chm=chm, loc=trees, maxcrown=3, exclusion=0.5))

plot(chm)
XY<-SpatialPoints(trees[,1:2])
plot(XY, add=TRUE, col="red")
assign(paste('b.Tr', tr.plots[6], sep=""), can.bound[[1]])
plot(boundaryTrees, add=T, border='green', bg='transparent')




}