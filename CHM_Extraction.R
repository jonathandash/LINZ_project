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


edit(FindTreesCHM)
edit(xyFromCell)
edit(focal)

fws<-3
minht<-0.5



#smooth CHM
s.my.chm<-CHMsmoothing(masked.plot10, "Gaussian", 5)
#Identify trees
tree.list<-FindTreesCHM(full.chm, 5, 1)
tree.list<-FindTreesCHM(s.my.chm, 5, 0.5)
summary(tree.list)

#Plot it up
plot(s.my.chm)
XY<-SpatialPoints(tree.list[,1:2])
plot(XY, add=TRUE, col="red")

#Get 2D canopy boundaries
can.bound<-ForestCAS(chm=s.my.chm, loc=tree.list, maxcrown=5, exclusion=0.5)
boundaryTrees<-can.bound[[1]]
plot(boundaryTrees, add=T, border='red', bg='transparent')

###############################
# Read Rod and Dave's tree locations

source ('geraldine.field.data.R')

plot(tree.out)
str(tree.out)
names(tree.out)

tree.list<-tree.out[c('tree.x', 'tree.y', 'HT.m.')]
names(tree.list)<-c("x", "y", "height")
class(tree.list)
str(tree.list)

can.bound<-ForestCAS(chm=full.chm, loc=tree.list, maxcrown=5, exclusion=0.5)

function (chm, loc, maxcrown = 10, exclusion = 0.3) 
{
  if (class(chm) != "RasterLayer" & class(chm) != "SpatialGridDataFrame") {
    stop("The chm is invalid. It must to be a RasterLayer or SpatialGridDataFrame'")
  }
  if (ncol(loc) != 3) {
    stop("The input loc is invalid. It must to be 3-column matrix or dataframe with the x,y coordinates and heights of the individual trees")
  }
  if (class(maxcrown) != "numeric") {
    stop("The maxcrown parameter is invalid. It is not a numeric input")
  }
  if (class(exclusion) != "numeric") {
    stop("The exclusion parameter is invalid. It is not a numeric input")
  }
  if (exclusion >= 1) {
    stop("The exclusion parameter is invalid. It must to be less than 1numeric input")
  }
  if (class(chm) == "RasterLayer") {
    chm <- as(chm, "SpatialGridDataFrame")
  }
  Hthreshold <- min(loc[, 3]) * exclusion
  polys <- list()
  width <- numeric()
  DF2raster <- function(h.mH, i) {
    h.mHkl <- subset(h.mH, h.mH[, 4] == levels(factor(h.mH[, 
                                                           4]))[i])
    if (nrow(h.mHkl == 1) == TRUE) {
      h.mHkl <- rbind(h.mHkl, c(h.mHkl[, 1], h.mHkl[, 
                                                    2] + 0.005, h.mHkl[, 3] + 0.005, h.mHkl[, 4]))
    }
    spP <- cbind(h.mHkl[, 2:3], h.mHkl[, 1], h.mHkl[, 4])
    colnames(spP) <- c("x", "y", "z", "id")
    m <- suppressWarnings(SpatialPixelsDataFrame(points = spP[c("x", 
                                                                "y")], data = spP))
    rasterDF <- raster(m)
    hhg <- boundaries(rasterDF, type = "outer")
    p <- rasterToPolygons(hhg, dissolve = TRUE)
    sp.polys <- p[1, ]
    return(sp.polys)
  }
  for (i in 1:nrow(loc)) {
    width[i] = maxcrown
    discbuff <- disc(radius = width[i], centre = c(loc$x[i], 
                                                   loc$y[i]))
    discpoly <- Polygon(rbind(cbind(discbuff$bdry[[1]]$x, 
                                    y = discbuff$bdry[[1]]$y), c(discbuff$bdry[[1]]$x[1], 
                                                                 y = discbuff$bdry[[1]]$y[1])))
    polys <- c(polys, discpoly)
  }
  spolys <- list()
  for (i in 1:length(polys)) {
    spolybuff <- Polygons(list(polys[[i]]), ID = row.names(loc)[i])
    spolys <- c(spolys, spolybuff)
  }
  polybuffs <- SpatialPolygons(spolys)
  chmdf <- as.data.frame(chm, xy = TRUE)
  Points.Ply <- over(SpatialPoints(chmdf[, 2:3]), polybuffs)
  Points.PlyD <- cbind(chmdf, Points.Ply)
  Points.PlyD <- na.omit(Points.PlyD)
  vor = deldir(loc[, 1], loc[, 2], z = loc[, 3], suppressMsge = T)
  tile = tile.list(vor)
  polys = vector(mode = "list", length = length(tile))
  for (i in seq(along = polys)) {
    pcrds = cbind(tile[[i]]$x, tile[[i]]$y)
    pcrds = rbind(pcrds, pcrds[1, ])
    polys[[i]] = Polygons(list(Polygon(pcrds)), ID = as.character(i))
  }
  SP = SpatialPolygons(polys)
  veronoi = SpatialPolygonsDataFrame(SP, data = data.frame(x = loc[, 
                                                                   1], y = loc[, 2], row.names = sapply(slot(SP, "polygons"), 
                                                                                                        function(x) slot(x, "ID"))))
  chmV <- over(SpatialPoints(Points.PlyD[, 2:3]), SP)
  RpD <- cbind(Points.PlyD, chmV)
  RpD.filter <- subset(RpD[, 1:5], RpD[, 1] >= Hthreshold)
  RpD.filter <- cbind(RpD.filter[, 1:3], RpD.filter[, 5])
  colnames(RpD.filter) <- c("z", "x", "y", "g")
  h.mH <- ddply(RpD.filter, "g", function(RpD.filter) subset(RpD.filter, 
                                                             RpD.filter[, 1] >= max(RpD.filter[, 1]) * exclusion))
  for (j in 1:nlevels(factor(h.mH[, 4]))) {
    assign(paste0("SP.polys", j), DF2raster(h.mH, j))
    cat(".")
    flush.console()
  }
  polygons <- slot(get("SP.polys1"), "polygons")
  for (i in 1:nlevels(factor(h.mH[, 4]))) {
    data.loc <- get(paste0("SP.polys", i))
    polygons <- c(slot(data.loc, "polygons"), polygons)
  }
  for (i in 1:length(polygons)) {
    slot(polygons[[i]], "ID") <- paste(i)
  }
  spatialPolygons <- SpatialPolygons(polygons)
  spdf <- SpatialPolygonsDataFrame(spatialPolygons, data.frame(Trees = 1:length(polygons)))
  options(scipen = 10)
  spdf <- spdf[spdf@data[-length(polygons), ], ]
  areaList <- sapply(slot(spdf, "polygons"), slot, "area")
  canopyTable <- cbind(loc, areaList)
  colnames(canopyTable) <- c("x", "y", "z", "ca")
  result = list(spdf, canopyTable)
  return(result)
}


