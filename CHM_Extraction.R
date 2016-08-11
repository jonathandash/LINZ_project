# *************************************************************************************************************************
# This analysis is part of the LINZ wilding project
# Can we extract canopy extents from the CHM? quick and dirty first look.
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

#chm<-masked.plot10
#str(masked.plot35)
#plot(chm)


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

#source ('geraldine.field.data.R')

#plot(tree.out)
#str(tree.out)
#names(tree.out)

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

#test<-if(length(grep("X",tr.plots[i]))>0) print("found") else print("Not found")

#if(test == "Not found")
#{
  chm<- get(paste('masked.', tr.plots[i], sep=""))
#}

#if(test == "found")
#{
#  chm<- get(paste('masked.', tr.plots[i], sep=""))
#}

try(can.bound<-ForestCAS(chm=chm, loc=trees, maxcrown=3, exclusion=0.01))

plot(chm, main=tr.plots[i])
XY<-SpatialPoints(trees[,1:2])
plot(XY, add=TRUE, col="red")
try(assign(paste('b.Tr', tr.plots[i], sep=""), can.bound[[1]]))
try(plot(get(paste('b.Tr', tr.plots[i], sep="")), add=T, border='green', bg='transparent'))
}



# Manuals for failures
i=1
try(can.bound<-ForestCAS(chm=chm, loc=trees, maxcrown=3, exclusion=0.5))


# #List all objects in the environment with b.Tr
list.b <- mget(grep("b.Tr",ls(),value=TRUE))
#list.3[[1]] <- NULL
IDbs<-names(list.b) # Get the object names from the list... seems to be easier to work with this
length(IDbs)

for (j in 1:length(IDbs))
{
  
  writeOGR(get(IDbs[j]), dsn = 'Q:\\Forest Industry Informatics\\Projects\\LINZ\\Data\\canopy_shapes', layer=IDbs[j], driver = 'ESRI Shapefile')
  
}


#*******************************
# HERE need to add functions for merge all the canopy shapes and dissolving - currently done manually in acr with the addition of
# a single tree canopy
#*******************************
for (j in 1:length(IDbs))
{
  
  merged_canopy_polys<-union (get(IDbs[1]), get(IDbs[2]))
}
  
# lasclip reclassify wilding points as 10
# lasclip to plot boundaries - remove plot X4 from the sample because the tree locations were not recoded in the field
  
#******************************
# Now the task is to read the coloured wilding classified point clouds for the plots into and convert to dataframes to
# be fed into random forests
#*********************************

library(scatterplot3d)
p5<-readLAS('Z:\\Geraldine_Lidar\\10_Classified_Normalised_Wildings\\coloured_wilding_classified_clipped\\plotx5.las', short=FALSE)
p5<-as.data.frame(p5)

# read shapefiles
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

mats<-list(mats)
rm(mats) mats[1]
for (i in 1:length(mats))
{
  rm(plot26)
}



# Make a binomial response variable.

all.points$wld.key<-ifelse(all.points$Classification == 10, 'W', 'N')
all.points$wld.key<-as.factor(all.points$wld.key)
all.points$plot<-as.factor(all.points$plot)
str(all.points)

library(randomForest)
library(party)
rf1<-randomForest(wld.key~Z+Intensity+R+G+B+ScanAngleRank, data = all.points, proximity=FALSE, importance=TRUE, 
                  keep.forest=FALSE, keep.inbag=FALSE, ntree=5000)

rf1<-randomForest(wld.key~Z+Intensity+R+G+B+ScanAngleRank, data = all.points, proximity=FALSE, importance=TRUE, 
                  keep.forest=FALSE, keep.inbag=FALSE, ntree=5000)

rf2<-randomForest(wld.key~Z+Intensity+ScanAngleRank, data = all.points, proximity=FALSE, importance=TRUE, 
                  keep.forest=FALSE, keep.inbag=FALSE, ntree=500)


#Compare with first returns only
f.returns<-subset(all.points, ReturnNumber == 1)
rf1.fr<-randomForest(wld.key~Z+Intensity+R+G+B+ScanAngleRank, data = f.returns, proximity=FALSE, importance=TRUE, 
                          keep.forest=FALSE, keep.inbag=FALSE, ntree=1000)

#Check the model without ground returns
abg.returns<-subset(all.points, Classification != 2)
rf1.abg<-randomForest(wld.key~Z+Intensity+R+G+B+ScanAngleRank, data = abg.returns, proximity=FALSE, importance=TRUE, 
                     keep.forest=FALSE, keep.inbag=FALSE)


#Explore caret
library(caret)

Folds <- createMultiFolds(y = all.points$wld.key, k = 10, times = 10)

rfControl <- trainControl(method = "repeatedcv", 
                          number = 10, repeats = 10, index = Folds,
                          classProbs = TRUE,
                          allowParallel = TRUE,
                          selectionFunction = "oneSE",
                          trim = TRUE,
                          returnResamp = "final")

rfGrid <- expand.grid(mtry = seq(2,4, by = 1))
library(e1071)
str(all.points)
gc()
rfFit <- train(wld.key~Z+Intensity+R+G+B+ScanAngleRank, data = all.points,
               method = "rf",
               importance = TRUE, ntree = 500,
               trControl = rfControl, tuneGrid = rfGrid,
               metric = "Kappa", maximize = TRUE)

str(all.points)
#Explore ranger
library(ranger)
rf1.rang<-ranger(wld.key~Z, data = all.points, importance = "impurity")
rf2.rang<-ranger(wld.key~Z + Intensity, data = all.points, importance = "impurity")
rf3.rang<-ranger(wld.key~Z + R, data = all.points, importance = "impurity")
rf4.rang<-ranger(wld.key~Z+Intensity + R, data = all.points, importance = "impurity")
rf5.rang<-ranger(wld.key~Z+Intensity + R + B, data = all.points, importance = "impurity")
rf6.rang<-ranger(wld.key~Z+Intensity + R + G + B, data = all.points, importance = "impurity")
rf7.rang<-ranger(wld.key~ R + G + B, data = all.points, importance = "impurity")
rf8.rang<-ranger(wld.key~ Z + R + G + B, data = all.points, importance = "impurity")

f.returns<-subset(all.points, ReturnNumber == 1)
rf9.fr.rang<-ranger(wld.key~Z+Intensity + R + G + B, data = f.returns, importance = "impurity")


#Calculate Cohen's Kappa for all models including the model with just first returns
library(irr)
ratings.rf1<-data.frame(all.points$wld.key, rf1.rang$predictions)
ratings.rf2<-data.frame(all.points$wld.key, rf2.rang$predictions)
ratings.rf3<-data.frame(all.points$wld.key, rf3.rang$predictions)
ratings.rf4<-data.frame(all.points$wld.key, rf4.rang$predictions)
ratings.rf5<-data.frame(all.points$wld.key, rf5.rang$predictions)
ratings.rf6<-data.frame(all.points$wld.key, rf6.rang$predictions)
ratings.rf7<-data.frame(all.points$wld.key, rf7.rang$predictions)
ratings.rf8<-data.frame(all.points$wld.key, rf8.rang$predictions)

ratings.rf9.fr<-data.frame(f.returns$wld.key, rf9.fr.rang$predictions)


rf1.k<-kappa2(ratings=ratings.rf1, weight ="unweighted", sort.levels = FALSE)
rf2.k<-kappa2(ratings=ratings.rf2, weight ="unweighted", sort.levels = FALSE)
rf3.k<-kappa2(ratings=ratings.rf3, weight ="unweighted", sort.levels = FALSE)
rf4.k<-kappa2(ratings=ratings.rf4, weight ="unweighted", sort.levels = FALSE)
rf5.k<-kappa2(ratings=ratings.rf5, weight ="unweighted", sort.levels = FALSE)
rf6.k<-kappa2(ratings=ratings.rf6, weight ="unweighted", sort.levels = FALSE)
rf7.k<-kappa2(ratings=ratings.rf7, weight ="unweighted", sort.levels = FALSE)
rf8.k<-kappa2(ratings=ratings.rf8, weight ="unweighted", sort.levels = FALSE)
rf9.fr.k<-kappa2(ratings=ratings.rf9.fr, weight ="unweighted", sort.levels = FALSE)

importance(rf6.rang)

####******
# Bringin in lower density datasets for comparison

#*****************
# 10 pulses
# read shapefiles
setwd(las.dir.10)
plist=list.files(las.dir.10, pattern="las$", full.names=FALSE)
for(i in plist) { assign(unlist(strsplit(i, "[.]"))[1], readLAS(i, short=FALSE)) }


## List all spatial polygons dataframe in environemnt 
ClassFilter <- function(x) inherits(get(x), 'matrix' ) # filter function for 
mats <- Filter( ClassFilter, ls() )

# merge all the points into a datframe
all.points.10<-data.frame()

for (i in 1:length(mats))
{
  p<-as.data.frame(get(mats[i]))
  p$plot<-mats[i]
  all.points.10<-rbind(all.points.10, p)
}

# Make a binomial response variable.

all.points.10$wld.key<-ifelse(all.points.10$Classification == 10, 'W', 'N')
all.points.10$wld.key<-as.factor(all.points.10$wld.key)
all.points.10$plot<-as.factor(all.points.10$plot)


#******************************************
# 5 pulses

# read shapefiles
setwd(las.dir.5)
plist=list.files(las.dir.5, pattern="las$", full.names=FALSE)
for(i in plist) { assign(unlist(strsplit(i, "[.]"))[1], readLAS(i, short=FALSE)) }


## List all spatial polygons dataframe in environemnt 
ClassFilter <- function(x) inherits(get(x), 'matrix' ) # filter function for 
mats <- Filter( ClassFilter, ls() )

# merge all the points into a datframe
all.points.5<-data.frame()

for (i in 1:length(mats))
{
  p<-as.data.frame(get(mats[i]))
  p$plot<-mats[i]
  all.points.5<-rbind(all.points.5, p)
}

# Make a binomial response variable.

all.points.5$wld.key<-ifelse(all.points.5$Classification == 10, 'W', 'N')
all.points.5$wld.key<-as.factor(all.points.5$wld.key)
all.points.5$plot<-as.factor(all.points.5$plot)



#******************************************
# 2 pulses

# read shapefiles
setwd(las.dir.2)
plist=list.files(las.dir.2, pattern="las$", full.names=FALSE)
for(i in plist) { assign(unlist(strsplit(i, "[.]"))[1], readLAS(i, short=FALSE)) }


## List all spatial polygons dataframe in environemnt 
ClassFilter <- function(x) inherits(get(x), 'matrix' ) # filter function for 
mats <- Filter( ClassFilter, ls() )

# merge all the points into a datframe
all.points.2<-data.frame()

for (i in 1:length(mats))
{
  p<-as.data.frame(get(mats[i]))
  p$plot<-mats[i]
  all.points.2<-rbind(all.points.2, p)
}

# Make a binomial response variable.

all.points.2$wld.key<-ifelse(all.points.2$Classification == 10, 'W', 'N')
all.points.2$wld.key<-as.factor(all.points.2$wld.key)
all.points.2$plot<-as.factor(all.points.2$plot)



#******************************************
# 1 pulse

# read shapefiles
setwd(las.dir.1)
plist=list.files(las.dir.1, pattern="las$", full.names=FALSE)
for(i in plist) { assign(unlist(strsplit(i, "[.]"))[1], readLAS(i, short=FALSE)) }


## List all spatial polygons dataframe in environemnt 
ClassFilter <- function(x) inherits(get(x), 'matrix' ) # filter function for 
mats <- Filter( ClassFilter, ls() )

# merge all the points into a datframe
all.points.1<-data.frame()

for (i in 1:length(mats))
{
  p<-as.data.frame(get(mats[i]))
  p$plot<-mats[i]
  all.points.1<-rbind(all.points.1, p)
}

# Make a binomial response variable.

all.points.1$wld.key<-ifelse(all.points.1$Classification == 10, 'W', 'N')
all.points.1$wld.key<-as.factor(all.points.1$wld.key)
all.points.1$plot<-as.factor(all.points.1$plot)


# Now fit RF models for the lower densities
library(verification)

rf6.rang10p<-ranger(wld.key~Z+Intensity + R + G + B, data = all.points.10, importance = "impurity", write.forest = TRUE)
rf6.rang5p<-ranger(wld.key~Z+Intensity + R + G + B, data = all.points.5, importance = "impurity", write.forest = TRUE)
rf6.rang2p<-ranger(wld.key~Z+Intensity + R + G + B, data = all.points.2, importance = "impurity", write.forest = TRUE)
rf6.rang1p<-ranger(wld.key~Z+Intensity + R + G + B, data = all.points.1, importance = "impurity", write.forest = TRUE)


predictions(rf6.rang10p)

ratings.rf6.10p<-data.frame(all.points.10$wld.key, rf6.rang10p$predictions)
ratings.rf6.5p<-data.frame(all.points.5$wld.key, rf6.rang5p$predictions)
ratings.rf6.2p<-data.frame(all.points.2$wld.key, rf6.rang2p$predictions)
ratings.rf6.1p<-data.frame(all.points.1$wld.key, rf6.rang1p$predictions)

rf6.k
rf6.10p.k<-kappa2(ratings=ratings.rf6.10p, weight ="unweighted", sort.levels = FALSE)
rf6.5p.k<-kappa2(ratings=ratings.rf6.5p, weight ="unweighted", sort.levels = FALSE)
rf6.2p.k<-kappa2(ratings=ratings.rf6.2p, weight ="unweighted", sort.levels = FALSE)
rf6.1p.k<-kappa2(ratings=ratings.rf6.1p, weight ="unweighted", sort.levels = FALSE)


x=ifelse(all.points.10$wld.key == 'W', 1,0)
pred = ifelse(rf6.rang10p$predictions == 'W', 1,0)


roc.plot(x=x, pred=pred, legend = TRUE, binormal =FALSE) 





#################################
# Counts in the point cloud stuff
#********************************

canopy.shape.dir<-'Q:\\Forest Industry Informatics\\Projects\\LINZ\\Data\\canopy_shapes\\' #Set shapefile directory
#cpy.shp<-shapefile('Q:\\Forest Industry Informatics\\Projects\\LINZ\\Data\\canopy_shapes\\all_merged_canopies.shp')
proj.nztm<-"+proj=tmerc +lat_0=0 +lon_0=173 +k=0.9996 +x_0=1600000 +y_0=10000000 +ellps=GRS80 +units=m +no_defs"

#***************
# Make a figure
#***********************

# Make a figure for presentaion
setwd(canopy.shape.dir) # Get the canopy shapes
cpy.shp<-shapefile('b.TrX17.shp')

#Get full point cloud dataset
setwd(las.dir)
X17all<-readLAS('X17.las', short = FALSE)
X17all<-as.data.frame(X17all)
coordinates(X17all)<-~X+Y #Make the las file a spatial points dataframe

#Get 10 point cloud dataset
setwd(las.dir.10)
X17.10<-readLAS('X17.las', short = FALSE)
X17.10<-as.data.frame(X17.10)
coordinates(X17.10)<-~X+Y #Make the las file a spatial points dataframe

#Get 5 point cloud dataset
setwd(las.dir.5)
X17.5<-readLAS('X17.las', short = FALSE)
X17.5<-as.data.frame(X17.5)
coordinates(X17.5)<-~X+Y #Make the las file a spatial points dataframe

#Get 2 point cloud dataset
setwd(las.dir.2)
X17.2<-readLAS('X17.las', short = FALSE)
X17.2<-as.data.frame(X17.2)
coordinates(X17.2)<-~X+Y #Make the las file a spatial points dataframe

#Get 1 point cloud dataset
setwd(las.dir.1)
X17.1<-readLAS('X17.las', short = FALSE)
X17.1<-as.data.frame(X17.1)
coordinates(X17.1)<-~X+Y #Make the las file a spatial points dataframe



library(sfsmisc)
tiff('Q:\\Forest Industry Informatics\\Projects\\LINZ\\Reports\\images_for report\\plot17_density_comp.tif', w=17, h=7, units="cm", res=300)
layout(matrix(1:5, nrow = 1))
#par(mar = c(0, 1.5, 0, 0), oma = c(4, 4, 3, 1), tcl = 0.35, mgp = c(2, 0.4, 0))
op <- par(mar = rep(0, 4), oma = c(4, 4, 3, 1), tcl = 0.35, mgp = c(2, 0.4, 0))
plot(X17all, pch=19, cex = 0.2)
plot(cpy.shp, add=T, border = "red")
eaxis(2, las=1, log = TRUE)
eaxis(1)
box()

plot(X17.10, pch=19, cex = 0.2)
plot(cpy.shp, add=T, border = "red")
eaxis(2, labels = F)
eaxis(1)
box()

plot(X17.5, pch=19, cex = 0.2)
plot(cpy.shp, add=T, border = "red")
eaxis(2, labels = F)
eaxis(1)
box()

plot(X17.2, pch=19, cex = 0.2)
plot(cpy.shp, add=T, border = "red")
eaxis(2, labels = F)
eaxis(1)
box()

plot(X17.1, pch=19, cex = 0.2)
plot(cpy.shp, add=T, border = "red")
axis(2, labels = F)
axis(1)
box()
dev.off()


test<-extract(cpy.shp, X17)

writeOGR(X17, dsn='D:\\temp', layer='X17pts.shp', driver = "ESRI Shapefile")



######















####
# explore SVM
library(kernlab) # for ksvm
ksvm.1<-ksvm(wld.key~Z+Intensity+R+G+B+ScanAngleRank, data = all.points)

d=all.points[c('Z', 'Intensity', 'R', 'G', 'B', 'ScanAngleRank')]
plot(ksvm.1, d)

#explore sofia
lambda <- 0.01
iterations <- 1e+07
learner_type <- 'sgd-svm'
eta_type <- 'pegasos'
loop_type <- "balanced-stochastic"
rank_step_probability <- 0.5

sofiaModel <- sofia(wld.key ~ Z + R,
                    data=all.points,
                    random_seed =9,
                    lambda = lambda,
                    iterations = iterations,
                    learner_type = learner_type,
                    eta_type = eta_type,
                    loop_type = loop_type,
                    rank_step_probability = rank_step_probability)


np<-predict.sofia(sofiaModel, all.points, prediction_type = "linear")










#*********************************************************
print(rf1)
MDSplot(rf1, all.points$Species)
plot(rf1)
varImpPlot(rf1)
importance(rf1)
partialPlot(rf1)
rf1$confusion

library(ROCR)


OOB.votes <- predict (rf1,all.points,type="prob");
OOB.pred <- OOB.votes[,2];

pred.obj <- prediction (OOB.pred,y);

RP.perf <- performance(pred.obj, "rec","prec");
plot (RP.perf);

ROC.perf <- performance(pred.obj, "fpr","tpr");
plot (ROC.perf);

plot  (RP.perf@alpha.values[[1]],RP.perf@x.values[[1]]);
lines (RP.perf@alpha.values[[1]],RP.perf@y.values[[1]]);
lines (ROC.perf@alpha.values[[1]],ROC.perf@x.values[[1]]);


#cf1<-cforest(wld.key ~ ., data=all.points,  control=cforest_unbiased())





