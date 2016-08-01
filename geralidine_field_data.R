####*************************************************************************####
# This script is for data processing of the raw data collected at Geraldine
# Jonathan.dash@scionresearch.com
# July 2016
#****************************************************************************

#Set libraries
library(dplyr)
library(ggplot2)
library(rgeos)
library(sp)
library(rgdal)
library(raster)
library(XLConnect)

# Read plot centre shapefile
plt.cent<-shapefile('Q:\\Forest Industry Informatics\\Projects\\LINZ\\Data\\GPS\\Geraldine_all_plots.shp')
plot(plt.cent)

# Read tree data
tree.dat<- readWorksheetFromFile('Q:\\Forest Industry Informatics\\Projects\\LINZ\\Data\\Geraldine Wilding Survey_AllData.xls',
                                     sheet=2)

# Read plot data
plot.dat<- readWorksheetFromFile('Q:\\Forest Industry Informatics\\Projects\\LINZ\\Data\\Geraldine Wilding Survey_AllData.xls',
                                 sheet=3)

# Make a dataframe from the shapefile
plt.cent.df<-as.data.frame(plt.cent)

# rename the coordinate columns
names(plt.cent.df)[names(plt.cent.df) == "coords.x1"] <- "x"
names(plt.cent.df)[names(plt.cent.df) == "coords.x2"] <- "y"
names(plt.cent.df)[names(plt.cent.df) == "coords.x3"] <- "z"

plt.cent.df$Plot.No.<-gsub("plot", "", plt.cent.df$Comment) # remove the "plot" from the plot name
plt.cent.df$Plot.No.<-toupper(plt.cent.df$Plot.No.) # Get rid of the lower case Xs in the plot name

plt<-plt.cent.df[c('Plot.No.', 'x', 'y')] # create df with just the x and y coordinates

#unique(tree.dat$Plot.No.)
#unique(plt$Plot.No.)

tree.dat.xy<-merge(plt, tree.dat, by="Plot.No.") # This should have the same number of rows as tree.dat

deg2rad <- function(deg) {(deg * pi) / (180)} # function converst decimal degrees to radians


tree.dat.xy$Bearing.grid<-tree.dat.xy$Bearing+24 # Create a new column accounting for the magneitc declination

tree.dat.xy$Bearing.grid<- ifelse(tree.dat.xy$Bearing.grid < 360, tree.dat.xy$Bearing.grid,
                                  tree.dat.xy$Bearing.grid - 360) # Deal with the greater than 360 problem



tree.dat.xy$bea.rad<-deg2rad(tree.dat.xy$Bearing.grid) #make new column containing 

tree.dat.xy$tree.x<-tree.dat.xy$x+sin(tree.dat.xy$bea.rad)*tree.dat.xy$Distance #Calculate x coordinate of trees 
tree.dat.xy$tree.y<-tree.dat.xy$y+cos(tree.dat.xy$bea.rad)*tree.dat.xy$Distance #Calculate y coordinate of trees 
tree.dat.xy$ht.key<-ifelse(is.na(tree.dat.xy$DBH.mm=="TRUE"),0,1)

### Create summary table for inclusion in report
table.rep<-tree.dat.xy %>% group_by(Spp., ht.key) %>% summarise (n = length(x),
                                                         mean.dbh = mean(DBH.mm., na.rm=T),
                                                         min.dbh = min(DBH.mm., na.rm=T),
                                                         max.dbh = max(DBH.mm., na.rm=T),
                                                         mean.ht = mean(HT.m., na.rm=T),
                                                         min.ht = min(HT.m., na.rm=T),
                                                         max.ht = max(HT.m., na.rm=T),
                                                         mean.gld = mean(GLD.mm., na.rm=T),
                                                         min.gld = min(GLD.mm., na.rm=T),
                                                         max.gld = max(GLD.mm., na.rm=T))
              

write.csv(table.rep, 'Q:\\Forest Industry Informatics\\Projects\\LINZ\\Reports\\field_table.csv')

tree.out<-tree.dat.xy[c("x", "y",  "tree.x", "tree.y", "Plot.No.", "Tr.No.", "Spp.", "DBH.mm.", "GLD.mm.", "HT.m.", "Distance", "Bearing.grid", "bea.rad")] # prepare for output



tree.out$DBH.mm.[is.na(tree.out$DBH.mm.)] <- 0 # change NAs to zeros
tree.out$GLD.mm.[is.na(tree.out$GLD.mm.)] <- 0 # change NAs to zeros


tree.out<-(tree.out[complete.cases(tree.out), ]) # remove trees with no distance and bearing recorded
#write.csv(tree.out, 'D:\\temp\\tree.dat.csv')
coordinates(tree.out)<-~tree.x + tree.y #make it a spatial points dataframe
plot(tree.out)

# Export to be viewed
#writeOGR(tree.out, dsn='Q:\\Forest Industry Informatics\\Projects\\LINZ\\Data\\GPS', layer='tree.locations', driver='ESRI Shapefile')


# Now calculate plot level statistics

plt.tr<-merge(plot.dat, tree.dat, by="Plot.No.", all=T)

plt.sum<- plt.tr %>% group_by(Plot.No.) %>% filter(!is.na(Tr.No.)) %>% summarise (sph=length(Tr.No.),
                                                                                  BA=(sum(0.00007854 * (DBH.mm./10)^2, na.rm=T)),
                                                                                  mn.ht = mean(HT.m.))

plt.sum.stats<-merge(plot.dat, plt.sum, by="Plot.No.", all=T)


plt.sum.stats$sph[is.na(plt.sum.stats$sph)] <- 0
plt.sum.stats$BA[is.na(plt.sum.stats$BA)] <- 0
plt.sum.stats$mn.ht[is.na(plt.sum.stats$mn.ht)] <- 0

plt.sum.stats$Vol<-plt.sum.stats$BA*plt.sum.stats$mn.ht

hist(plt.sum.stats$BA, breaks=60)
hist(plt.sum.stats$sph, breaks=60)
hist(plt.sum.stats$mn.ht, breaks=60)
hist(plt.sum.stats$Vol, breaks=80)



