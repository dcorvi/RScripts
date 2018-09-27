# Tagdata_Eastcoast.R 
#
# sample R program to read tag data, get satellite data for the tag times &
# locations using xtracto, and map the satellite values for the tag times &
# locations
#
# This uses the rerddap version of xtractomatic: rerddapXtracto
# 
# The two essential things to modify to run another data set are
# (1) the infile containing the tagged data to match-up to satellite data (line 54) 
# (2) the satellite data you want data from (line 76).  This data is taken from 
# the ERDDAP server at http://coastwatch.pfeg.noaa.gov/erddap/
#  
# There are some sample plots drawn just to demonstrate that the data was read in 
# and is available - what you do with it next is entirely up to you!  

###### INSTALL PACKAGES

devtools::install_github("ropensci/rerddap")
devtools::install_github("rmendels/rerddapXtracto")


############################################################################
############################################################################
# GGPlot using lobster landings
### LOAD LIBRARIES

require(sp)
require(ncdf4)
require(parsedate)
require(maps)
require(maptools)
require(graphics)
require(lubridate)

library(rerddap)
library(rerddapXtracto)
library("lubridate", lib.loc="~/R/R-3.5.1/library")


setwd("C:/Sources/datafiles")





infile <- 'noaa_trip_2007_0Oct.csv' #5010 observations - 1.4 hours


tagdata <- read.csv(infile,head=TRUE,sep=",",stringsAsFactors = FALSE)

# for convenience make shorter names for the latitude and longitude data 

lon <- tagdata$lon
lat <- tagdata$lat

# Define the search "radius" in the x any y directions, in units of degrees
xlen <- 0.1
ylen <- 0.1

# Change the date format code as needed for reading in datasets using a different timeformat
tdatestimes <- tagdata$date        # read the date column from the tagdata csv file
# tdatestimes <- mdy_hms(tdatestimes)  # create date format from factor format (categorical format)
date <- dmy(tdatestimes)

# Shorten 3-hourly tag data to daily: subset turtle times for only times = 0:00
# tcoord_sub <- subset(tdatestimes, format(tdatestimes,'%H')=='00')
# # Do the same for xcoord and ycoord: subset lats and lons for Hour=0
# xcoord_sub <- subset(xcoord, format(tdatestimes,'%H')=='00')
# ycoord_sub <- subset(ycoord, format(tdatestimes,'%H')=='00')

### Chlorophyll
dataInfo <- rerddap::info('erdMH1chla1day')
dataInfo
parameter <- 'chlorophyll' ### type dataInfo to find variable of new dataset


################ Bathymetry ################
dataInfo <- rerddap::info('etopo360')
dataInfo
parameter <- 'altitude'

# zcoord <- rep(0, length(ycoord) )  # vector of zero's & length of ycoord_sub - not needed when dataset has no altitude
# Be sure rxtracto specifies the ERDDAP server holding the data: use the
# argument "urlbase"


#########################################################################
#### Modis data - older -  no zcoord - error: NA values in coordia - subset of data
abeginTime <- Sys.time()
modischl <- rxtracto(dataInfo,
                     parameter=parameter,
                     xcoord=lon, ycoord=lat, tcoord=date,
                     xlen=xlen, ylen=ylen,
                     urlbase="https://coastwatch.pfeg.noaa.gov/erddap", verbose = TRUE)
aendTime <- Sys.time()
abeginTime - aendTime

################ Bathymetry ################

abeginTime <- Sys.time()
modischl <- rxtracto(dataInfo,
                     parameter=parameter,
                     xcoord=lon, ycoord=lat,
                     xlen=xlen, ylen=ylen,
                     urlbase="https://coastwatch.pfeg.noaa.gov/erddap", verbose = TRUE)
aendTime <- Sys.time()
abeginTime - aendTime


# After it has completed the extraction the data.frame viirschl will contain
# as many rows as datapoints in the input file and will have 13 columns: 
#
# mean chlor_a =      mean of data within search radius
# stdev chlor_a =     standard deviation of data within search radius
# n =                 number of points found within search radius
# satellite date =    time of returned value
# requested lon min = min longitude of call (tagdata lon - search radius)
# requested lon max = max longitude of call (tagdata lon + search radius)
# requested lat min = min latitude of call 
# requested lat max = max latitude of call
# requested z min =   min altitude of call
# requested z max =   max altitude of call
# requested date =    requested time in tag dataset
# median =            median of data within search radius
# mad =               median absolute deviation of data within search radius


# Now make some plots of the data. 
require(ggplot2)
require(mapdata)

# modischl <- modischlJuly07
# modischl <- modischlSept07

str(modischl)
alldata <- data.frame(date, lon, lat, modischl$"mean chlorophyll")


str(alldata)
names(alldata)[names(alldata)=="modischl..mean.chlorophyll."] <- "modischl_mean"
str(alldata)

# Create a variable that shows if chla is missing
alldata$missing <- is.na(alldata$modischl_mean)*1
str(alldata)

### make NAs 0 to find max number
alldata$modischl_mean[is.na(alldata$modischl_mean)] <- 0

### get Max parameter number to add to top of color bar
maxchl <- max(alldata$modischl_mean)

# Write the Data to CSV File
write.table(alldata, file=paste0("C:/Sources/datafiles/LobsterChlor_",substr(infile,start=17,stop=19),"07.csv"),sep=",",row.names=F)

##############################################################################



############################################################################
################ Bathymetry ################

# Now make some plots of the data. 
require(ggplot2)
require(mapdata)

# modischl <- modischlJuly07
# modischl <- modischlSept07

str(modischl)
alldata <- data.frame(date, lon, lat, modischl$"mean altitude")


str(alldata)
names(alldata)[names(alldata)=="modischl..mean.altitude."] <- "modischl_mean"
str(alldata)

# Create a variable that shows if chla is missing
alldata$missing <- is.na(alldata$modischl_mean)*1
str(alldata)

### make NAs 0 to find max number
alldata$modischl_mean[is.na(alldata$modischl_mean)] <- 0

### get Max parameter number to add to top of color bar
maxDepth <- max(alldata$modischl_mean)
minDepth <- min(alldata$modischl_mean)
# Write the Data to CSV File
write.table(alldata, file=paste0("C:/Sources/datafiles/LobsterDepth_",substr(infile,start=17,stop=19),"07.csv"),sep=",",row.names=F)

etopoJuly07 <- modischl
#####################################################################################

# set limits of the map

# ### For East Coast
# xlim <- c(-78, -67)
# ylim <- c(32,44)

### For Lobsters
range(lon)
range(lat)
xlim <- c(-75, -66.5)
ylim <- c(40,45.5)




################ Chlorophyll ################

# get outline data for map
w <- map_data("worldHires", ylim = ylim, xlim = xlim)
# plot using ggplot
z<-ggplot(alldata2,aes(x=lon,y=lat)) +
  geom_point(aes(colour=`modischl_mean`,shape=factor(missing)),size=2.) + ### need backticks for spaces in variables ``
  scale_shape_manual(values=c(19,1))
z + geom_polygon(data = w, aes(x=long, y = lat, group = group), fill = "grey80") +
  theme_bw() +
  scale_colour_gradient(limits=c(0.,maxchl),low = "#F8EE1F",high = "#F81F1F","Chl-a") +
  coord_fixed(1.3,xlim = xlim, ylim = ylim) + ggtitle("Mean chl-a Values at Lobster Landings, Oct, 2007")

######################################################################################################


################ Bathymetry ################
# get outline data for map
w <- map_data("state", ylim = ylim, xlim = xlim)

# plot using ggplot
z<-ggplot(alldata,aes(x=lon,y=lat)) +
  geom_point(aes(colour=`modischl_mean`),size=2.) + ### need backticks for spaces in variables ``
  scale_shape_manual(values=c(19,1))
z + geom_polygon(data = w, aes(x=long, y = lat, group = group), fill = "grey80") +
  theme_bw() +
  scale_colour_gradient(limits=c(minDepth,maxDepth),"Depth (M)") +
  coord_fixed(1.3,xlim = xlim, ylim = ylim) + ggtitle("Bathymetry at Lobster Landings, July, 2007")



###### Clean up before next run ###### 

rm(modischl)
rm(alldata)
rm(tagdata)
rm(dataInfo)
rm(parameter)
rm(infile)
