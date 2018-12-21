
##################################################################################
################ Get Files in a folder as xls and convert to xlsx ################
##################################################################################


PKG <- c("readxl","data.table","openxlsx", "dplyr","tidyr","magrittr","zoo",
         "openxlsx","SASxport","foreign","rio","tcltk2","geosphere", "chron")

for (p in PKG) {
  if(!require(p,character.only = TRUE)) {  
    install.packages(p)
    require(p,character.only = TRUE)}
}

## Get timestamps
tclTaskSchedule(1000, {options(prompt=paste(Sys.time(),"> "))}, id = "ticktock", redo = TRUE)

#######################################################################################################
### Fix Flight Data ###
# abun <- read.csv("C:/Users/Dennis.Corvi/Desktop/Abundance rasters/All_Months_CellID.csv",as.is = TRUE)


csv <- read.csv("C:/Sources/DCorvi/NARWProject/FlightData/narwsurvey_4dennis_Plane.csv",as.is = TRUE)
str(csv)
csv1 <- csv
## reset
csv <- csv1

# csv$DATETIME <- strptime(paste(csv$DATE_LOCAL, " ", csv$TIME_LOCAL),format= "%d-%b-%y %H:%M:%S")
# csv$DATETIME <- as.Date(paste(csv$DATE_LOCAL, " ", csv$TIME_LOCAL),format= "%d-%b-%y %H:%M:%S")

### Add datetime field
csv$Datetime <- as.POSIXct(paste(csv$DATE_LOCAL, " ", csv$TIME_LOCAL),format= "%d-%b-%y %H:%M:%S")


### convert string dates to date type
csv %<>%
  mutate(DATE_LOCAL= as.Date(DATE_LOCAL, format= "%d-%b-%y"))


### Sort event numbers based on day and plane
csv2 <- csv[order(csv$DATE_LOCAL,csv$PLANE,csv$EVENT_NUMBER),]

# str(csv2)

### Get NA Count
colSums(is.na(csv2))
# NACount <- as.data.frame(colSums(is.na(csv2)))

# 
# #### Turn 0 lat/lons to NAs - Should keep all raw data
# is.na(csv2$lat) <- !csv2$lat
# is.na(csv2$lon) <- !csv2$lon
# 
# ## remove NAs lat/lons - cant run twice
# completeFun <- function(data, desiredCols) {
#   completeVec <- complete.cases(data[, desiredCols])
#   return(data[completeVec, ])
# }
# csv2 <- completeFun(csv2, "lat")

### Instead replace NAs with 0s
csv2$lat[is.na(csv2$lat)] <- 0
csv2$lon[is.na(csv2$lon)] <- 0
dim(csv2)

### Create new lat/lon columns with 0s replaced with next available coordinates
csv2$lat2 <- csv2$lat
csv2$lon2 <- csv2$lon
# 
# index <- df$b == 0
# index <- csv2$lat2 == 0 & csv2$EVENT_NUMBER == 1
# df$est[index] <- (df$a[index] - 5)/2.533 
# 
# csv2$EVENT_NUMBER

is.na(csv2$lat2) <- csv2$lat2 == 0
csv2$lat2        <- na.locf(csv2$lat2,na.rm=FALSE,fromLast=TRUE)
is.na(csv2$lon2) <- csv2$lon2 == 0
csv2$lon2        <- na.locf(csv2$lon2,na.rm=FALSE,fromLast=TRUE)

# ### Create new lat/lon columns with 0s replaced with last available coordinates
# is.na(csv2$lat2) <- csv2$lat2 == 0
# csv2$lat2        <- na.locf(csv2$lat2,na.rm=FALSE)
# is.na(csv2$lon2) <- csv2$lon2 == 0
# csv2$lon2        <- na.locf(csv2$lon2,na.rm=FALSE)



## Get dataframe with only lat/lons
pts <- csv2[c("lon2", "lat2")]
## Pass in two derived data.frames that are lagged by one point

segDists <- distVincentyEllipsoid(p1 = pts[-nrow(csv2),], 
                                  p2 = pts[-1,])

# add the 0 to distance 1 and add distances to dataframe
segDists <- c(0, segDists)
csv2$Dist <- segDists

## Convert distance column to nautical miles
csv2$Dist <- csv2$Dist * 1852

## Conversions for sums
metersPerMile <- 1609.34
metersPerNauticalMile <- 1852
### View total distance in miles and nautical miles
sum(segDists, na.rm = TRUE)/metersPerMile
sum(segDists, na.rm = TRUE)/metersPerNauticalMile


# add the 0 to distance 1 and add distances to dataframe
# a <- segDists
# a <- c(0, a)
# csv2$Dist <- a
# segDists <- c(0, segDists)
# csv2$Dist <- segDists

## Add unqiue id
csv2$DataID <- seq(1,nrow(csv2))

## Add unique id for flights
csv2$FlightID <- as.integer(factor(with(csv2, paste(csv2$DATE_LOCAL,csv2$PLANE))))

## Get max and min dates
format(min(csv2$DATE_LOCAL), "%d_%b_%y")
format(max(csv2$DATE_LOCAL), "%d_%b_%y")
### Get number of NAs for each column
colSums(is.na(csv2))

str(csv2)
# ### Turn nulls to 0
# csv2[is.na(csv2)] <- 0

### Add column of incrementing consecutive zero lats to add to lat/lons
csv2$lat0 <- ave(!csv2$lat, cumsum(csv2$lat), FUN = cumsum)

### Add column of incrementing consecutive zero distances to add to lat/lons
csv2$AddTo <- ave(!csv2$Dist, cumsum(csv2$Dist), FUN = cumsum)

# csv2$AddTo <- ave(!csv2$Dist, cumsum(csv2$Dist), FUN = cumsum)
# csv2$Dist2 <- !csv2$Dist
# csv2$Dist3 <- ?cumsum(csv2$Dist)
# 
# csv2$index <- is.na(csv2$EVENT_NUMBER)
# csv2$Dist4[index] <- (csv2$EVENT_NUMBER[index] + 1)

### Change the first row of AddTo column to 0
csv2[1,grep("^AddTo$", colnames(csv2))] <- 0

### modify lat/lon percision to add - may need to make absolute value
csv2$AddTo <- csv2$AddTo * 0.00000001

str(csv2)

drops <- c("Dist2","Dist3","index","Dist4")
csv2 <- csv2[ , !(names(csv2) %in% drops)]

csv2$EVENT_NUMBER2 <- csv2$EVENT_NUMBER
is.na(csv2$EVENT_NUMBER2) <- csv2$EVENT_NUMBER2 == 0
csv2$EVENT_NUMBER2  <- na.locf(csv2$EVENT_NUMBER2,na.rm=FALSE,fromLast=FALSE)


# csv2$index <- csv2$EVENT_NUMBER == csv2$lat0
# df$est[index] <- (df$a[index] - 5)/2.533 

setwd('C:/Sources/DCorvi/NARWProject/FlightData/') 

write.csv(csv2, paste0("narwflt_",format(min(csv2$DATE_LOCAL), "%y"),"_",format(max(csv2$DATE_LOCAL), "%y"),"_2.csv"),row.names = FALSE, na = "")
write.csv(csv2, "narwflt_02_14_3.csv",row.names = FALSE, na = "")

data <- data.table( Col1 = 1:5, Col2 = letters[1:5] )
data2 <- data.table( Col1= 1:5, Col2= letters[1:5], Col3= c("NA", "NA", "3", "3", "3"))


x <- c(1,2,3,4,5,NA,NA,1,2,3,NA,NA,1,2,3,4)
goodIdx <- !is.na(x)
goodVals <- c(NA, x[goodIdx])
fillIdx <- cumsum(goodIdx)+1
goodVals[fillIdx]

is.na(csv2$EVENT_NUMBER) <- csv2$lat2 == 0
csv2$lat2        <- na.locf(csv2$lat2,na.rm=FALSE,fromLast=TRUE)


##### DONE ###