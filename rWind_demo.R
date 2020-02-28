setwd("/home/luisa/Desktop/")
getwd()

# Download and install "rWind" package from CRAN:
install.packages("rWind")
# You should install also "raster" package if you do not have it 

library(rWind)
library(sp)
library(raster)

packageDescription("rWind")
help(package="rWind")

# "rWind" is a package with several tools for downloading, editing and transforming wind data from Global Forecast 
# System (GFS, see <https://www.ncdc.noaa.gov/data-access/model-data/model-datasets/global-forcast-system-gfs>) of the USA's
# National Weather Service (NWS, see <http://www.weather.gov/>).

citation("rWind")

# > Javier Fernández-López (2016). rWind: Download, Edit and Transform Wind Data from GFS. R package version 0.1.3.
# > https://CRAN.R-project.org/package=rWind

# First, we can download a wind dataset of a specified date from GFS using wind.dl function
# help(wind.dl)

# Download wind for Spain region at 2015, February 12, 00:00
help(wind.dl)

wind.dl(2015,2,12,0,-10,5,35,45)

# By default, this function generates an R object with downloaded data. You can store it...

wind_data<-wind.dl(2015,2,12,0,-10,5,35,45, type = "read-data", trace = 1)
wind_data<-wind.dl(2015,2,12,0,-10,5,35,45)
# SCS: wind_data<-wind.dl(2015,12,12,0,95,125,-10,25, type = "read-data", trace = 1)

head(wind_data)

# or download into your work directory a CVS file with the data using type="csv" argument:

getwd()
wind.dl(2015,2,12,0,-10,5,35,45, type="csv")

# If you inspect inside wind_data object, you can see that data are organized in a weird way, with
# to rows as headers, a column with date and time, longitude data expresed in 0/360 notation and wind
# data defined by the two vector components U and V. You can transform in a much more nice format these 
# data using "wind.fit" function:
#help(wind.fit)

wind_data<-wind.fit(wind_data)

head(wind_data)

# Now, data are organized by latitude, with -180/180 and U and V vector components are transformed
# into direction and speed. You can export the data.frame as an CVS file to be used with a GIS software

write.csv(wind_data, "wind_data.csv")

# Once that you have data organized by latitude and you have direction and speed information fields,
# you can use it to create a raster layer with wind2raster function to be used by GIS software or to be ploted 
# in R, for example.
# As raster layer can only store one information field, you should choose between direction (type="dir")
# or speed (type="speed").

r_dir <- wind2raster(wind_data, type="dir")
r_speed <- wind2raster(wind_data, type="speed") 

# Now, you can use rworldmap package to plot countries contourns with your direction and speed data!

#install.packages("rworldmap")
library(rworldmap) 
newmap <- getMap(resolution = "low")

par(mfrow=c(1,2))

plot(r_dir, main="direction")
lines(newmap, lwd=4)

plot(r_speed, main="speed")
lines(newmap, lwd=4)

# Additionally, you can use arrowDir and Arrowhead (from "shape" package) functions to plot wind direction
# over a raster graph:

#install.packages("shape")
library(shape) 

dev.off()
alpha<- arrowDir(wind_data)
plot(r_speed, main="wind direction (arrows) and speed (colours)")
lines(newmap, lwd=4)
Arrowhead(wind_data$lon, wind_data$lat, angle=alpha, arr.length = 0.12, arr.type="curved")

# If you want a time series of wind data, you can download it by using a for-in loop:
# First, you should create an empty list where you will store all the data

wind_serie<- list()

# Then, you can use a wind.dl inside a for-in loop to download and store wind data of 
# the first 5 days of February 2015 at 00:00 in Europe region. It could take a while...
 
for (d in 1:5){
  w<-wind.dl(2015,2,d,0,-10,30,35,70)
  wind_serie[[d]]<-w
}

wind_serie

# Finally, you can use wind.mean function to calculate wind average 

wind_average<-wind.mean(wind_serie)
wind_average<-wind.fit(wind_average)
r_average_dir<-wind2raster(wind_average, type="dir")
r_average_speed<-wind2raster(wind_average, type="speed")
  
  par(mfrow=c(1,2))

plot(r_average_dir, main="direction average")
lines(newmap, lwd=1)

plot(r_average_speed, main="speed average")
lines(newmap, lwd=1)








# Download wind for Iberian Peninsula region at 2015, February 12, 00:00
## Not run: 

wind.dl_2("2018/3/15 9:00:00",-10,5,35,45)


library(lubridate)
dt <- seq(ymd_hms(paste(2018,1,1,00,00,00, sep="-")),
          ymd_hms(paste(2018,1,2,21,00,00, sep="-")),by="3 hours")
ww <- wind.dl_2(dt,-10,5,35,45)
tidy (ww)


## End(Not run)
