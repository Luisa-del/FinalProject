##############################################################################################################################
# R Script of Final Project ##################################################################################################
##############################################################################################################################

## load necessary packages----------------------------------------------------------------------------------------------------

library(oceanmap)
library(oce)
library(ocedata)
library(ncdf4)
library(tidyverse)
library(lubridate)



##############################################################################################################################
# Elevation ##################################################################################################################
##############################################################################################################################

## Bauer 2017, p.16/17 ########################################################################################################

## define extent of SCS
lon <- c(95, 125)
lat <- c(-10, 25)

# get bathymetry data 
par(mfrow = c(2,1))
bathy <- get.bathy(lon=lon, lat=lat, terrain=T, res=3, keep=T, visualize=T, subplot=FALSE, grid=F, cbpos="r")

# add figure (optional)
figure(width = 9.75, height = 5.28)
v(bathy, param="bathy", subplot=TRUE, terrain=T, levels=c(200 ,2000), cbpos="r", main="South China Sea") # change map by adding parameters





##############################################################################################################################
# SST SCS ##################################################################################################################
##############################################################################################################################

## Mapping with OCE ########################################################################################################

## load oceanographic datasets

data("coastlineWorld") #low resolution suitable for global-scale maps plotted at a small size, e.g. inset diagrams
data("coastlineWorldMedium") #moderate resolution suitable for global- or regional-scale maps
data("coastlineWorldFine") #high resolution suitable for shelf-scale maps

# SST of SCS---------------------------------------------------------------------------------------------------------------

## define area, margin of the plot

par(mar=c(2, 2, 1, 1))
lonlim_scs <- c(95, 125)
latlim_scs <- c(-10, 25)
Zlim_scs = c(18,30)

## load SST data
nc_scs = nc_open("/home/luisa/Documents/EAGLE_Master/MB2_Programming_Geostatistics/FinalProject/MappingWithOCE/Data/opendap_SST_GHRSST_20110101//20110101-NCDC-L4LRblend-GLOB-v01-fv02_0-AVHRR_AMSR_OI.nc.bz2.nc4")

## optional: add figure for active plotting
figure(width = 9.75, height = 5.28)

## draw scalebar
drawPalette(zlim = Zlim_scs, 
            col=oce.colorsJet, 
            at = seq(18,30), 
            pos = 4, 
            drawTriangles = FALSE)

## make a base
mapPlot(coastlineWorld, 
        projection="+proj=longlat +datum=WGS84 +no_defs",
        col="lightgray", 
        longitudelim=lonlim_scs, 
        latitudelim=latlim_scs, 
        main = "South China Sea", #add  title 
        clip = TRUE)

## extract the longitude varibale
lon = ncvar_get(nc_scs, "lon")
## extract the latitude varibale
lat = ncvar_get(nc_scs, "lat")
## extract sst variable
sst = ncvar_get(nc_scs, "analysed_sst")

## Kelvin to Degree celcius and calibrate
sst = sst-273.149993896484

## overlay gridded sst layer
mapImage(longitude = lon,
         latitude = lat, 
         z = sst, 
         zlim = Zlim_scs,
         col = oceColorsJet(120))

## add contour
mapContour(longitude = lon,
           latitude = lat, 
           z = sst, 
           levels =  seq(15,30,2),
           col = "black",
           lwd = .85,lty = 1)

## add grid             --> edit
mapGrid(dlongitude = 8, 
        dlatitude = 8, 
        lty = 3, lwd = .5)

## add polygon  
mapPolygon(coastlineWorld, 
           col="lightgray")

## add text
mapScalebar(x = "topleft", y = NULL, length = 1000, col = "black")






##############################################################################################################################
# Wind #######################################################################################################################
##############################################################################################################################


# load necessary packages 

library(rWind)
library(sp)
library(raster)
library(rworldmap) 
library(shape)


# Download and generate R-Object of wind for Study Area (SCS) at e.g 2015, February 12, 00:00
wind_data_scs<-wind.dl(2015,12,12,0,95,125,-10,25)
head(wind_data_scs)

#Spain region################################
#wind_data<-wind.dl(2015,2,12,0,-10,5,35,45)
#head(wind.data)
#############################################


# generate direction and speed objects
r_dir_scs <- wind_data_scs$dir
r_speed_scs <- wind_data_scs$speed

windraster_scs <- wind2raster(wind_data_scs)
head(windraster_scs)

#Spain region########################
#r_dir <- wind_data$dir
#r_speed <- wind_data$speed
#windraster <- wind2raster(wind_data)
#head(windraster)
#####################################


# Use rworldmap package to plot countries contourns with your direction and speed data!
newmap <- getMap(resolution = "low")

par(mfrow=c(1,2))

plot(windraster_scs$direction, main="direction")
lines(newmap, lwd=4)

plot(windraster_scs$speed, main="speed")
lines(newmap, lwd=4)

# Spain Region ################################
#newmap <- getMap(resolution = "low")
#par(mfrow=c(1,2))
#plot(windraster$direction, main="direction")
#lines(newmap, lwd=4)
#plot(windraster$speed, main="speed")
#lines(newmap, lwd=4)
###############################################


# Use arrowDir and Arrowhead (from "shape" package) functions to plot wind direction over a raster graph

dev.off()
alpha<- arrowDir(wind_data_scs)
figure(width = 9.75, height = 5.28)
plot(windraster_scs$speed, main="wind direction (arrows) and speed (colours)")
lines(newmap, lwd=4)
Arrowhead(wind_data_scs$lon, wind_data_scs$lat, angle=alpha, arr.length = 0.12, arr.type="curved")

#Spain Region ############################################################################
#dev.off()
#alpha<- arrowDir(wind_data)
#plot(windraster$speed, main="wind direction (arrows) and speed (colours)")
#lines(newmap, lwd=4)
#Arrowhead(wind_data$lon, wind_data$lat, angle=alpha, arr.length = 0.12, arr.type="curved")
##########################################################################################



##############################################################################################################################
# Chlorophyll #######################################################################################################################
##############################################################################################################################






