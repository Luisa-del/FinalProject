##############################################################################################################################
#R Script to Build Animation of Arctic Sea Ice Extent#########################################################################
##############################################################################################################################

# Script: URL: https://semba-blog.netlify.com/02/13/2019/mapping-with-oce/


#load necessary packages---------------------------------------------------------------------------------------------

library(oce)
library(ocedata)
library(ncdf4)
library(tidyverse)
library(lubridate)


#load oceanographic datasets-----------------------------------------------------------------------------------------

data("coastlineWorld") #low resolution suitable for global-scale maps plotted at a small size, e.g. inset diagrams
data("coastlineWorldMedium") #moderate resolution suitable for global- or regional-scale maps
data("coastlineWorldFine") #high resolution suitable for shelf-scale maps


#####################################################################################################################
## BASEMAP ##########################################################################################################
#####################################################################################################################

#create basemap with plotMap() function------------------------------------------------------------------------------

#--> Figure 1: The Western Indian Ocean Region - study area basemap

## define the margin of the plot
par(mar=c(2, 2, 1, 1))

## ...
lonlim <- c(35, 55)
latlim <- c(-35, 2)

## make a base
mapPlot(coastlineWorldFine, 
        projection="+proj=moll",
        col="lightgray", 
        longitudelim=lonlim, 
        latitudelim=latlim)


# add points on the base with a mapPoints() function------------------------------------------------------------------

#--> Figure 2: Map of the Western Indian Ocean region showing experimental sites 

## create points name with their respective locations
stations = data.frame(name = c("Stn1", "Stn2", "Stn3"),
                      lon = c(45,41.2,41.3),
                      lat = c(-25,-11,-3))

## overlay the point layer
mapPoints(longitude = stations$lon,
          latitude = stations$lat, 
          pch = 20, 
          cex = 1.75)


# add labels of station names on the base with a mapPoints() function------------------------------------------------

#--> Figure 3: Map of the Western Indian Ocean region showing experimental sites with labels

## add station name on the map
mapText(longitude = stations$lon+1.5,
        latitude = stations$lat+1.5,
        labels = stations$name)


#####################################################################################################################
## SEA SURFACE TEMPERATURE ##########################################################################################
#####################################################################################################################


# Load the netcdf SST data using the nc_open() function---------------------------------------------------------------

#first download the SST data from platform, e.g. https://opendap.jpl.nasa.gov/opendap/ 
#in this case 20110101: https://opendap.jpl.nasa.gov/opendap/OceanTemperature/ghrsst/data/L4/GLOB/NCDC/AVHRR_AMSR_OI/2011/001/20110101-NCDC-L4LRblend-GLOB-v01-fv02_0-AVHRR_AMSR_OI.nc.bz2.html

nc = nc_open("/home/luisa/Documents/EAGLE_Master/MB2_Programming_Geostatistics/FinalProject/MappingWithOCE/Data/opendap_SST_GHRSST_20110101//20110101-NCDC-L4LRblend-GLOB-v01-fv02_0-AVHRR_AMSR_OI.nc.bz2.nc4")
nc


# extract variables longitude, latitude and temperature---------------------------------------------------------------

# Metadata show how the variables are written
# use the metadata information to extract variables longitude, latitude and temperature
# Also the information in the metadata is used to calibrate the temperature values—from Kelvin to degree Celcius scale


## extract the longitude varibale
lon = ncvar_get(nc, "lon")
## extract the latitude varibale
lat = ncvar_get(nc, "lat")
## extract sst variable
sst = ncvar_get(nc, "analysed_sst")

## Kelvin to Degree celcius and calibrate
sst = sst-273.149993896484


# add SST layer on the basemap with mapImage() function, but before add scalebar with drawPalette() function and make the basemap------------------

#--> Figure 4-8: Map of the Western Indian Ocean region showing distribution of sea surface temperature. Solid line are contours 

par(mar=c(2, 2, 1, 1))
Zlim = c(15,30)

## draw scalebar
drawPalette(zlim = Zlim, 
            col=oce.colorsJet, 
            at = seq(16,30,2), 
            pos = 4, 
            drawTriangles = FALSE)
## make a base
mapPlot(coastlineWorld, 
        projection="+proj=moll",
        col="lightgray", 
        longitudelim=lonlim, 
        latitudelim=latlim, 
        clip = TRUE)
## overlay gridded sst layer
mapImage(longitude = lon,
         latitude = lat, 
         z = sst, 
         zlim = Zlim,
         col = oceColorsJet(120))


# add the missing country boundary with mapPolygon() function------------------------------------------------------------------

## add polygon
mapPolygon(coastlineWorld, 
           col="lightgray")


# add contour lines with mapContour() function, grid with the mapGrid() function and text with mapScalebar() function----------

## add contour
mapContour(longitude = lon,
           latitude = lat, 
           z = sst, 
           levels =  seq(15,30,2),
           col = "black",
           lwd = .85,lty = 1)
## add grid
mapGrid(dlongitude = 8, 
        dlatitude = 8, 
        lty = 3, lwd = .5)
## add text
mapScalebar(x = "topleft", y = NULL, length = 1000, col = "black")


# generate more local map by adjusting the limit of longitude and latitude and Zlim---------------------------------------

## adjust the spatial extent to local area
lonlim = c(38.0, 41)
latlim = c(-7,-4)
Zlim = c(27,30)

par(mar=c(2, 2, 1, 1))

## draw scalebar
drawPalette(zlim = Zlim, 
            col=oce.colorsJet, 
            at = seq(27,30,0.5), 
            pos = 4, 
            drawTriangles = FALSE)
## make a base
mapPlot(coastlineWorld, 
        projection="+proj=moll",
        col="lightgray", 
        longitudelim=lonlim, 
        latitudelim=latlim, 
        clip = TRUE)
## ## overlay gridded sst layer
mapImage(longitude = lon,
         latitude = lat, 
         z = sst, 
         zlim = Zlim,
         filledContour = FALSE,
         col = oceColorsJet(120))
## ## overlay gridded sst layer
mapContour(longitude = lon,
           latitude = lat, 
           z = sst, 
           levels =  seq(27,30,.5),lty = 3,
           col = "black",
           lwd = 1.25)
## add polygon
mapPolygon(coastlineWorldMedium, 
           col="lightgray")



#####################################################################################################################
## South China Sea ##################################################################################################
#####################################################################################################################

setwd("/home/luisa/Documents/EAGLE_Master/MB2_Programming_Geostatistics/FinalProject/")

#load necessary packages

library(oce)
library(ocedata)
library(ncdf4)
library(tidyverse)
library(lubridate)

#load oceanographic datasets

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


# Bathymetry of SCS---------------------------------------------------------------------------------------------------------------
#URL: https://semba-blog.netlify.com/02/14/2019/mapping-the-bathmetry-of-coastal-tanzania-from-etopo1-with-oce-package-in-r/ 
#!!!funktioniert bisher noch nicht!!!!

#set working directory
setwd("/home/luisa/Documents/EAGLE_Master/MB2_Programming_Geostatistics/FinalProject/MappingWithOCE")

#load necessary packages

library(tidyverse)
library(oce)
library(ocedata)

#import downloaded ascii-file
etopo = sp::read.asciigrid("/home/luisa/Documents/EAGLE_Master/MB2_Programming_Geostatistics/FinalProject/MappingWithOCE/Gitignore_temp/etopo1_scs.asc")
etopo = read.asciigrid("etopo1_scs.asc") #error: dimensions of map do not match that of header?!

###-ERROR-###-kann nicht weiter machen...

#import downloaded tiff-file

library(raster)
library(tiff)


etopo_scs<-read.asciigrid('etopo1_scs.tif')
imported_raster=raster(etopo_scs)
imported_raster

etopo_scs.m <- as.matrix(imported_raster)
etopo_scs.m
head(etopo_scs.m)

###-ERROR-funktioniert auch alles nicht :(-#####################

## set the extent
par(mar=c(2, 2, 1, 1))
lonlim_scs <- c(95, 125)
latlim_scs <- c(-10, 25)
Zlim_scs.b = c(-6000, 0)


## draw scalebar
drawPalette(zlim = Zlim_scs.b, 
            col=oce.colorsGebco(120), 
            at = seq(-6000,0,1000), 
            pos = 4, 
            drawTriangles = FALSE)

## make a base
mapPlot(coastlineWorld, 
        projection="+proj=longlat +datum=WGS84 +no_defs",
        col="lightgray", 
        longitudelim=lonlim_scs, 
        latitudelim=latlim_scs, 
        clip = TRUE)

## overlay gridded sst layer
mapImage(longitude = lon,
         latitude = lat, 
         z = etopo_scs, #muss ich irgendwie nich erstellen...
         zlim = Zlim_scs.b,
         filledContour = TRUE, 
         # zclip = TRUE,
         col = oceColorsGebco(120))

## add polygon
mapPolygon(coastlineWorldFine, 
           col="lightgray")

#####################################################################################################################
## Wind Map #########################################################################################################
#####################################################################################################################

setwd("/home/luisa/Documents/EAGLE_Master/MB2_Programming_Geostatistics/FinalProject/")

ncw = nc_open("/home/luisa/Documents/EAGLE_Master/MB2_Programming_Geostatistics/FinalProject/MappingWithOCE/Data/Winddata/QS_S1B60074.20110011246.gz.nc4")
ncw
### --> need data ?!
data("ncw")
data("wind_quikscat_grid")
wind = wind.quik$data
wind.jan =  wind %>% filter(time == dmy(160100))

## ## overlay wind vector
mapDirectionField(longitude =wind.jan$lon, 
                  latitude = wind.jan$lat, 
                  u = wind.jan$y_wind, 
                  v = wind.jan$x_wind, 
                  scale = 0.02, 
                  length = 0.1)


## set the extent
lonlim = c(45.0, 55)
latlim = c(-35,2)
Zlim = c(-6000,0)

par(mar=c(2, 2, 1, 1))


## draw scalebar
drawPalette(zlim = Zlim, 
            col=oce.colorsGebco(120), 
            at = seq(-6000,0,1000), 
            pos = 4, 
            drawTriangles = FALSE)
## make a base
mapPlot(coastlineWorld, 
        projection="+proj=mill",
        col="lightgray", 
        longitudelim=lonlim, 
        latitudelim=latlim, 
        clip = TRUE)
## ## overlay gridded sst layer
mapImage(longitude = lon,
         latitude = lat, 
         z = etopo.mat,  ##?? where to get?             
         zlim = Zlim,
         filledContour = TRUE, 
         # zclip = TRUE,
         col = oceColorsGebco(120))
## add polygon
mapPolygon(coastlineWorldFine, 
           col="lightgray")
