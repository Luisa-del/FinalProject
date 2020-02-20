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



install.packages("wind_quikscat_grid.RData")
wind = wind.quik$data
wind.jan =  wind %>% filter(time == dmy(160100))

## ## overlay wind vector
mapDirectionField(longitude =wind.jan$lon, 
                  latitude = wind.jan$lat, 
                  u = wind.jan$y_wind, 
                  v = wind.jan$x_wind, 
                  scale = 0.02, 
                  length = 0.1)

