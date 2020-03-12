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
# Chlorophyll ################################################################################################################
##############################################################################################################################

#...

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


# load necessary packages -----------------------------------------------------------------------------------------------------

library(rWind)
library(sp)
library(raster)
library(rworldmap) 
library(shape)
library(ggplot2)
library(gganimate)
library(cowplot)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)


# Download and generate R-Object of wind for Study Area (SCS) at 2019, Feb/May/Aug/Nov 12, 00:00 ------------------------------

#feb
wind_data_feb<-wind.dl(2019,02,12,0,95,125,-10,25)
#may
wind_data_may<-wind.dl(2019,05,12,0,95,125,-10,25)
#aug
wind_data_aug<-wind.dl(2019,08,12,0,95,125,-10,25)
#nov
wind_data_nov<-wind.dl(2019,11,12,0,95,125,-10,25)



# extract values for ggplot ---------------------------------------------------------------------------------------------------

#feb
longitude_feb = wind_data_feb$lon
latitude_feb = wind_data_feb$lat
u_feb = wind_data_feb$ugrd10m
v_feb = wind_data_feb$vgrd10m

#may
longitude_may = wind_data_may$lon
latitude_may = wind_data_may$lat
u_may = wind_data_may$ugrd10m
v_may = wind_data_may$vgrd10m

#aug
longitude_aug = wind_data_aug$lon
latitude_aug = wind_data_aug$lat
u_aug = wind_data_aug$ugrd10m
v_aug = wind_data_aug$vgrd10m

#feb
longitude_nov = wind_data_nov$lon
latitude_nov = wind_data_nov$lat
u_nov = wind_data_nov$ugrd10m
v_nov = wind_data_nov$vgrd10m



# create polygon of countries to add to plot ----------------------------------------------------------------------------------

world <- ne_countries(scale = "medium", returnclass = "sf")



# ggplot windmaps -------------------------------------------------------------------------------------------------------------

#feb
wind.vector_feb = ggplot(data = world) +
        geom_raster(data = wind_data_feb, aes(x = longitude_feb, y = latitude_feb, fill = wind_data_feb$speed))+
        geom_segment(data = wind_data_feb, 
                     aes(x = longitude_feb, xend = longitude_feb+u_feb/60, y = latitude_feb, 
                         yend = latitude_feb+v_feb/60), arrow = arrow(length = unit(0.1, "cm")))+
        geom_sf(data = NULL, fill = "grey85", col = 1)+
        coord_sf(xlim = c(95, 125), ylim =  c(-10, 25))+
        scale_fill_gradientn(colours = oce::oceColorsPalette(120), limits = c(0,12), 
                             na.value = "white", name = "Speed\n (m/s)")+
        scale_x_continuous(breaks = c(95.5, 125))+
        theme_bw()+
        theme(axis.text = element_text(size = 14, colour = 1),
              legend.text = element_text(size = 14, colour = 1), 
              legend.title = element_text(size = 14, colour = 1),
              legend.position = c(.12,.17),
              legend.background = element_rect(colour = 1, fill = "white"))+
        labs(x = NULL, y = NULL, title = "SCS Wind Map - February 2019")


#may
wind.vector_may = ggplot(data = world) +
        geom_raster(data = wind_data_may, aes(x = longitude_may, y = latitude_may, fill = wind_data_may$speed))+
        geom_segment(data = wind_data_may, 
                     aes(x = longitude_may, xend = longitude_may+u_may/60, y = latitude_may, 
                         yend = latitude_may+v_may/60), arrow = arrow(length = unit(0.1, "cm")))+
        geom_sf(data = NULL, fill = "grey85", col = 1)+
        coord_sf(xlim = c(95, 125), ylim =  c(-10, 25))+
        scale_fill_gradientn(colours = oce::oceColorsPalette(120), limits = c(0,12), 
                             na.value = "white", name = "Speed\n (m/s)")+
        scale_x_continuous(breaks = c(95.5, 125))+
        theme_bw()+
        theme(axis.text = element_text(size = 14, colour = 1),
              legend.text = element_text(size = 14, colour = 1), 
              legend.title = element_text(size = 14, colour = 1),
              legend.position = c(.12,.17),
              legend.background = element_rect(colour = 1, fill = "white"))+
        labs(x = NULL, y = NULL, title = "SCS Wind Map - May 2019")

#aug
wind.vector_aug = ggplot(data = world) +
        geom_raster(data = wind_data_aug, aes(x = longitude_aug, y = latitude_aug, fill = wind_data_aug$speed))+
        geom_segment(data = wind_data_aug, 
                     aes(x = longitude_aug, xend = longitude_aug+u_aug/60, y = latitude_aug, 
                         yend = latitude_aug+v_aug/60), arrow = arrow(length = unit(0.1, "cm")))+
        geom_sf(data = NULL, fill = "grey85", col = 1)+
        coord_sf(xlim = c(95, 125), ylim =  c(-10, 25))+
        scale_fill_gradientn(colours = oce::oceColorsPalette(120), limits = c(0,12), 
                             na.value = "white", name = "Speed\n (m/s)")+
        scale_x_continuous(breaks = c(95.5, 125))+
        theme_bw()+
        theme(axis.text = element_text(size = 14, colour = 1),
              legend.text = element_text(size = 14, colour = 1), 
              legend.title = element_text(size = 14, colour = 1),
              legend.position = c(.12,.17),
              legend.background = element_rect(colour = 1, fill = "white"))+
        labs(x = NULL, y = NULL, title = "SCS Wind Map - August 2019")


#nov
wind.vector_nov = ggplot(data = world) +
        geom_raster(data = wind_data_nov, aes(x = longitude_nov, y = latitude_nov, fill = wind_data_nov$speed))+
        geom_segment(data = wind_data_nov, 
                     aes(x = longitude_nov, xend = longitude_nov+u_nov/60, y = latitude_nov, 
                         yend = latitude_nov+v_nov/60), arrow = arrow(length = unit(0.1, "cm")))+
        geom_sf(data = NULL, fill = "grey85", col = 1)+
        coord_sf(xlim = c(95, 125), ylim =  c(-10, 25))+
        scale_fill_gradientn(colours = oce::oceColorsPalette(120), limits = c(0,12), 
                             na.value = "white", name = "Speed\n (m/s)")+
        scale_x_continuous(breaks = c(95.5, 125))+
        theme_bw()+
        theme(axis.text = element_text(size = 14, colour = 1),
              legend.text = element_text(size = 14, colour = 1), 
              legend.title = element_text(size = 14, colour = 1),
              legend.position = c(.12,.17),
              legend.background = element_rect(colour = 1, fill = "white"))+
        labs(x = NULL, y = NULL, title = "SCS Wind Map - November 2019")



cowplot::plot_grid(wind.vector_feb, wind.vector_may, wind.vector_aug, wind.vector_nov, ncol = 2, nrow = 2)













################################################################################################################################
#Schritte davor, play around ###################################################################################################
################################################################################################################################


# Download and generate R-Object of wind for Study Area (SCS) at e.g 2015, February 12, 00:00

#feb
wind_data_feb<-wind.dl(2019,02,12,0,95,125,-10,25)
#aug
wind_data_aug<-wind.dl(2019,08,12,0,95,125,-10,25)

        #falls download nicht geht, import csv.datei
        #wind_data_scs <- read.csv("/home/luisa/Documents/EAGLE_Master/MB2_Programming_Geostatistics/FinalProject/wind_2015_12_12_00_scs.csv")

head(wind_data_feb)
head(wind_data_aug)

str(wind_data_aug)


# raster data ansatz, simple plot-function-------------------------------------------------------------------------------------

# generate direction and speed objects

# feb
r_dir_feb <- wind_data_feb$dir
r_speed_feb <- wind_data_feb$speed

windraster_feb <- wind2raster(wind_data_feb)
head(windraster_feb)

# aug
r_dir_aug <- wind_data_aug$dir
r_speed_aug <- wind_data_aug$speed

windraster_aug <- wind2raster(wind_data_aug)
head(windraster_aug)



# Use rworldmap package to plot countries contourns with your direction and speed data!

# feb
newmap <- getMap(resolution = "low")
par(mfrow=c(1,2))

plot(windraster_feb$direction, main="direction")
#lines(newmap, lwd=4)
plot(windraster_feb$speed, main="speed")
#lines(newmap, lwd=4)

# aug
newmap <- getMap(resolution = "low")
par(mfrow=c(1,2))

plot(windraster_aug$direction, main="direction")
#lines(newmap, lwd=4)
plot(windraster_aug$speed, main="speed")
#lines(newmap, lwd=4)



# Use arrowDir and Arrowhead (from "shape" package) functions to plot wind direction over a raster graph

# feb
dev.off()
alpha_feb<- arrowDir(wind_data_feb)
#figure(width = 9.75, height = 5.28)
plot(windraster_feb$speed, main="wind direction (arrows) and speed (colours)")
#lines(newmap, lwd=4)
Arrowhead(wind_data_feb$lon, wind_data_feb$lat, angle=alpha_feb, arr.length = 0.05, arr.type="curved")
polygon

# aug
dev.off()
alpha_aug<- arrowDir(wind_data_aug)
#figure(width = 9.75, height = 5.28)
plot(windraster_aug$speed, main="wind direction (arrows) and speed (colours)")
#lines(newmap, lwd=4)
Arrowhead(wind_data_aug$lon, wind_data_aug$lat, angle=alpha_aug, arr.length = 0.05, arr.type="curved", lty = 1)



# play around------------------------------------------------------------------------------------------------------------------

#create data.frames, but actually not necessary since wind.dl is already data frame
#speed_aug <- as.data.frame(windraster_aug$speed)
#direction_aug <- as.data.frame(windraster_aug$direction)



#ggplot(wind_data_feb)


direction_feb <- wind_data_feb$dir
str(direction_feb)
str(wind_data_feb)


# extract values for ggplot----------------------------------------------------------------------------------------------------

#feb
longitude_feb = wind_data_feb$lon
latitude_feb = wind_data_feb$lat
u_feb = wind_data_feb$ugrd10m
v_feb = wind_data_feb$vgrd10m
#aug
longitude_aug = wind_data_aug$lon
latitude_aug = wind_data_aug$lat
u_aug = wind_data_aug$ugrd10m
v_aug = wind_data_aug$vgrd10m


# create polygon of countries to add to plot

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

studyarea <- data.frame(longitude = c(95, 125), latitude = c(-10, 25)) #no data, combine somehow woth world
class(studyarea)




## ggplot

feb = ggplot(data = world) + #adds country polygon, but need to crop a little and add labels
        geom_segment(data = wind_data_feb, 
                     aes(x = longitude_feb, xend = longitude_feb+u_feb/60, y = latitude_feb, 
                         yend = latitude_feb+v_feb/60), arrow = arrow(length = unit(0.1, "cm")))+
        geom_sf(data = NULL, fill = "grey85", col = 1)+ # I changed from data=tz.ke to data=NULL because of ERROR
        coord_sf(xlim = c(95, 125), ylim =  c(-10, 25))+
        scale_x_continuous(breaks = c(95.5, 125))+
        theme_bw()+
        theme(axis.text = element_text(size = 11, colour = 1))+
        labs(x = NULL, y = NULL)

aug = ggplot(data = world) + #adds country polygon, but need to crop a little and add labels
        geom_segment(data = wind_data_aug, 
                     aes(x = longitude_aug, xend = longitude_aug+u_aug/60, y = latitude_aug, 
                         yend = latitude_aug+v_aug/60), arrow = arrow(length = unit(0.1, "cm")))+
        geom_sf(data = NULL, fill = "grey85", col = 1)+ # I changed from data=tz.ke to data=NULL because of ERROR
        coord_sf(xlim = c(95, 125), ylim =  c(-10, 25))+
        scale_x_continuous(breaks = c(95.5, 125))+
        theme_bw()+
        theme(axis.text = element_text(size = 11, colour = 1))+
        labs(x = NULL, y = NULL)



cowplot::plot_grid(feb, aug,ncol = 2)






#feb
wind.vector_feb = ggplot(data = world) +
        geom_raster(data = wind_data_feb, aes(x = longitude_feb, y = latitude_feb, fill = wind_data_feb$speed))+
        geom_segment(data = wind_data_feb, 
                     aes(x = longitude_feb, xend = longitude_feb+u_feb/60, y = latitude_feb, 
                         yend = latitude_feb+v_feb/60), arrow = arrow(length = unit(0.1, "cm")))+
        geom_sf(data = NULL, fill = "grey85", col = 1)+
        coord_sf(xlim = c(95, 125), ylim =  c(-10, 25))+
        scale_fill_gradientn(colours = oce::oceColorsPalette(120), limits = c(0,12), 
                             na.value = "white", name = "Speed\n (m/s)")+
        scale_x_continuous(breaks = c(95.5, 125))+
        theme_bw()+
        theme(axis.text = element_text(size = 14, colour = 1),
              legend.text = element_text(size = 14, colour = 1), 
              legend.title = element_text(size = 14, colour = 1),
              legend.position = c(.12,.17),
              legend.background = element_rect(colour = 1, fill = "white"))+
        labs(x = NULL, y = NULL, title = "SCS Wind Map - February")


#aug
wind.vector_aug = ggplot(data = world, ) +
        geom_raster(data = wind_data_aug, aes(x = longitude_aug, y = latitude_aug, fill = wind_data_aug$speed))+
        geom_segment(data = wind_data_aug, 
                     aes(x = longitude_aug, xend = longitude_aug+u_aug/60, y = latitude_aug, 
                         yend = latitude_aug+v_aug/60), arrow = arrow(length = unit(0.1, "cm")))+
        geom_sf(data = NULL, fill = "grey85", col = 1)+
        coord_sf(xlim = c(95, 125), ylim =  c(-10, 25))+
        scale_fill_gradientn(colours = oce::oceColorsPalette(120), limits = c(0,12), 
                             na.value = "white", name = "Speed\n (m/s)")+
        scale_x_continuous(breaks = c(95.5, 125))+
        theme_bw()+
        theme(axis.text = element_text(size = 14, colour = 1),
              legend.text = element_text(size = 14, colour = 1), 
              legend.title = element_text(size = 14, colour = 1),
              legend.position = c(.12,.17),
              legend.background = element_rect(colour = 1, fill = "white"))+
        labs(x = NULL, y = NULL, title = "SCS Wind Map - August")



cowplot::plot_grid(wind.vector_feb, wind.vector_aug, ncol = 2)



# gganimate...-----------------------------------------------------------------------------------------------------------------

animate(wind.vector)







