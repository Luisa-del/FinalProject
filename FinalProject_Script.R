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

# Settings---------------------------------------------------------------------------------------------------------------------

#-> set working directoriy
#home_dir <- "/home/luisa/Documents/EAGLE_Master/MB2_Programming_Geostatistics/FinalProject/wind/"
#setwd(home_dir)
getwd()

#-> Define output directory of data downloads and export folder (maybe data download not necessary)
#data_dir <- "Data/"
#export_dir <- "Export/"
#dir.create(data_dir,showWarnings = TRUE)
#dir.create(export_dir,showWarnings = TRUE)



#------------------------------------------------------------------------------------------------------------------------------

# Download and generate R-Object of wind for Study Area (SCS) at 2019, Feb/May/Aug/Nov 12, 00:00 ------------------------------

#feb
wind_data_feb<-wind.dl(2019,02,12,0,95,125,-10,25)   #wind_data_feb2 <- wind.dl_2("2019/2/12 00:00:00",95,125,-10,25)
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

#nov
longitude_nov = wind_data_nov$lon
latitude_nov = wind_data_nov$lat
u_nov = wind_data_nov$ugrd10m
v_nov = wind_data_nov$vgrd10m



# create polygon of countries to add to plot ----------------------------------------------------------------------------------

world <- ne_countries(scale = "medium", returnclass = "sf", )
#studyarea <- clip()

class(world)

#scs_lonlat <- data.frame(longitude = c(95, 125), latitude = c(-10, 25)) #no data, combine somehow woth world
#class(scs_lonlat)


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

wind.vector_aug
wind.vector_nov
wind.vector_feb
wind.vector_may

cowplot::plot_grid(wind.vector_feb, wind.vector_may, wind.vector_aug, wind.vector_nov, ncol = 2, nrow = 2)
cowplot::plot_grid(wind.vector_feb, wind.vector_may, ncol = 2)
cowplot::plot_grid(wind.vector_aug, wind.vector_nov, ncol = 2)




##########################################################################################################################
#-Time Series-############################################################################################################
##########################################################################################################################

library(rWind)
library(lubridate)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)

#Download and store data--------------------------------------------------------------------------------------------------

#feb
dt_feb <- seq(ymd_hms(paste(2019,2,1,00,00,00, sep="-")),
          ymd_hms(paste(2019,2,28,18,00,00, sep="-")),by="6 hours") #eig 28 statt 7
wind_serie_feb <- wind.dl_2(dt_feb,95,125,-10,25)
tidy(wind_serie_feb) #ist das notwendig?
class(wind_serie_feb)

#may
dt_may <- seq(ymd_hms(paste(2019,5,1,00,00,00, sep="-")),
              ymd_hms(paste(2019,5,31,18,00,00, sep="-")),by="6 hours") #eig 31 statt 7
wind_serie_may <- wind.dl_2(dt_may,95,125,-10,25)
tidy(wind_serie_may)
class(wind_serie_may)

#aug
dt_aug <- seq(ymd_hms(paste(2019,8,1,00,00,00, sep="-")),
              ymd_hms(paste(2019,8,31,18,00,00, sep="-")),by="12 hours") #eig 31 statt 7
wind_serie_aug <- wind.dl_2(dt_aug,95,125,-10,25)
tidy(wind_serie_aug)
class(wind_serie_aug)

#sep
dt_sep <- seq(ymd_hms(paste(2019,9,1,00,00,00, sep="-")),
              ymd_hms(paste(2019,9,30,18,00,00, sep="-")),by="12 hours") #eig 31 statt 7
wind_serie_sep <- wind.dl_2(dt_sep,95,125,-10,25)

#okt
dt_oct <- seq(ymd_hms(paste(2019,10,1,00,00,00, sep="-")),
              ymd_hms(paste(2019,10,31,18,00,00, sep="-")),by="12 hours") #eig 30 statt 7
wind_serie_oct <- wind.dl_2(dt_oct,95,125,-10,25)

#nov
dt_nov <- seq(ymd_hms(paste(2019,11,1,00,00,00, sep="-")),
              ymd_hms(paste(2019,11,30,18,00,00, sep="-")),by="12 hours") #eig 30 statt 7
wind_serie_nov <- wind.dl_2(dt_nov,95,125,-10,25)
tidy(wind_serie_nov)
class(wind_serie_nov)


wind_serie_feb
wind_serie_may
wind_serie_aug
wind_serie_sep
wind_serie_oct
wind_serie_nov



#world polygon of Natural Earth Data
world <- ne_countries(scale = "medium", returnclass = "sf", )



# Use wind.mean function to calculate wind average----------------------------------------------------------------------------

#feb
w_mean_feb <- wind.mean(wind_serie_feb)
w_mean_feb
class(w_mean_feb)

#may
w_mean_may <- wind.mean(wind_serie_may)
w_mean_may
class(w_mean_may)

#aug
w_mean_aug <- wind.mean(wind_serie_aug)
w_mean_aug
class(w_mean_aug)

#sep
w_mean_sep <- wind.mean(wind_serie_sep)
w_mean_sep
class(w_mean_sep)

#oct
w_mean_oct <- wind.mean(wind_serie_oct)
w_mean_oct
class(w_mean_oct)

#nov
w_mean_nov <- wind.mean(wind_serie_nov)
w_mean_nov
class(w_mean_nov)


#extract necessary values---------------------------------------------------------------------------------------------

#feb
longitude_feb <- w_mean_feb$lon
latitude_feb <- w_mean_feb$lat
u_feb <- w_mean_feb$ugrd10m
v_feb <- w_mean_feb$vgrd10m

#may
longitude_may <- w_mean_may$lon
latitude_may <- w_mean_may$lat
u_may <- w_mean_may$ugrd10m
v_may <- w_mean_may$vgrd10m

#aug
longitude_aug <- w_mean_aug$lon
latitude_aug <- w_mean_aug$lat
u_aug <- w_mean_aug$ugrd10m
v_aug <- w_mean_aug$vgrd10m

#sep
longitude_sep <- w_mean_sep$lon
latitude_sep <- w_mean_sep$lat
u_sep <- w_mean_sep$ugrd10m
v_sep <- w_mean_sep$vgrd10m

#oct
longitude_oct = w_mean_oct$lon
latitude_oct = w_mean_oct$lat
u_oct = w_mean_oct$ugrd10m
v_oct = w_mean_oct$vgrd10m

#nov
longitude_nov = w_mean_nov$lon
latitude_nov = w_mean_nov$lat
u_nov = w_mean_nov$ugrd10m
v_nov = w_mean_nov$vgrd10m



#ggplot wind map for four seasons--------------------------------------------------------------------------------------------

#feb
wind.vector_feb = ggplot(data = world) +
        geom_raster(data = w_mean_feb, aes(x = longitude_feb, y = latitude_feb, fill = w_mean_feb$speed))+
        geom_segment(data = w_mean_feb, 
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
wind.vector_feb
#may
wind.vector_may = ggplot(data = world) +
        geom_raster(data = w_mean_may, aes(x = longitude_may, y = latitude_may, fill = w_mean_may$speed))+
        geom_segment(data = w_mean_may, 
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
        geom_raster(data = w_mean_aug, aes(x = longitude_aug, y = latitude_aug, fill = w_mean_aug$speed))+
        geom_segment(data = w_mean_aug, 
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

#sep
wind.vector_sep = ggplot(data = world) +
        geom_raster(data = w_mean_sep, aes(x = longitude_sep, y = latitude_sep, fill = w_mean_sep$speed))+
        geom_segment(data = w_mean_sep, 
                     aes(x = longitude_sep, xend = longitude_sep+u_sep/60, y = latitude_sep, 
                         yend = latitude_sep+v_sep/60), arrow = arrow(length = unit(0.1, "cm")))+
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
        labs(x = NULL, y = NULL, title = "SCS Wind Map - September 2019")

#oct
wind.vector_oct = ggplot(data = world) +
        geom_raster(data = w_mean_oct, aes(x = longitude_oct, y = latitude_oct, fill = w_mean_oct$speed))+
        geom_segment(data = w_mean_oct, 
                     aes(x = longitude_oct, xend = longitude_oct+u_oct/60, y = latitude_oct, 
                         yend = latitude_oct+v_oct/60), arrow = arrow(length = unit(0.1, "cm")))+
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
        labs(x = NULL, y = NULL, title = "SCS Wind Map - October 2019")

#nov
wind.vector_nov = ggplot(data = world) +
        geom_raster(data = w_mean_nov, aes(x = longitude_nov, y = latitude_nov, fill = w_mean_nov$speed))+
        geom_segment(data = w_mean_nov, 
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


#plot single maps
wind.vector_feb
wind.vector_may
wind.vector_aug
wind.vector_sep
wind.vector_oct
wind.vector_nov


cowplot::plot_grid(wind.vector_feb, wind.vector_may, wind.vector_aug, wind.vector_nov, ncol = 2, nrow = 2)
cowplot::plot_grid(wind.vector_feb, wind.vector_may, wind.vector_aug, wind.vector_sep, wind.vector_oct, wind.vector_nov, ncol = 3, nrow = 2)
cowplot::plot_grid(wind.vector_feb, wind.vector_may, ncol = 2)
cowplot::plot_grid(wind.vector_aug, wind.vector_nov, ncol = 2)
cowplot::plot_grid(wind.vector_feb, wind.vector_aug, ncol = 2)
cowplot::plot_grid(wind.vector_may, wind.vector_nov, ncol = 2)







# And then use wind2raster directly to create a raster layer  

r_mean_feb <- wind2raster(w_mean_feb)  

# We can plot a subset of this raster around Azores Islands.  
# Using "arrowDir" we can include arrows for wind direction with   
# "Arrowhead" function from "shape" R package  

plot(r_mean_feb$speed, main= "Wind speed and direction average",
     xlim=c(95,125), ylim=c(-10,25))  
alpha <- arrowDir(w_mean_feb)  
library(shape)  
Arrowhead(w_mean_feb$lon, w_mean_feb$lat, angle=alpha,   
          arr.length = 0.02, arr.type="curved")  
lines(getMap(resolution = "low"), lwd=4)  



#with loop function------------------------------------------------------------------------------------------------------------------------------

# create an empty list where you will store all the data
wind_serie_feb2 <- list()

# use wind.dl(2) inside a for-in loop to download and store wind data of the first 5 days of February 2015 at 00:00
for (d in 1:5){
        w <- wind.dl_2("2019/2/12 00:00:00",95,125,-10,25)
        wind_serie_feb2[[d]] <- w
}

class(wind_serie_feb2)
wind_serie_feb2

#oder
wind_serie_feb <- list()
for (d in 1:5){
        w<-wind.dl(2019,2,d,0,95,125,-10,25)
        wind_serie_feb[[d]]<-w
}

class(wind_serie_feb)
wind_serie_feb


wind.series <- wind.series
class(wind.series)


#new try##################################################################################


# create an empty list where you will store all the data
wind_serie <- list()

# Then, you can use a wind.dl inside a for-in loop to download and store wind data of 
# the first 5 days of February 2015 at 00:00 in Europe region. It could take a while...

for (d in 1:5){
        w<-wind.dl(2019,2,d,0,95,125,-10,25)
        wind_serie[[d]]<-w
}

wind_serie


# Finally, you can use wind.mean function to calculate wind average 

wind_average <- wind.mean(wind.series)
wind_average <- wind.fit(wind_average)
r_average_dir <- wind2raster(wind_average, type="dir")
r_average_speed <- wind2raster(wind_average, type="speed")

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
class(ww)


library(lubridate)
dt <- seq(ymd_h(paste(2015,1,3,00, sep="-")),
          ymd_h(paste(2015,1,3,21, sep="-")),by="3 hours")
wind.series <- wind.dl_2(dt, 164, 179, -48, -33)

## End(Not run)










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







