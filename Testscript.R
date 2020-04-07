##########################################################################################################################
# Wind Map of the South China Sea ########################################################################################
##########################################################################################################################
# Aim: Creating a Wind Map for the South China Sea 
# Steps: Calculate wind average of all months of 2019
#        Donwload and edit other necessary parameters for the map
#        Ggplot all data
#        Save maps as jpg/pdf,...
##########################################################################################################################


# Arrangements-------------------------------------------------------------------------------------------------

#load necessary packages
library(lubridate)
library(rWind)
library(rnaturalearth)
library(raster)
library(rgeos)
library(sf)
library(ggplot2)
library(ggspatial)
library(ggrepel)


#set working directory
home_dir <- "/home/luisa/Documents/EAGLE_Master/MB2_Programming_Geostatistics/FinalProject"
setwd(home_dir)
getwd()



# Download and store winddata for all months for the South China Sea region------------------------------------

#-> downloading the data for a whole month takes a while
#-> so for testing only data of first two days will be downloaded

#jan
dt_jan <- seq(ymd_hms(paste(2019,1,1,00,00,00, sep="-")),
              ymd_hms(paste(2019,1,2,18,00,00, sep="-")),by="6 hours") #actually 31 days
wind_serie_jan <- wind.dl_2(dt_jan,95,125,-10,25)

#feb
dt_feb <- seq(ymd_hms(paste(2019,2,1,00,00,00, sep="-")),
              ymd_hms(paste(2019,2,2,18,00,00, sep="-")),by="6 hours") #actually 28 days
wind_serie_feb <- wind.dl_2(dt_feb,95,125,-10,25)

#mar
dt_mar <- seq(ymd_hms(paste(2019,3,1,00,00,00, sep="-")),
              ymd_hms(paste(2019,3,2,18,00,00, sep="-")),by="6 hours") #actually 31 days
wind_serie_mar <- wind.dl_2(dt_mar,95,125,-10,25)

#apr
dt_apr <- seq(ymd_hms(paste(2019,4,1,00,00,00, sep="-")),
              ymd_hms(paste(2019,4,2,18,00,00, sep="-")),by="6 hours") #actually 30 days
wind_serie_apr <- wind.dl_2(dt_apr,95,125,-10,25)

#may
dt_may <- seq(ymd_hms(paste(2019,5,1,00,00,00, sep="-")),
              ymd_hms(paste(2019,5,2,18,00,00, sep="-")),by="6 hours") #actually 31 days
wind_serie_may <- wind.dl_2(dt_may,95,125,-10,25)

#jun
dt_jun <- seq(ymd_hms(paste(2019,6,1,00,00,00, sep="-")),
              ymd_hms(paste(2019,6,2,18,00,00, sep="-")),by="6 hours") #actually 30 days
wind_serie_jun <- wind.dl_2(dt_jun,95,125,-10,25)

#jul
dt_jul <- seq(ymd_hms(paste(2019,7,1,00,00,00, sep="-")),
              ymd_hms(paste(2019,7,2,18,00,00, sep="-")),by="6 hours") #actually 31 days
wind_serie_jul <- wind.dl_2(dt_jul,95,125,-10,25)

#aug
dt_aug <- seq(ymd_hms(paste(2019,8,1,00,00,00, sep="-")),
              ymd_hms(paste(2019,8,2,18,00,00, sep="-")),by="6 hours") #actually 31 days
wind_serie_aug <- wind.dl_2(dt_aug,95,125,-10,25)

#sep
dt_sep <- seq(ymd_hms(paste(2019,9,1,00,00,00, sep="-")),
              ymd_hms(paste(2019,9,2,18,00,00, sep="-")),by="6 hours") #actually 31 days
wind_serie_sep <- wind.dl_2(dt_sep,95,125,-10,25)

#oct
dt_oct <- seq(ymd_hms(paste(2019,10,1,00,00,00, sep="-")),
              ymd_hms(paste(2019,10,2,18,00,00, sep="-")),by="6 hours") #actually 30 days
wind_serie_oct <- wind.dl_2(dt_oct,95,125,-10,25)

#nov
dt_nov <- seq(ymd_hms(paste(2019,11,1,00,00,00, sep="-")),
              ymd_hms(paste(2019,11,2,18,00,00, sep="-")),by="6 hours") #actually 30 days
wind_serie_nov <- wind.dl_2(dt_nov,95,125,-10,25)

#dec
dt_dec <- seq(ymd_hms(paste(2019,12,1,00,00,00, sep="-")),
              ymd_hms(paste(2019,12,2,18,00,00, sep="-")),by="6 hours") #actually 31 days
wind_serie_dec <- wind.dl_2(dt_dec,95,125,-10,25)


# Use wind.mean function to calculate wind average------------------------------------------------------------

#jan
w_mean_jan <- wind.mean(wind_serie_jan)

#feb
w_mean_feb <- wind.mean(wind_serie_feb)

#mar
w_mean_mar <- wind.mean(wind_serie_mar)

#apr
w_mean_apr <- wind.mean(wind_serie_apr)

#may
w_mean_may <- wind.mean(wind_serie_may)

#jun
w_mean_jun <- wind.mean(wind_serie_jun)

#jul
w_mean_jul <- wind.mean(wind_serie_jul)

#aug
w_mean_aug <- wind.mean(wind_serie_aug)

#sep
w_mean_sep <- wind.mean(wind_serie_sep)

#oct
w_mean_oct <- wind.mean(wind_serie_oct)

#nov
w_mean_nov <- wind.mean(wind_serie_nov)

#dec
w_mean_dec <- wind.mean(wind_serie_dec)


#extract necessary values for ggplot-------------------------------------------------------------------------

#jan
longitude_jan <- w_mean_jan$lon
latitude_jan <- w_mean_jan$lat
u_jan <- w_mean_jan$ugrd10m
v_jan <- w_mean_jan$vgrd10m

#feb
longitude_feb <- w_mean_feb$lon
latitude_feb <- w_mean_feb$lat
u_feb <- w_mean_feb$ugrd10m
v_feb <- w_mean_feb$vgrd10m

#mar
longitude_mar <- w_mean_mar$lon
latitude_mar <- w_mean_mar$lat
u_mar <- w_mean_mar$ugrd10m
v_mar <- w_mean_mar$vgrd10m

#apr
longitude_apr <- w_mean_apr$lon
latitude_apr <- w_mean_apr$lat
u_apr <- w_mean_apr$ugrd10m
v_apr <- w_mean_apr$vgrd10m

#may
longitude_may <- w_mean_may$lon
latitude_may <- w_mean_may$lat
u_may <- w_mean_may$ugrd10m
v_may <- w_mean_may$vgrd10m

#jun
longitude_jun <- w_mean_jun$lon
latitude_jun <- w_mean_jun$lat
u_jun <- w_mean_jun$ugrd10m
v_jun <- w_mean_jun$vgrd10m

#jul
longitude_jul <- w_mean_jul$lon
latitude_jul <- w_mean_jul$lat
u_jul <- w_mean_jul$ugrd10m
v_jul <- w_mean_jul$vgrd10m

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

#dec
longitude_dec <- w_mean_dec$lon
latitude_dec <- w_mean_dec$lat
u_dec <- w_mean_dec$ugrd10m
v_dec <- w_mean_dec$vgrd10m



#prepare country polygons for the map------------------------------------------------------------------------

#download world data of natural earth dataset (country polygons and border lines)
#-> class need to be of sp for intersection, convert later again to sf to plot in ggplot
world <- ne_countries(scale = "medium", type = "countries", returnclass = "sp") #sp for boundarybox
lines <- ne_download(scale = 50, category = "cultural", type = "boundary_lines_land", destdir = tempdir(), load = TRUE, returnclass = "sp")


#set lat-/lon limits for study area
lonlim <- c(95, 125)
latlim <- c(-10, 25)


#create bounding box for studyarea (sa), sp-object for intersection
sa_bb_sp <- as(extent(lonlim, latlim), "SpatialPolygons")


#set same proj4strings for objects
proj4string(sa_bb_sp) <- CRS("+proj=longlat +datum=WGS84 +no_defs")
proj4string(world) <- CRS("+proj=longlat +datum=WGS84 +no_defs")
proj4string(lines) <- CRS("+proj=longlat +datum=WGS84 +no_defs")


#intersect now world polygon with boundarybox
sarea <- gIntersection(world, sa_bb_sp)
slines <- gIntersection(lines, sa_bb_sp)

#and convert sp to sf for ggplot
studarea <- st_as_sf(sarea)
studlines <- st_as_sf(slines)




#prepare country labels for the map--------------------------------------------------------------------------

#download world data of Natural Earth Dataset as sf-object for country-labels
countries <- ne_countries(scale = "medium", type = "countries", returnclass = "sf")

#create centroid country labels
world_points <- cbind(countries, st_coordinates(st_centroid(countries$geometry)))

#convert sp object sa_bb_sf to bbox-object for cropping
sa_bb_sf <- st_bbox(sa_bb_sp)

#cropping of sf/dataframe world_points only possible with bbox-object
sa_points <- st_crop(world_points, sa_bb_sf)





#ggplot wind map for every months for the South China Sea ----------------------------------------------------


#create theme-design function

theme_design <- function(){            # creating a new theme function
  theme_bw()+                          # using a predefined theme as a base
    theme(plot.title = element_text(size = 10, colour = 1) ,
          plot.subtitle = element_text(size = 8, colour = 1),
          axis.text = element_text(size = 8, colour = 1),
          axis.title = element_text(size = 7, colour = 1),
          legend.text = element_text(size = 7, colour = 1),
          legend.title = element_text(size = 7, colour = 1), 
          legend.position = c(1.1,.44),
          legend.key.height = unit(1.3, "cm"))
}


#jan
wind.vector_jan = ggplot(data = NULL) +
  #subsequent: plot average wind direction and speed of each month + set parameters for axes and title
  geom_raster(data = w_mean_jan, aes(x = longitude_jan, y = latitude_jan, fill = w_mean_jan$speed))+
  geom_segment(data = w_mean_jan, 
               aes(x = longitude_jan, xend = longitude_jan+u_jan/60, y = latitude_jan, 
                   yend = latitude_jan+v_jan/60), arrow = arrow(length = unit(0.08, "cm")))+
  labs(x = "Longitude", y = "Latitude", title = "Wind Map - South China Sea", subtitle = "January 2019")+ 
  #subsequent: plot countrypolygon + borderlines
  geom_sf(data = studarea, fill = "#C7A77E", col = 1)+
  geom_sf(data = studlines, col = 1)+
  #subsequent: mapdesign
  geom_label_repel(data = sa_points, aes(x=X, y=Y, label=name), size = 1.5,
                   label.padding = unit(0.1, "lines"), label.size = 0.01, color = "black", fill = "grey80") +
  annotation_scale(location = "br", bar_cols = c("black", "white"), 
                   line_width = 0.5, height = unit(0.08, "cm"), width_hint = 0.3,
                   pad_x = unit(0.3, "cm"), pad_y = unit(0.1, "cm"), text_cex = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         height = unit(0.9, "cm"), width = unit(0.9, "cm"),
                         pad_x = unit(0.25, "cm"), pad_y = unit(0.4, "cm"),
                         style = north_arrow_nautical(
                           line_width = 1, line_col = "grey80",fill = c("grey80", "grey30"),
                           text_col = "grey80"))+        
  coord_sf(xlim = lonlim, ylim =  latlim) + 
  scale_fill_gradientn(colours = oce::oceColorsPalette(120), limits = c(0,23),  #ckeck highest windspeed for limit 
                       na.value = "white", name = "Speed\n (m/s)")+ 
  scale_x_continuous(breaks = lonlim)+ 
  theme_design()

wind.vector_jan

#feb
wind.vector_feb = ggplot(data = NULL) +
  geom_raster(data = w_mean_feb, 
              aes(x = longitude_feb, y = latitude_feb, fill = w_mean_feb$speed))+
  geom_segment(data = w_mean_feb, 
               aes(x = longitude_feb, xend = longitude_feb+u_feb/60, y = latitude_feb, 
                   yend = latitude_feb+v_feb/60), arrow = arrow(length = unit(0.08, "cm")))+
  labs(x = "Longitude", y = "Latitude", title = "Wind Map - South China Sea", subtitle = "February 2019")+
  geom_sf(data = studarea, fill = "#C7A77E", col = 1)+ 
  geom_sf(data = studlines, col = 1)+ 
  geom_label_repel(data = sa_points, aes(x=X, y=Y, label=name), size = 1.5,
                   label.padding = unit(0.1, "lines"), label.size = 0.01, color = "black", fill = "grey80") +
  annotation_scale(location = "br", bar_cols = c("black", "white"), 
                   line_width = 0.5, height = unit(0.08, "cm"), width_hint = 0.3,
                   pad_x = unit(0.3, "cm"), pad_y = unit(0.1, "cm"), text_cex = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         height = unit(0.9, "cm"), width = unit(0.9, "cm"),
                         pad_x = unit(0.25, "cm"), pad_y = unit(0.4, "cm"),
                         style = north_arrow_nautical(
                           line_width = 1, line_col = "grey80",fill = c("grey80", "grey30"),
                           text_col = "grey80"))+        
  coord_sf(xlim = lonlim, ylim =  latlim) + 
  scale_fill_gradientn(colours = oce::oceColorsPalette(120), limits = c(0,23),
                       na.value = "white", name = "Speed\n (m/s)")+
  scale_x_continuous(breaks = lonlim)+
  theme_design()

wind.vector_feb



#mar
wind.vector_mar = ggplot(data = NULL) +
  geom_raster(data = w_mean_mar, aes(x = longitude_mar, y = latitude_mar, fill = w_mean_mar$speed))+
  geom_segment(data = w_mean_mar, 
               aes(x = longitude_mar, xend = longitude_mar+u_mar/60, y = latitude_mar, 
                   yend = latitude_mar+v_mar/60), arrow = arrow(length = unit(0.08, "cm")))+
  labs(x = "Longitude", y = "Latitude", title = "Wind Map - South China Sea", subtitle = "March 2019")+
  geom_sf(data = studarea, fill = "#C7A77E", col = 1)+ 
  geom_sf(data = studlines, col = 1)+ 
  geom_label_repel(data = sa_points, aes(x=X, y=Y, label=name), size = 1.5,
                   label.padding = unit(0.1, "lines"), label.size = 0.01, color = "black", fill = "grey80") +
  annotation_scale(location = "br", bar_cols = c("black", "white"), 
                   line_width = 0.5, height = unit(0.08, "cm"), width_hint = 0.3,
                   pad_x = unit(0.3, "cm"), pad_y = unit(0.1, "cm"), text_cex = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         height = unit(0.9, "cm"), width = unit(0.9, "cm"),
                         pad_x = unit(0.25, "cm"), pad_y = unit(0.4, "cm"),
                         style = north_arrow_nautical(
                           line_width = 1, line_col = "grey80",fill = c("grey80", "grey30"),
                           text_col = "grey80"))+        
  coord_sf(xlim = lonlim, ylim =  latlim) + 
  scale_fill_gradientn(colours = oce::oceColorsPalette(120), limits = c(0,23), 
                       na.value = "white", name = "Speed\n (m/s)")+ 
  scale_x_continuous(breaks = lonlim)+ 
  theme_design()

wind.vector_mar

#apr
wind.vector_apr = ggplot(data = NULL) +
  geom_raster(data = w_mean_apr, aes(x = longitude_apr, y = latitude_apr, fill = w_mean_apr$speed))+
  geom_segment(data = w_mean_apr, 
               aes(x = longitude_apr, xend = longitude_apr+u_apr/60, y = latitude_apr, 
                   yend = latitude_apr+v_apr/60), arrow = arrow(length = unit(0.08, "cm")))+
  labs(x = "Longitude", y = "Latitude", title = "Wind Map - South China Sea", subtitle = "April 2019")+
  geom_sf(data = studarea, fill = "#C7A77E", col = 1)+ 
  geom_sf(data = studlines, col = 1)+ 
  geom_label_repel(data = sa_points, aes(x=X, y=Y, label=name), size = 1.5,
                   label.padding = unit(0.1, "lines"), label.size = 0.01, color = "black", fill = "grey80") +
  annotation_scale(location = "br", bar_cols = c("black", "white"), 
                   line_width = 0.5, height = unit(0.08, "cm"), width_hint = 0.3,
                   pad_x = unit(0.3, "cm"), pad_y = unit(0.1, "cm"), text_cex = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         height = unit(0.9, "cm"), width = unit(0.9, "cm"),
                         pad_x = unit(0.25, "cm"), pad_y = unit(0.4, "cm"),
                         style = north_arrow_nautical(
                           line_width = 1, line_col = "grey80",fill = c("grey80", "grey30"),
                           text_col = "grey80"))+        
  coord_sf(xlim = lonlim, ylim =  latlim) + 
  scale_fill_gradientn(colours = oce::oceColorsPalette(120), limits = c(0,23), 
                       na.value = "white", name = "Speed\n (m/s)")+ 
  scale_x_continuous(breaks = lonlim)+ 
  theme_design()

wind.vector_apr        

#may
wind.vector_may = ggplot(data = NULL) +
  geom_raster(data = w_mean_may, aes(x = longitude_may, y = latitude_may, fill = w_mean_may$speed))+
  geom_segment(data = w_mean_may, 
               aes(x = longitude_may, xend = longitude_may+u_may/60, y = latitude_may, 
                   yend = latitude_may+v_may/60), arrow = arrow(length = unit(0.08, "cm")))+
  labs(x = "Longitude", y = "Latitude", title = "Wind Map - South China Sea", subtitle = "May 2019")+
  geom_sf(data = studarea, fill = "#C7A77E", col = 1)+ 
  geom_sf(data = studlines, col = 1)+ 
  geom_label_repel(data = sa_points, aes(x=X, y=Y, label=name), size = 1.5,
                   label.padding = unit(0.1, "lines"), label.size = 0.01, color = "black", fill = "grey80") +
  annotation_scale(location = "br", bar_cols = c("black", "white"), 
                   line_width = 0.5, height = unit(0.08, "cm"), width_hint = 0.3,
                   pad_x = unit(0.3, "cm"), pad_y = unit(0.1, "cm"), text_cex = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         height = unit(0.9, "cm"), width = unit(0.9, "cm"),
                         pad_x = unit(0.25, "cm"), pad_y = unit(0.4, "cm"),
                         style = north_arrow_nautical(
                           line_width = 1, line_col = "grey80",fill = c("grey80", "grey30"),
                           text_col = "grey80"))+        
  coord_sf(xlim = lonlim, ylim =  latlim) + 
  scale_fill_gradientn(colours = oce::oceColorsPalette(120), limits = c(0,23), 
                       na.value = "white", name = "Speed\n (m/s)")+ 
  scale_x_continuous(breaks = lonlim)+ 
  theme_design()

wind.vector_may        

#jun
wind.vector_jun = ggplot(data = NULL) +
  geom_raster(data = w_mean_jun, aes(x = longitude_jun, y = latitude_jun, fill = w_mean_jun$speed))+
  geom_segment(data = w_mean_jun, 
               aes(x = longitude_jun, xend = longitude_jun+u_jun/60, y = latitude_jun, 
                   yend = latitude_jun+v_jun/60), arrow = arrow(length = unit(0.08, "cm")))+
  labs(x = "Longitude", y = "Latitude", title = "Wind Map - South China Sea", subtitle = "June 2019")+
  geom_sf(data = studarea, fill = "#C7A77E", col = 1)+ 
  geom_sf(data = studlines, col = 1)+ 
  geom_label_repel(data = sa_points, aes(x=X, y=Y, label=name), size = 1.5,
                   label.padding = unit(0.1, "lines"), label.size = 0.01, color = "black", fill = "grey80") +
  annotation_scale(location = "br", bar_cols = c("black", "white"), 
                   line_width = 0.5, height = unit(0.08, "cm"), width_hint = 0.3,
                   pad_x = unit(0.3, "cm"), pad_y = unit(0.1, "cm"), text_cex = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         height = unit(0.9, "cm"), width = unit(0.9, "cm"),
                         pad_x = unit(0.25, "cm"), pad_y = unit(0.4, "cm"),
                         style = north_arrow_nautical(
                           line_width = 1, line_col = "grey80",fill = c("grey80", "grey30"),
                           text_col = "grey80"))+        
  coord_sf(xlim = lonlim, ylim =  latlim) + 
  scale_fill_gradientn(colours = oce::oceColorsPalette(120), limits = c(0,23), 
                       na.value = "white", name = "Speed\n (m/s)")+ 
  scale_x_continuous(breaks = lonlim)+ 
  theme_design()

wind.vector_jun

#jul
wind.vector_jul = ggplot(data = NULL) +
  geom_raster(data = w_mean_jul, aes(x = longitude_jul, y = latitude_jul, fill = w_mean_jul$speed))+
  geom_segment(data = w_mean_jul, 
               aes(x = longitude_jul, xend = longitude_jul+u_jul/60, y = latitude_jul, 
                   yend = latitude_jul+v_jul/60), arrow = arrow(length = unit(0.08, "cm")))+
  labs(x = "Longitude", y = "Latitude", title = "Wind Map - South China Sea", subtitle = "July 2019")+
  geom_sf(data = studarea, fill = "#C7A77E", col = 1)+ 
  geom_sf(data = studlines, col = 1)+ 
  geom_label_repel(data = sa_points, aes(x=X, y=Y, label=name), size = 1.5,
                   label.padding = unit(0.1, "lines"), label.size = 0.01, color = "black", fill = "grey80") +
  annotation_scale(location = "br", bar_cols = c("black", "white"), 
                   line_width = 0.5, height = unit(0.08, "cm"), width_hint = 0.3,
                   pad_x = unit(0.3, "cm"), pad_y = unit(0.1, "cm"), text_cex = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         height = unit(0.9, "cm"), width = unit(0.9, "cm"),
                         pad_x = unit(0.25, "cm"), pad_y = unit(0.4, "cm"),
                         style = north_arrow_nautical(
                           line_width = 1, line_col = "grey80",fill = c("grey80", "grey30"),
                           text_col = "grey80"))+        
  coord_sf(xlim = lonlim, ylim =  latlim) + 
  scale_fill_gradientn(colours = oce::oceColorsPalette(120), limits = c(0,23), 
                       na.value = "white", name = "Speed\n (m/s)")+ 
  scale_x_continuous(breaks = lonlim)+ 
  theme_design()

wind.vector_jul        

#aug
wind.vector_aug = ggplot(data = NULL) +
  geom_raster(data = w_mean_aug, aes(x = longitude_aug, y = latitude_aug, fill = w_mean_aug$speed))+
  geom_segment(data = w_mean_aug, 
               aes(x = longitude_aug, xend = longitude_aug+u_aug/60, y = latitude_aug, 
                   yend = latitude_aug+v_aug/60), arrow = arrow(length = unit(0.08, "cm")))+
  labs(x = "Longitude", y = "Latitude", title = "Wind Map - South China Sea", subtitle = "August 2019")+
  geom_sf(data = studarea, fill = "#C7A77E", col = 1)+ 
  geom_sf(data = studlines, col = 1)+ 
  geom_label_repel(data = sa_points, aes(x=X, y=Y, label=name), size = 1.5,
                   label.padding = unit(0.1, "lines"), label.size = 0.01, color = "black", fill = "grey80") +
  annotation_scale(location = "br", bar_cols = c("black", "white"), 
                   line_width = 0.5, height = unit(0.08, "cm"), width_hint = 0.3,
                   pad_x = unit(0.3, "cm"), pad_y = unit(0.1, "cm"), text_cex = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         height = unit(0.9, "cm"), width = unit(0.9, "cm"),
                         pad_x = unit(0.25, "cm"), pad_y = unit(0.4, "cm"),
                         style = north_arrow_nautical(
                           line_width = 1, line_col = "grey80",fill = c("grey80", "grey30"),
                           text_col = "grey80"))+        
  coord_sf(xlim = lonlim, ylim =  latlim) + 
  scale_fill_gradientn(colours = oce::oceColorsPalette(120), limits = c(0,23), 
                       na.value = "white", name = "Speed\n (m/s)")+ 
  scale_x_continuous(breaks = lonlim)+ 
  theme_design()

wind.vector_aug     

#sep
wind.vector_sep = ggplot(data = NULL) +
  geom_raster(data = w_mean_sep, aes(x = longitude_sep, y = latitude_sep, fill = w_mean_sep$speed))+
  geom_segment(data = w_mean_sep, 
               aes(x = longitude_sep, xend = longitude_sep+u_sep/60, y = latitude_sep, 
                   yend = latitude_sep+v_sep/60), arrow = arrow(length = unit(0.08, "cm")))+
  labs(x = "Longitude", y = "Latitude", title = "Wind Map - South China Sea", subtitle = "September 2019")+
  geom_sf(data = studarea, fill = "#C7A77E", col = 1)+ 
  geom_sf(data = studlines, col = 1)+ 
  geom_label_repel(data = sa_points, aes(x=X, y=Y, label=name), size = 1.5,
                   label.padding = unit(0.1, "lines"), label.size = 0.01, color = "black", fill = "grey80") +
  annotation_scale(location = "br", bar_cols = c("black", "white"), 
                   line_width = 0.5, height = unit(0.08, "cm"), width_hint = 0.3,
                   pad_x = unit(0.3, "cm"), pad_y = unit(0.1, "cm"), text_cex = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         height = unit(0.9, "cm"), width = unit(0.9, "cm"),
                         pad_x = unit(0.25, "cm"), pad_y = unit(0.4, "cm"),
                         style = north_arrow_nautical(
                           line_width = 1, line_col = "grey80",fill = c("grey80", "grey30"),
                           text_col = "grey80"))+        
  coord_sf(xlim = lonlim, ylim =  latlim) + 
  scale_fill_gradientn(colours = oce::oceColorsPalette(120), limits = c(0,23), 
                       na.value = "white", name = "Speed\n (m/s)")+ 
  scale_x_continuous(breaks = lonlim)+ 
  theme_design()

wind.vector_sep       

#oct
wind.vector_oct = ggplot(data = NULL) +
  geom_raster(data = w_mean_oct, aes(x = longitude_oct, y = latitude_oct, fill = w_mean_oct$speed))+
  geom_segment(data = w_mean_oct, 
               aes(x = longitude_oct, xend = longitude_oct+u_oct/60, y = latitude_oct, 
                   yend = latitude_oct+v_oct/60), arrow = arrow(length = unit(0.08, "cm")))+
  labs(x = "Longitude", y = "Latitude", title = "Wind Map - South China Sea", subtitle = "October 2019")+
  geom_sf(data = studarea, fill = "#C7A77E", col = 1)+ 
  geom_sf(data = studlines, col = 1)+ 
  geom_label_repel(data = sa_points, aes(x=X, y=Y, label=name), size = 1.5,
                   label.padding = unit(0.1, "lines"), label.size = 0.01, color = "black", fill = "grey80") +
  annotation_scale(location = "br", bar_cols = c("black", "white"), 
                   line_width = 0.5, height = unit(0.08, "cm"), width_hint = 0.3,
                   pad_x = unit(0.3, "cm"), pad_y = unit(0.1, "cm"), text_cex = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         height = unit(0.9, "cm"), width = unit(0.9, "cm"),
                         pad_x = unit(0.25, "cm"), pad_y = unit(0.4, "cm"),
                         style = north_arrow_nautical(
                           line_width = 1, line_col = "grey80",fill = c("grey80", "grey30"),
                           text_col = "grey80"))+        
  coord_sf(xlim = lonlim, ylim =  latlim) + 
  scale_fill_gradientn(colours = oce::oceColorsPalette(120), limits = c(0,23), 
                       na.value = "white", name = "Speed\n (m/s)")+ 
  scale_x_continuous(breaks = lonlim)+ 
  theme_design()

wind.vector_oct       

#nov
wind.vector_nov = ggplot(data = NULL) +
  geom_raster(data = w_mean_nov, aes(x = longitude_nov, y = latitude_nov, fill = w_mean_nov$speed))+
  geom_segment(data = w_mean_nov, 
               aes(x = longitude_nov, xend = longitude_nov+u_nov/60, y = latitude_nov, 
                   yend = latitude_nov+v_nov/60), arrow = arrow(length = unit(0.08, "cm")))+
  labs(x = "Longitude", y = "Latitude", title = "Wind Map - South China Sea", subtitle = "November 2019")+
  geom_sf(data = studarea, fill = "#C7A77E", col = 1)+ 
  geom_sf(data = studlines, col = 1)+ 
  geom_label_repel(data = sa_points, aes(x=X, y=Y, label=name), size = 1.5,
                   label.padding = unit(0.1, "lines"), label.size = 0.01, color = "black", fill = "grey80") +
  annotation_scale(location = "br", bar_cols = c("black", "white"), 
                   line_width = 0.5, height = unit(0.08, "cm"), width_hint = 0.3,
                   pad_x = unit(0.3, "cm"), pad_y = unit(0.1, "cm"), text_cex = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         height = unit(0.9, "cm"), width = unit(0.9, "cm"),
                         pad_x = unit(0.25, "cm"), pad_y = unit(0.4, "cm"),
                         style = north_arrow_nautical(
                           line_width = 1, line_col = "grey80",fill = c("grey80", "grey30"),
                           text_col = "grey80"))+        
  coord_sf(xlim = lonlim, ylim =  latlim) + 
  scale_fill_gradientn(colours = oce::oceColorsPalette(120), limits = c(0,23), 
                       na.value = "white", name = "Speed\n (m/s)")+ 
  scale_x_continuous(breaks = lonlim)+ 
  theme_design()

wind.vector_nov

#dec
wind.vector_dec = ggplot(data = NULL) +
  geom_raster(data = w_mean_dec, aes(x = longitude_dec, y = latitude_dec, fill = w_mean_dec$speed))+
  geom_segment(data = w_mean_dec, 
               aes(x = longitude_dec, xend = longitude_dec+u_dec/60, y = latitude_dec, 
                   yend = latitude_dec+v_dec/60), arrow = arrow(length = unit(0.08, "cm")))+
  labs(x = "Longitude", y = "Latitude", title = "Wind Map - South China Sea", subtitle = "December 2019")+
  geom_sf(data = studarea, fill = "#C7A77E", col = 1)+ 
  geom_sf(data = studlines, col = 1)+ 
  geom_label_repel(data = sa_points, aes(x=X, y=Y, label=name), size = 1.5,
                   label.padding = unit(0.1, "lines"), label.size = 0.01, color = "black", fill = "grey80") +
  annotation_scale(location = "br", bar_cols = c("black", "white"), 
                   line_width = 0.5, height = unit(0.08, "cm"), width_hint = 0.3,
                   pad_x = unit(0.3, "cm"), pad_y = unit(0.1, "cm"), text_cex = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         height = unit(0.9, "cm"), width = unit(0.9, "cm"),
                         pad_x = unit(0.25, "cm"), pad_y = unit(0.4, "cm"),
                         style = north_arrow_nautical(
                           line_width = 1, line_col = "grey80",fill = c("grey80", "grey30"),
                           text_col = "grey80"))+        
  coord_sf(xlim = lonlim, ylim =  latlim) + 
  scale_fill_gradientn(colours = oce::oceColorsPalette(120), limits = c(0,23), 
                       na.value = "white", name = "Speed\n (m/s)")+ 
  scale_x_continuous(breaks = lonlim)+ 
  theme_design()

wind.vector_dec


#save single plots as pdf/jpg...
ggsave("1Windmean_jan", wind.vector_jan, device = "jpg")
ggsave("2Windmean_feb", wind.vector_feb, device = "jpg") 
ggsave("3Windmean_mar", wind.vector_mar, device = "jpg")
ggsave("4Windmean_apr", wind.vector_apr, device = "jpg")
ggsave("5Windmean_may", wind.vector_may, device = "jpg") 
ggsave("6Windmean_jun", wind.vector_jun, device = "jpg")
ggsave("7Windmean_jul", wind.vector_jul, device = "jpg")
ggsave("8Windmean_aug", wind.vector_aug, device = "jpg") 
ggsave("9Windmean_sep", wind.vector_sep, device = "jpg") 
ggsave("10Windmean_oct", wind.vector_oct, device = "jpg") 
ggsave("11Windmean_nov", wind.vector_nov, device = "jpg") 
ggsave("12Windmean_dec", wind.vector_dec, device = "jpg")


