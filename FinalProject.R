#############################################################################################################
# Final Project R-Course 2019/20 ############################################################################
#############################################################################################################
# Aim: Creating a Wind Map for the South China Sea ##########################################################
#############################################################################################################
# Luisa Pflumm ### 6th of May 2020 ##########################################################################
#############################################################################################################

# -> load necessary packages --------------------------------------------------------------------------------

library(lubridate)
library(rWind)
library(rnaturalearth)
library(raster)
library(rgeos)
library(sf)
library(ggplot2)
library(ggspatial)
library(ggrepel)
library(easycsv)



# -> set working directories --------------------------------------------------------------------------------

# set home-working directory
home_dir <- easycsv::choose_dir()
setwd(home_dir)


# Define output directory of downloads and export folder 
datafolder <- "Winddata"
mapfolder <- "Windmap"
dir.create(datafolder,showWarnings = TRUE)
dir.create(mapfolder,showWarnings = TRUE)


# Rewrite folders paths for data and export 
data_directory <- paste0(home_dir,"/Winddata", "/")
map_directory <- paste0(home_dir,"/Windmap", sep = "/")



# -> Set necessary parameters and download and store winddata for all months --------------------------------

# Generate Months
Month_List <- sprintf("%02d", 1:12)
Month_Name <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")


# select requested year, winddata available from server since January 2012
year <- 2019


# define number of days of a month

numberOfDays <- function(date) {
  
  m <- format(date, format="%m")
  
  while (format(date, format="%m") == m) {
  
      date <- date + 1
  }
  return(as.integer(format(date - 1, format="%d")))
}


# Define Lon/lat of Studyarea South China Sea
lonlim <- c(95, 125)
latlim <- c(-10, 25)


# Download the data and calculate the monthly average within a loop
# -> Download takes long time for whole year

for (k in 1:length(Month_List)){
  
  Name <- paste0(Month_List[k],"_w_mean_", Month_Name[k])  
  
  Start_Date <- paste(year,k,1,00,00,00, sep="-")
  
  Numb_Days <- numberOfDays(as.Date(Start_Date, "%Y-%m-%d" ))
  
  Interval <- 6
  
  Dates_List <- seq(ymd_hms(Start_Date),
                ymd_hms(paste(year,k,Numb_Days,18,00,00, sep="-")), by= paste(Interval ,"hours"))
  
  serie <- wind.dl_2(Dates_List,lonlim[1], lonlim[2], latlim[1], latlim[2])
  
  serie_mean <- wind.mean(serie)
  
  assign(Name,serie_mean)
  
  rm(serie,serie_mean)
  
}


# create List of downloaded data
Data_List <- list(`01_w_mean_Jan`,`02_w_mean_Feb`, `03_w_mean_Mar`,`04_w_mean_Apr`,
                  `05_w_mean_May`,`06_w_mean_Jun`,`07_w_mean_Jul`,`08_w_mean_Aug`,
                  `09_w_mean_Sep`,`10_w_mean_Oct`,`11_w_mean_Nov`,`12_w_mean_Dec`)


# Save downloaded data in folder
setwd(data_directory)
saveRDS(Data_List, file = paste0(year,"_", "Monthly_Windmean.Rds"))


#############################################################################################################

# -> alternative for proceeding without waiting for complete download:
# -> copy already downloaded dataset of 2017, 2018 or 2019 windmean into datafolder (Rds data)

# import list of predownloaded data from datafolder
setwd(data_directory)

Data_List <- readRDS("2019_Monthly_Windmean.Rds") #select required datalist

#############################################################################################################


# -> Prepare necessary elements for the map -----------------------------------------------------------------

# create bounding box for studyarea
bbox <- st_bbox(as(extent(lonlim, latlim), "SpatialPolygons"))


# download world data of Natural Earth Dataset for countries and labels and crop to studyarea
countries <- ne_countries(scale = "medium", type = "countries", returnclass = "sf")
countries <- st_crop(countries, bbox)


# create centroid country labels
labels <- cbind(countries, st_coordinates(st_centroid(countries$geometry)))


# Set label-name for Studyarea
region <- "South China Sea"


# -> ggplot wind map for every month of required year for the South China Sea -------------------------------

# set working directory for exported maps
setwd(map_directory)


for(i in 1:length(Data_List)){
  
  Graph <- ggplot() +
   
    geom_raster(data = Data_List[[i]],                                        #plot speed
                aes(x = Data_List[[i]]$lon, y = Data_List[[i]]$lat, fill = Data_List[[i]]$speed))+
    
    geom_segment(data = Data_List[[i]],                                       #plot direction
                 aes(x = Data_List[[i]]$lon, xend = Data_List[[i]]$lon+Data_List[[i]]$ugrd10m/60,
                    y = Data_List[[i]]$lat, yend = Data_List[[i]]$lat+Data_List[[i]]$vgrd10m/60),
                 arrow = arrow(length = unit(0.08, "cm")))+
    
    labs(x = "Longitude", y = "Latitude",                                     #axes and title
         title = paste0("Wind Map - ",region), subtitle = paste0(Month_Name[i]," ",year))+ 
    
    geom_sf(data = countries, fill = "#C7A77E", col = 1)+                     #country-polygons
    
    geom_label_repel(data = labels, aes(x=X, y=Y, label=name), size = 2,      #country-labels
                     label.padding = unit(0.1, "lines"), label.size = 0.01,
                     color = "black", fill = "grey80", point.padding = NA) +
    
    annotation_scale(location = "tl", bar_cols = c("black", "white"),        
                     line_width = 0.5, height = unit(0.08, "cm"), width_hint = 0.3,
                     text_cex = 0.6,)+
    
    annotation_north_arrow(location = "tl", which_north = "true",           
                           height = unit(0.9, "cm"), width = unit(0.9, "cm"),
                           pad_y = unit(0.5, "cm"),
                           style = north_arrow_nautical(
                             line_width = 1, line_col = "black",fill = c("grey80", "black"),
                             text_col = "black"))+
    
    coord_sf(xlim = lonlim, ylim =  latlim, expand = FALSE) +   
    
    scale_fill_gradientn(colours = rev(RColorBrewer::brewer.pal(7, "RdYlBu")),
                         limits = c(0, 25), na.value = "white", name = "Speed\n (m/s)")+
                         # if highest mean-windspeed exceed 25 m/s -> change limit manually
    
    theme_bw() +                                                              
    
    theme(plot.title = element_text(size = 10, colour = 1) ,
          plot.subtitle = element_text(size = 8, colour = 1),
          axis.text = element_text(size = 8, colour = 1),
          axis.title = element_text(size = 7, colour = 1),
          legend.text = element_text(size = 7, colour = 1),
          legend.title = element_text(size = 7, colour = 1), 
          legend.position = c(1.1,.44), 
          legend.key.height = unit(1.3, "cm"))
  
  ggsave(paste0(year,"-",Month_List[i],Month_Name[i],"_Windmean.png"),  
         Graph, width = 17, height = 15, units = "cm")
  
}



#############################################################################################################
# END #######################################################################################################
#############################################################################################################