##############################################################################################################################
# R Script of Final Project ##################################################################################################
##############################################################################################################################

# SOURCE: URL: https://plotly-r.com/client-side-linking.html#fig:storms

#install.packages("plotly")
#install.packages("sf")

demo("sf-plotly-3D-globe", package = "plotly")


####### CODE FROM DEMO #####################################################################################################

library(sf)
library(plotly)
library(dplyr)
library(rgdal)


# latitude, longitude, and altitiude of tropical storms
storms <- sf::st_read(system.file("shape/storms_xyz.shp", package = "sf"), quiet = TRUE)
storms
head(storms)
str(storms)
plot(storms)


majorcurrents <- readOGR("/home/luisa/Documents/EAGLE_Master/MB2_Programming_Geostatistics/FinalProject/3DGlobe/Major_Ocean_Currents_arrowPolys_30m_8/Major_Ocean_Currents_arrowPolys_30m_8.shp")
#-> Source https://www.arcgis.com/home/item.html?id=24bfd85e97b042948e6ed4928dc45a8b
head(majorcurrents)
plot(majorcurrents)


# even grid of lat/lons spanning the globe (for creating the globe surface)
nlat <- 200
nlon <- 100

lat <- seq(-180, 180, length.out = nlat)
lon <- seq(-90, 90, length.out = nlon)

lat <- matrix(rep(lat, nlon), nrow = nlat)
lon <- matrix(rep(lon, each = nlat), nrow = nlat)


# helper function for converting polar (lat/lon) -> cartesian (x/y/z)
degrees2radians <- function(degree) degree * pi / 180 


# show as little as possible when hovering over surface
empty_axis <- list(
  showgrid = FALSE, 
  zeroline = FALSE, 
  showticklabels = FALSE, 
  showspikes = FALSE, 
  spikesides = FALSE, 
  title = "")

#'--> what if I change?
#'empty_axis <- list(showgrid = TRUE,   zeroline = TRUE,   showticklabels = TRUE,   showspikes = TRUE,   spikesides = TRUE, title = "storm")

# for centering camera/lighting on the center of the storm paths
xyzmean <- list(x = .41, y = -.71, z = 0.57)

# A 3D globe implemented with 3D lines and a spherical surface
 # Note that the globe has a radius of 1, but project the lines with 
 # a radius of 1.001 so that we appear on top of the surface
 globe <- plot_ly(height = 500) %>% 
   add_sf(
     data = sf::st_as_sf(maps::map("world", plot = FALSE, fill = TRUE)), 
     x = ~ 1.001 * cos(degrees2radians(x)) * cos(degrees2radians(y)),
     y = ~ 1.001 * sin(degrees2radians(x)) * cos(degrees2radians(y)),
     z = ~ 1.001 * sin(degrees2radians(y)),
     color = I("black"), size = I(1),
     hoverinfo = "none") %>%
   add_sf(
     data = highlight_key(storms, group = "current paths"),
     name = "current paths",
     x = ~ 1.001 * cos(degrees2radians(x)) * cos(degrees2radians(y)),
     y = ~ 1.001 * sin(degrees2radians(x)) * cos(degrees2radians(y)),
     z = ~ 1.001 * sin(degrees2radians(y)),
     color = ~z, size = I(6),
     text = ~paste("Latitude:", y, "<br>", "Longitude:", x, "<br>", "Altitude:", z),
     hoverinfo = "text") %>%
   add_surface(
     x = cos(degrees2radians(lon)) * cos(degrees2radians(lat)),
     y = sin(degrees2radians(lon)) * cos(degrees2radians(lat)),
     z = sin(degrees2radians(lat)),
     # NOTE: you can map a value to surfacecolor to encode, say air temp
     # for an example, see https://github.com/cpsievert/Weather_Stuff/blob/master/radiation-plot-3D.R
     # But here is a trick to set the surface color to a constant white
     surfacecolor = matrix(NA, nrow = nlat, ncol = nlon),
     showscale = FALSE, hoverinfo = "skip",
     lightposition = xyzmean, 
     contours = list(
       x = list(highlight = FALSE), 
       y = list(highlight = FALSE), 
       z = list(highlight = FALSE)
       )
     ) %>%
   layout(
     showlegend = FALSE,
     scene = list(
       xaxis = empty_axis,
       yaxis = empty_axis,
       zaxis = empty_axis,
       aspectratio = list(x = 1, y = 1, z = 1),
       camera = list(eye = xyzmean)
       )
     )

 
# spherical distance between the first point and every other point
 # https://en.wikipedia.org/wiki/Great-circle_distance
arc_dist <- function(lon, lat) {
       lon <- degrees2radians(lon)
       lat <- degrees2radians(lat)
       lon0 <- lon[1]
       lat0 <- lat[1]
       delta <- cos(abs(lon - lon0))
       acos(sin(lat0) * sin(lat) + cos(lat0) * cos(lat) * delta)
     }

# plot altitude of each storm versus the distance it has traveled

distanceByAlt <- storms %>%
     sf::st_coordinates() %>%
     as.data.frame() %>%
     group_by(L1) %>%
     mutate(dist = arc_dist(X, Y)) %>%
     rename(altitude = Z) %>%
     highlight_key(~L1, group = "storm paths") %>%
     plot_ly(x = ~dist, y = ~altitude, height = 400) %>%
     # plotly.js doesn't support color gradient along *2D* lines
     add_lines(color = I("gray")) %>%
     add_markers(
       color = ~altitude, hoverinfo = "text",
       text = ~paste("Distance:", round(dist, 2), "<br>", "Altitude:", altitude, "<br>", "Storm:", L1)
     ) %>%
     layout(
         showlegend = FALSE,
         title = "Tropical storm altitude by distance \n (click to highlight storm)",
         font = list(size = 15, family = "Balta"),
         margin = list(t = 60)
       )

# force persistent selection
 # TODO: persistence via shift should work with two separate graphs!!
options(persistent = TRUE)

library(htmltools)

browsable(tagList(globe, distanceByAlt))





############################################################################################################################
# try to run: https://github.com/cpsievert/Weather_Stuff/blob/master/radiation-plot-3D.R ###################################
############################################################################################################################

  
install.packages("reticulate")
library(reticulate)
library(plotly)

# use scipy to read from netcdf file since R's ncdf4 
# doesn't seem to know how to read it...
dat <- reticulate::py_run_file("radiation-read.py")

# generate a 2-way color gradient
breaks <- seq(0, 1, length.out = 14)
colors <- scales::colour_ramp(c('#313695', '#3a67af', '#5994c5', '#84bbd8', '#afdbea', '#d8eff5', '#d6ffe1', '#fef4ac', '#fed987', '#fdb264', '#f78249', '#e75435', '#cc2727', '#a50026'))(breaks) 
colorscale <- data.frame(breaks, colors)

# collection of simple features which define the land boundaries
world <- sf::st_as_sf(maps::map("world", plot = FALSE, fill = TRUE))

# hide all the axes
empty_axis <- list(
  showgrid = FALSE, 
  zeroline = FALSE,
  showticklabels = FALSE,
  title = ""
)

# helper function for converting polar -> cartesian
degrees2radians <- function(degree) degree * pi / 180 

# create a grid of points for the sphere
nlat <- length(dat$lat)
nlon <- length(dat$lon) + 1
lat <- matrix(rep(dat$lat, nlon), nrow = nlat)
lon <- matrix(rep(c(dat$lon, 179.5), each = nlat), nrow = nlat)

plot_ly() %>%
  add_surface(
    x = cos(degrees2radians(lon)) * cos(degrees2radians(lat)),
    y = sin(degrees2radians(lon)) * cos(degrees2radians(lat)),
    z = sin(degrees2radians(lat)),
    surfacecolor = dat$olr,
    colorscale = colorscale,
    colorbar = list(title = "W/m²"),
    text = round(dat$olr, 2),
    hoverinfo = "text"
  ) %>%
  add_sf(
    data = world,
    x = ~ 1.001 * cos(degrees2radians(x)) * cos(degrees2radians(y)),
    y = ~ 1.001 * sin(degrees2radians(x)) * cos(degrees2radians(y)),
    z = ~ 1.001 * sin(degrees2radians(y)),
    color = I("black"), hoverinfo = "none", size = I(1)
  ) %>%
  layout(
    title = "Outgoing Longwave Radiation Anomalies<br>Dec 2017-Jan 2018",
    showlegend = FALSE,
    scene = list(
      xaxis = empty_axis, 
      yaxis = empty_axis, 
      zaxis = empty_axis,
      aspectratio = list(x = 1, y = 1, z = 1),
      camera = list(eye = list(x = 2, y = 1.15, z = 0.5))
    )
  )


#############################################################################################################################
#############################################################################################################################
# MAJOR CURRENTS ############################################################################################################
#############################################################################################################################
#############################################################################################################################


####### CODE FROM DEMO #####################################################################################################

library(sf)
library(plotly)
library(dplyr)
library(rgdal)


# latitude, longitude, and altitiude of tropical storms
#storms <- sf::st_read(system.file("shape/storms_xyz.shp", package = "sf"), quiet = TRUE)
#storms
#head(storms)
#plot(storms)

majorcurrents <- readOGR("/home/luisa/Documents/EAGLE_Master/MB2_Programming_Geostatistics/FinalProject/3DGlobe/Major_Ocean_Currents_arrowPolys_30m_8/Major_Ocean_Currents_arrowPolys_30m_8.shp")
#-> Source https://www.arcgis.com/home/item.html?id=24bfd85e97b042948e6ed4928dc45a8b
head(majorcurrents)
plot(majorcurrents)


# even grid of lat/lons spanning the globe (for creating the globe surface)
nlat <- 200
nlon <- 100

lat <- seq(-180, 180, length.out = nlat)
lon <- seq(-90, 90, length.out = nlon)

lat <- matrix(rep(lat, nlon), nrow = nlat)
lon <- matrix(rep(lon, each = nlat), nrow = nlat)


# helper function for converting polar (lat/lon) -> cartesian (x/y/z)
degrees2radians <- function(degree) degree * pi / 180 


# show as little as possible when hovering over surface
empty_axis <- list(
  showgrid = FALSE, 
  zeroline = FALSE, 
  showticklabels = FALSE, 
  showspikes = FALSE, 
  spikesides = FALSE, 
  title = "")

#'--> what if I change?
#'empty_axis <- list(showgrid = TRUE,   zeroline = TRUE,   showticklabels = TRUE,   showspikes = TRUE,   spikesides = TRUE, title = "storm")

# for centering camera/lighting on the center of the storm paths
xyzmean <- list(x = .41, y = -.71, z = 0.57)

# A 3D globe implemented with 3D lines and a spherical surface
# Note that the globe has a radius of 1, but project the lines with 
# a radius of 1.001 so that we appear on top of the surface
globe <- plot_ly(height = 500) %>% 
  add_sf(
    data = sf::st_as_sf(maps::map("world", plot = FALSE, fill = TRUE)), 
    x = ~ 1.001 * cos(degrees2radians(x)) * cos(degrees2radians(y)),
    y = ~ 1.001 * sin(degrees2radians(x)) * cos(degrees2radians(y)),
    z = ~ 1.001 * sin(degrees2radians(y)),
    color = I("black"), size = I(1),
    hoverinfo = "none") %>%
  add_sf(
    data = highlight_key(majorcurrents, group = "storm paths"), #Error  
    name = "storm paths",
    x = ~ 1.001 * cos(degrees2radians(x)) * cos(degrees2radians(y)),
    y = ~ 1.001 * sin(degrees2radians(x)) * cos(degrees2radians(y)),
    z = ~ 1.001 * sin(degrees2radians(y)),
    color = ~z, size = I(6),
    text = ~paste("Latitude:", y, "<br>", "Longitude:", x, "<br>", "Altitude:", z),
    hoverinfo = "text") %>%
  add_surface(
    x = cos(degrees2radians(lon)) * cos(degrees2radians(lat)),
    y = sin(degrees2radians(lon)) * cos(degrees2radians(lat)),
    z = sin(degrees2radians(lat)),
    # NOTE: you can map a value to surfacecolor to encode, say air temp
    # for an example, see https://github.com/cpsievert/Weather_Stuff/blob/master/radiation-plot-3D.R
    # But here is a trick to set the surface color to a constant white
    surfacecolor = matrix(NA, nrow = nlat, ncol = nlon),
    showscale = FALSE, hoverinfo = "skip",
    lightposition = xyzmean, 
    contours = list(
      x = list(highlight = FALSE), 
      y = list(highlight = FALSE), 
      z = list(highlight = FALSE)
    )
  ) %>%
  layout(
    showlegend = FALSE,
    scene = list(
      xaxis = empty_axis,
      yaxis = empty_axis,
      zaxis = empty_axis,
      aspectratio = list(x = 1, y = 1, z = 1),
      camera = list(eye = xyzmean)
    )
  )


# spherical distance between the first point and every other point
# https://en.wikipedia.org/wiki/Great-circle_distance
arc_dist <- function(lon, lat) {
  lon <- degrees2radians(lon)
  lat <- degrees2radians(lat)
  lon0 <- lon[1]
  lat0 <- lat[1]
  delta <- cos(abs(lon - lon0))
  acos(sin(lat0) * sin(lat) + cos(lat0) * cos(lat) * delta)
}

# plot altitude of each storm versus the distance it has traveled

distanceByAlt <- storms %>%
  sf::st_coordinates() %>%
  as.data.frame() %>%
  group_by(L1) %>%
  mutate(dist = arc_dist(X, Y)) %>%
  rename(altitude = Z) %>%
  highlight_key(~L1, group = "storm paths") %>%
  plot_ly(x = ~dist, y = ~altitude, height = 400) %>%
  # plotly.js doesn't support color gradient along *2D* lines
  add_lines(color = I("gray")) %>%
  add_markers(
    color = ~altitude, hoverinfo = "text",
    text = ~paste("Distance:", round(dist, 2), "<br>", "Altitude:", altitude, "<br>", "Storm:", L1)
  ) %>%
  layout(
    showlegend = FALSE,
    title = "Tropical storm altitude by distance \n (click to highlight storm)",
    font = list(size = 15, family = "Balta"),
    margin = list(t = 60)
  )

# force persistent selection
# TODO: persistence via shift should work with two separate graphs!!
options(persistent = TRUE)

library(htmltools)

browsable(tagList(globe, distanceByAlt))
browsable(globe)

