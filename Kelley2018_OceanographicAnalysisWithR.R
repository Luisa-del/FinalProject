###############################################################################################################################################
###############################################################################################################################################
###############################################################################################################################################


library(testthat)
library(gsw)
library(oce)
library(ocedata)



###############################################################################################################################################
#Chapter 2#####################################################################################################################################
###############################################################################################################################################

#2.3 Syntax ####################################################################################################################################

#Expressions (p.12)-------------------
377/120

#       (p.24)-------------------
seas <- c("Mediterranean", "Adriatic", "Arabian", "Black", "Caspian", "Persian", "Red")
seas[c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE)]





#2.4 Graphics ##################################################################################################################################

#scatter and line plots (----------------------

data(buoy, package="ocedata")
head(buoy)


theta <- (90 - buoy$direction) * pi / 180
u <- -buoy$wind*cos(theta)
v <- -buoy$wind*sin(theta)

s <- c(-1, 1) * max(buoy$wind, na.rm=TRUE)
plot(u, v, xlab="u [m/s]", ylab="v [m/s]",
     xlim=s, ylim=s, asp=1)
for (ring in seq(5, 30, 5))
  lines(ring*cos(seq(0, 2*pi, pi/32)),
        ring*sin(seq(0, 2*pi, pi/32)), col="gray")


data(argo, package="oce")
head(argo)
summary(argo)
lat <- argo[["latitude"]]
lon <- argo[["longitude"]]

plot(lon, lat, asp=1/cos(pi*mean(range(lat))/180))
data(coastlineWorldMedium, package="ocedata")
cwlon <- coastlineWorldMedium[["longitude"]]
cwlat <- coastlineWorldMedium[["latitude"]]
polygon(cwlon, cwlat, col="gray")

#
















###############################################################################################################################################
#Chapter 3 ####################################################################################################################################
###############################################################################################################################################


# Annual-mean sea surface temperature shown in Mollweide projection (p.99)---------------------------------

data(levitus, package="ocedata")

# Although oce can easily select a colour-scale for the image, analysts usually prefer to set such things to achieve uniformity across plots, and this may be done with
cm <- colormap(zlim=c(-2, 30), col=oceColorsJet)
# which uses the “jet” color mapping (see Sect. 2.4.14). Then a palette is drawn with
drawPalette(colormap=cm)
#At a global scale, the coastline provided with oce provides sufficient detail and the Mollweide projection may be a good 
#choice (see Appendix C for more on projections); with these choices,
data(coastlineWorld, package="oce")
mapPlot(coastlineWorld, projection="+proj=moll",
        grid=FALSE)
#draws the gray land area in Fig. 3.3. Finally,
mapImage(levitus$longitude, levitus$latitude,
         levitus$SST, colormap=cm)
#adds the sea-surface temperature. Readers who are following along will notice
#that some of the image grid elements are painting over the land. This problem is
#alleviated by redrawing that land, after first drawing lines of longitude and latitude:
  mapGrid()
mapLines(coastlineWorld)
#thus completing Fig. 3.3.




###############################################################################################################################################
#Appendix C ###################################################################################################################################
###############################################################################################################################################

#See Page 248
#orld view, using the Winkel Tripel projection, popularized by the National Geographic Society
#The dots are the positions of Argo floats in January, 2018.

data(coastlineWorld, package="oce")
mapPlot(coastlineWorld, projection="+proj=wintri",
        col="lightgray")
year <- 2018
month <- 1
url <- "https://data.nodc.noaa.gov/argo/inv/basins"
for (basin in c("atlantic", "pacific", "indian")) {
  f <- sprintf("%s/%s/%s/%s%s%02d_argoinv.txt", url, basin,
               year, substr(basin, 1, 2), year, month)
  d <- read.csv(f, stringsAsFactors=FALSE)
  mapPoints(d$longitude_min, d$latitude_min,
            pch=20, cex=0.4)
}
