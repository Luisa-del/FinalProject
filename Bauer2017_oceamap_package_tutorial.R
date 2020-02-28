###############################################################################################################################################
#Working with Oceanmap-package##############################################################################################################################################
###############################################################################################################################################

library(oceanmap)

# Map of the South China Sea by using longitude and latitude coordinates
lon <- c(95, 125)
lat <- c(-10, 25)
figure(width = 9.75, height = 5.28)
figure(width = 20, height = 20)
plotmap(lon=lon, lat=lat ,main="South China Sea")
plotmap(xlim=lon, ylim=lat ,main="South China Sea")

# -> add.region() noch erforschen wegen plot mit landmask, define region label...




# Bathymetry of the South China Sea (lon-lat)
lon <- c(95, 125)
lat <- c(-10, 25)
get.bathy(lon=lon, lat=lat, main="South China Sea", cbpos='r')

## load & plot bathymetry data from the NOAA - ETOPO1 database (Kelley p.16/17)
par(mfrow = c(2,1))
bathy <- get.bathy(lon=lon, lat=lat, terrain=T, res=3, keep=T, visualize=T, subplot=TRUE, grid=F, cbpos="r")

figure(width = 9.75, height = 5.28)
v(bathy, param="bathy", subplot=TRUE, terrain=T, levels=c(200 ,2000), cbpos="r", main="South China Sea") # change map by adding parameters

