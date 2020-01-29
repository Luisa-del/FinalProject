#doesn't install packages


library(usethis)
library(devtools)
install.packages("RStoolbox")
devtools::install_github("bleutner/RStoolbox")

library(RStoolbox)


library(rgdal)

#QGIS, OTB
#Home-Folder
#Terminal?!  -> vpn


##run this in order to update r (https://stackoverflow.com/questions/13656699/update-r-using-rstudio/29050116)
install_github('andreacirilloac/updateR')
library(updateR)
updateR(admin_password = 'Admin user password')