##############################################################################################################################
#R Script to Build Animation of Arctic Sea Ice Extent#########################################################################
##############################################################################################################################


#' R Script to Build Animation of Arctic Sea Ice Extent – Update 12/20/13
#' URL: https://www.r-bloggers.com/r-script-to-build-animation-of-arctic-sea-ice-extent-update-122013/





  
library(animation)

ani.options
ani.options(convert=shQuote('C:\Program Files (x86)\ImageMagick-6.7.9-Q16\convert.exe')) #??
ani.options(convert=shQuote('/home/luisa/ImageMagick-7.0.9-24')) #??
## Use setwd() to specify directory where you want png images to be saved
setwd("/home/luisa/Documents/EAGLE_Master/MB2_Programming_Geostatistics/FinalProject/ArcticSeaIceExtent/")
# use png_yn to toggle between plot output to png file or screen
png_yn <- "y"
# Establish chart series patterns and colors to be able to distinguish current yr from previous years in plot
pattern <- c(rep("dashed", 5), rep("solid", 12))
ser_col <- c(rep("black",5),rep("grey",12))
# Establish chart annotations for date, chart title,
what_date <- format(Sys.Date(), "%b %d, %Y")  # with month as a word
title <- paste("IARC-JAXA Daily Arctic Sea Ice Extent*n", what_date)
note_1 <- "*Extent - Area of Ocean with at least 15% Sea Ice"
par(oma=c(2,1,1,1)); par(mar=c(2,4,2,1))
#  Day of year axis setup
## Set up basic day of year vectors (mon_names, 1st day of mon)
mon_names <- c("Jan", "Feb", "Mar", "April", "May", "June", "July", "Aug", "Sept", "Oct","Nov","Dec")
mon_doy <- c(1,32,60,91,121,151,182,213,244,274,305,335,366)
mon_pos <- c(16, 46, 75, 106,135, 165, 200, 228, 255, 289, 320, 355)
# Read JAXA Arctic Sea ice Extent csv file
# Data File: Month,Day,1980's Avg,1990's Avg,2000's Average,2002:2012
link <- <a_class="vglnk" href="http://www.ijis.iarc.uaf.edu/seaice/extent/plot.csv" rel="nofollow"><span>http</span><span>://</span><span>www</span><span>.</span><span>ijis</span><span>.</span><span>iarc</span><span>.</span><span>uaf</span><span>.</span><span>edu</span><span>/</span><span>seaice</span><span>/</span><span>extent</span><span>/</span><span>plot</span><span>.</span><span>csv</span></a>"
j_data <- read.csv(link, header = F, skip=1, na.strings = c(-9999))
series_id <-  c("mo", "day", "1980s", "1990s", "2000s","2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009",
                "2010", "2011", "2012", "2013")
colnames(j_data) <- series_id
# File has data for each day in 366 day year
# Establish Day of year
for (i in 1:366)   j_data$yr_frac[i] <- i
#convert ASIE to millions Km^2
j_data[,c(3:17)] <- j_data[,c(3:17)]/1000000
# Loop through years
for (j in 3:17)
{
  png_name <- paste("asie",series_id[j],".png",sep="")
  if (png_yn =="y") png(filename=png_name)
  which_yr <- j
  no_yrs <- j
  # Calc min asie for year
  min_asie <- min(j_data[,j], na.rm = T)  # must remove na's to get valid answer
  lab_asie <- round(min_asie,3)
  min_r <- which(j_data[,j] == min_asie)
  min_d <- j_data[min_r,2]
  min_m <- j_data[min_r,1]
  min_date <- paste(min_m,"/",min_d,"/",series_id[j], sep="")
  plot(j_data[,17],  type="n", col = "grey",axes=F, xlab="",
       ylab="Arctic Sea Ice Extent - Millions Sq KM",
       ylim=c(0,15),xaxs="i", yaxs = "i",
       main=title)
  text(20, 1.5, note_1, cex = 0.8, adj=0, col = "black")
  text(20,1,"Data Source: <a class="vglnk" href="http://www.ijis.iarc.uaf.edu/seaice/extent/plot.csv" rel="nofollow"><span>http</span><span>://</span><span>www</span><span>.</span><span>ijis</span><span>.</span><span>iarc</span><span>.</span><span>uaf</span><span>.</span><span>edu</span><span>/</span><span>seaice</span><span>/</span><span>extent</span><span>/</span><span>plot</span><span>.</span><span>csv</span></a>", cex = 0.8, adj=0,col = "black")
  mtext("D Kelly O'Day - <a class="vglnk" href="https://chartsgraphs.wordpress.com" rel="nofollow"><span>https</span><span>://</span><span>chartsgraphs</span><span>.</span><span>wordpress</span><span>.</span><span>com</span></a>", 1,0.5, adj = 0, cex = 0.8, outer=T)
  # custom x & y axes
  axis(side = 1, at=mon_doy, labels=F, xaxs="i")
  axis(side=1, at= mon_pos, labels=mon_names, tick=F, line=F, xaxs="i")
  axis(side=2,  yaxs="i", las=1)
  points(70, min_asie, col = "red",pch=19, cex = 2)
  # Add each previous yr data series as light grey line
  for (n in 3:no_yrs)
  {
    points(j_data[,18], j_data[,n], type="l",lwd=1,lty=pattern[j], col=ser_col[j])
    text(182,14,series_id[j], col = "red", cex = 1.1)
  }
  points(j_data[,18], j_data[,j], col="red", type="l",lwd=2.5)
  text(182,14,series_id[j], col = "red", cex = 1.1)
  text(120,min_asie+0.5, min_date, col="red", cex=0.9)
  text(120,min_asie, lab_asie, col="red", cex=0.9)
  if(png_yn == "y") dev.off()
}
## copy last png file 3 times to provide pause in animation
if(png_yn== "y")
{
  for (c in 1:2)
  {
    file_name <- paste("asie2012",c, ".png",sep="")
    file.copy(from= "asie2012.png", to = file_name, overwrite=T)
  }
  ani.options(outdir = getwd())    # direct gif output file to working dir
  ani.options(interval= 0.80)
  im.convert("asie*.png", "last_animation.gif")
}
</em>
