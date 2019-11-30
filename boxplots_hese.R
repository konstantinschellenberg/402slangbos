# Starter Script for GEO214 for csv data import from ecognition and boxplotting - without much reformat
# S. Hese 16.5.2016 Ü4 214  obia ü2
#
# setting the path to all our datasets and scripts
path <- "/Volumes/SSD-Data/home_darwin/01-Lectures/03-Fern-II-SS16-GEO214/Uebung-4-2"
#assigning the path value to setwd
setwd(path)
# defining the output format and the layout of the plotfile setting the device
pdf("mean-boxplots.pdf", width=11, height=7)
# par defines the geometry of the plot - setting the graphical parameters for pdf
par(mfrow=c(1,1))
# defining the data input to file
file <- "GEO214-Ue6-cpi-1mapVersion.csv"

# read table gets its data from file and reformats on the fly depending on out read.table parameters - "data" gets the output
data <- read.table(file, header=TRUE, sep=";", dec=".", skip=0)
 
# boxplot is doing the plotting to the pdf taking the item description from the data-file data.csv and data2.csv contents
boxplot (list("Green Channel"=data$MeanLayer2,"Red Channel"=data$MeanLayer3,"NIR Channel"=data$MeanLayer4,"NIR2 Channel"=data$MeanLayer5), col="purple",ylim=c(0,255),ylab="DN", xlab="Landsat Channels")

# dev is closed the pdf gets a file end mark and is closed
dev.off()



