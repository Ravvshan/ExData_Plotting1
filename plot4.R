## the following function plots graph and saves it in plot1.png file.
## example to run this script
## source("plot3.R")
## plot3()
## Note that it may take some time as to complete the reading and plotting the graph


plot4 <- function() {
  library(dplyr)
  
   datafile <- "./household_power_consumption.txt"
## just reading the head and first few rows to get column classess, which later helps to read whole datafile faster
  head5 <- read.table(datafile, header = TRUE, sep=";", na.strings="?", nrows = 5)
  
  classes <- sapply(head5, class) ## gets classes of each variable
  cnames <- names(head5) ## gives names of variables
  
## only reading the dates (first column) 
  dateColumn <- read.table(datafile, header = TRUE, sep=";", na.strings="?",colClasses = c("factor",rep("NULL",8)), nrows = 2075259)
## transform the dates into date format
dateColumn <- as.Date(dateColumn[,1], format="%d/%m/%Y")  

## finding the number of rows coming before the date that we need (2nd Feb 2007)
## which can be skipped from reading
nrow_skip <- which(dateColumn=="2007-02-01")[1]-1

## to find the last row number that we need
nrow_read <- tail(which(dateColumn=="2007-02-02"),n=1) - nrow_skip

## now read only the part of the data file that we need
data <- read.table(datafile, header = TRUE, sep=";", na.strings="?", nrows = nrow_read,skip=nrow_skip,col.names=cnames)    

data<-mutate(data, time0=as.numeric(strptime(paste(data$Date, data$Time, sep=" "),format="%d/%m/%Y %H:%M:%S")))

day1<-as.numeric(strptime(paste("01/02/2007", "00:00:00", sep=" "),format="%d/%m/%Y %H:%M:%S"))
day2<-as.numeric(strptime(paste("02/02/2007", "00:00:00", sep=" "),format="%d/%m/%Y %H:%M:%S"))
day3<-as.numeric(strptime(paste("03/02/2007", "00:00:00", sep=" "),format="%d/%m/%Y %H:%M:%S"))

## save plot to the PNG file
png(filename = "plot4.png", width = 480, height = 480, units = "px", pointsize = 12, bg = "white",  res = NA)

par(mfrow=c(2,2))
####################
plot(data$time0,data$Global_active_power, type="n", main="", xlab="",ylab="Global Active Power",xaxt="n")

axis(1, at=day1, labels="Thu"); axis(1, at=day2, labels="Fri"); axis(1, at=day3, labels="Sat")
lines(data$time0,data$Global_active_power)

################
plot(data$time0,data[,5], type="n", main="", xlab="datetime",ylab="Voltage",xaxt="n")
lines(data$time0,data[,5], col="black") 

axis(1, at=day1, labels="Thu"); axis(1, at=day2, labels="Fri"); axis(1, at=day3, labels="Sat") 

lines(data$time0,data[,5], col="black")
#############
plot(data$time0,data[,7], type="n", main="", xlab="",ylab="Energy Sub Metering",xaxt="n")
axis(1, at=day1, labels="Thu"); axis(1, at=day2, labels="Fri"); axis(1, at=day3, labels="Sat")

lines(data$time0,data[,7], col="black"); lines(data$time0,data[,8], col="red"); lines(data$time0,data[,9], col="blue")

legend('topright', names(data)[7:9] , lty=1, col=c('black', 'red', 'green'), bty='n', cex=1)
###############
plot(data$time0,data[,4], type="n", main="", ylab=names(data)[4],xlab="datetime",xaxt="n")
lines(data$time0,data[,4], col="black") 
axis(1, at=day1, labels="Thu"); axis(1, at=day2, labels="Fri"); axis(1, at=day3, labels="Sat")


dev.off() ## closing the device
print("plot is saved in plot4.png file!")
rm(list=ls())
## now the plot1.png file containing histogram must have been created.
}
