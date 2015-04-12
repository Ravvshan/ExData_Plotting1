## the following function plots histogram and saves it in plot1.png file.

## example to run this script
## Rscript plot1.R
## Note that it may take some time as to complete the reading and plotting the graph
## The file household_power_consumption.txt is assumed to be in the working directory!

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
      
## save plot to the PNG file
png(filename = "plot1.png", width = 480, height = 480, units = "px", pointsize = 12, bg = "white",  res = NA)

hist(data[,3],col="red",main="Global Active Power",xla=" Global Active Power (kilowatts)")

dev.off() ## closing the device
print("plot is saved in plot1.png file!")
rm(list=ls())
## now the plot1.png file containing histogram must have been created.

