#########################################
## Coursera Class, Exploratory Data Analysis
## Course Project 1
## Plot 2
##  Mark Becker

## For complete run:
## source('~/plot2.R')
## hpc <- read_hpc()
## plot2(hpc)

#########################################

read_hpc <- function() {
  read.csv("household_power_consumption.txt", sep=";"
           , colClasses = c("character", "character", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric")
           , na.strings = '?')
}

plot2 <- function(hpc_data_frame, dates = c('1/2/2007', '2/2/2007')) {
  png("plot2.png", width = 480, height = 480)
  
  hpc_subset <- subset(hpc_data_frame, Date %in% dates)
  date_times <- paste(hpc_subset$Date, hpc_subset$Time)
  x <- strptime(date_times, "%m/%d/%Y %H:%M:%S")
  y <- hpc_subset$Global_active_power
  
  dayseq <- 1:length(x)
  
  plot(dayseq, y, type = "l"
       #plot(x, y, type = "l"
       , xaxt = "n"
       , ylab = "Global Active Power(kilowatts)"
       , xlab = ""
       #, main = "Global Active Power", 
       #     , mar = c(2,2,2,2)
  )
  
  #axis.POSIXct(side = 1, at =seq(x[1], by = "day", length = 3), format = "%a")
  axis(1, c(0, 1440, 2880),  c("Thu", "Fri", "Sat"))
  
  dev.off()
}