#########################################
## Coursera Class, Exploratory Data Analysis
## Course Project 1
## Plot 4
##  Mark Becker

## For complete run:
## source('~/plot4.R')
## hpc <- read_hpc()
## plot4(hpc)

#########################################



read_hpc <- function() {
  read.csv("household_power_consumption.txt", sep=";"
           , colClasses = c("character", "character", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric")
           , na.strings = '?')
}

plot4 <- function(hpc_data_frame, dates = c('1/2/2007', '2/2/2007')) {
  png("plot4.png", width = 480, height = 480)
  
  
  hpc_subset <- subset(hpc_data_frame, Date %in% dates)
  date_times <- paste(hpc_subset$Date, hpc_subset$Time)
  x <- strptime(date_times, "%m/%d/%Y %H:%M:%S")
  dayseq <- 1:length(x)
  
  par(mfrow = c(2,2))  #create 2x2 grid
  
  #########################################
  ## upper left, global active power
  #########################################
  
  y <- hpc_subset$Global_active_power
  ylimit = range(y)
  
  plot(dayseq, y, type = "l"
       #plot(x, y, type = "l"
       , xaxt = "n"
       , ylab = "Global Active Power"
       , xlab = ""
       , ylim = ylimit
  )
  
  axis(1, c(0, 1440, 2880),  c("Thu", "Fri", "Sat"))
  

  #########################################
  ## upper right, voltage
  #########################################
  
  y <- hpc_subset$Voltage
  ylimit = range(y)
  plot(dayseq, y, type = "l"
       #plot(x, y, type = "l"
       , xaxt = "n"
       , ylab = "Voltage"
       , xlab = "datetime"
       , ylim = ylimit
  )
  
  axis(1, c(0, 1440, 2880),  c("Thu", "Fri", "Sat"))
  
    
  #########################################
  ##lower left, energy sub metering  
  #########################################
  
  y <- hpc_subset$Sub_metering_1
  y2 <- hpc_subset$Sub_metering_2
  y3 <- hpc_subset$Sub_metering_3
  ylimit = range(y)
  
  plot(dayseq, y, type = "l"
       #plot(x, y, type = "l"
       , xaxt = "n"
       , ylab = "Energy sub metering"
       , xlab = ""
       , ylim = ylimit
       , col = "black"
  )
  
  lines(dayseq, y2, col = "red")
  lines(dayseq, y3, col = "blue")
  
  #axis.POSIXct(side = 1, at =seq(x[1], by = "day", length = 3), format = "%a")
  axis(1, c(0, 1440, 2880),  c("Thu", "Fri", "Sat"))
  
  legend("topright", c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), col = c("black", "red", "blue"), lty = c("solid", "solid", "solid"))
 
  
  #########################################
  ## lower right, global reactive power
  #########################################
  
  y <- hpc_subset$Global_reactive_power
  ylimit = range(y)
  
  plot(dayseq, y, type = "l"
       #plot(x, y, type = "l"
       , xaxt = "n"
       , ylab = "Global_reactive_power"
       , xlab = "datetime"
       , ylim = ylimit
  )
  
  axis(1, c(0, 1440, 2880),  c("Thu", "Fri", "Sat"))
   
  dev.off()
}