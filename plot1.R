#########################################
## Coursera Class, Exploratory Data Analysis
## Course Project 1
## Plot 1
##  Mark Becker

## For complete run:
## source('~/plot1.R')
## hpc <- read_hpc()
## plot1(hpc)

#########################################

read_hpc <- function() {
  read.csv("household_power_consumption.txt", sep=";"
                  , colClasses = c("character", "character", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric")
                  , na.strings = '?')
}

plot1 <- function(hpc_data_frame, dates = c('1/2/2007', '2/2/2007')) {
  png("plot1.png", width = 480, height = 480)
  
  hpc_subset <- subset(hpc_data_frame, Date %in% dates)
  
  hist(hpc_subset$Global_active_power, col = "red", xlim = c(0, 6), ylim =  c(0, 1200)
       , xlab = "Global Active Power(kilowatts", ylab = "Frequency", main = "Global Active Power", 
       , mar = c(2,2,2,2))
  
  dev.off()
}

