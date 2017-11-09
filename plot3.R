plot3 <- function () {
    ## Initiate the enviroment
    setup_env()
    
    ## Load data into script
    dt <- load_data()
    
    ## Draw plot1
    draw(dt)
}

setup_env <- function () {
    ## Create data directory to keep all dataset files in the same folder.
    if (!dir.exists("./data")) {
        dir.create("./data")
        
        if (!file.exists("./data/household_power_consumption.txt")) {
            URL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
            
            download.file(URL,destfile = "./data/temp.zip")
            unzip(zipfile = "./data/temp.zip", exdir = "./data/")
            
            ## Unlink unneccesary file
            unlink("./data/temp.zip")
        }
    } 
}

load_data <- function () {
    
    ## Check the existence of data.table library and install it in case of missing.
    list.of.packages <- c("data.table")
    new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
    if(length(new.packages)) install.packages(new.packages)
    
    ## Load the data.table package
    library(data.table)
    
    
    ## Load features labels for dataset
    dt <- fread("./data/household_power_consumption.txt", na.strings = "?")
    
    ## First convert Date column to Date type in order to filter 
    ## data for desired days and then apply strptime on data junk
    ## in this way we get performance boost since strptime is not 
    ## fast enough to apply over all the records
    dt[, Date := as.Date(Date, "%d/%m/%Y")]
    
    ## Filter colected data during first and second of Feb 2007
    sample <- dt[Date >= as.Date("2007-02-01") & Date <= as.Date("2007-02-02"),]
    
    ## Bootstrap the Date/Time column
    sample[,"datetime" := strptime(paste(Date,Time), format = "%Y-%m-%d %H:%M:%OS")]
    
    ## Free memory from redundant Date and Time columns
    sample[,c("Date","Time") := NULL]
}

draw <- function(dt) {
    png("plot3.png", width = 480, height = 480)
    plot(dt$datetime, 
         dt$Sub_metering_1, 
         type = 'l', 
         ylab = "Energy sub metering", 
         xlab = "")
    
    lines(dt$datetime, dt$Sub_metering_2, col = 'red')
    lines(dt$datetime, dt$Sub_metering_3, col = 'blue')
    
    legend("topright", pch = "_", 
           col = c("black", "red", "blue"), 
           legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
    
    ## Save the plot in png file
    dev.off()
}