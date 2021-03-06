##      its necessary to install the 'data.table' and 'lubridate' packages.
## 
##      install.packages("data.table")
##      install.packages("lubridate")
##      

plot4 <- function () {
        
 
        library("data.table")
        library("lubridate")
        
        
        df <- fread("household_power_consumption.txt", header = TRUE, 
                       sep = ";", na.strings = c("?", ""), 
                    data.table = FALSE, colClasses = "character", 
                    showProgress = TRUE )
        
        headers <- names (df)
        
        myData <- suppressWarnings( data.frame( as.Date(df[, 1 ], "%d/%m/%Y"),
                                                df[,2],
                                                as.numeric(df[, 3 ]), as.numeric(df[, 4 ]),
                                                as.numeric(df[, 5 ]), as.numeric(df[, 6 ]), 
                                                as.numeric(df[, 7 ]), as.numeric(df[, 8 ]),
                                                as.numeric(df[, 9 ]),
                                                dmy_hms(paste(df[,1], df[,2], sep = " ")) ) )
        
        
        
        
        names(myData) <- c(headers, "TT" )
        
        
        good <- complete.cases(myData)
        
        myData <- myData[good,]
        
        myData <- subset.data.frame (myData, 
                                     myData$Date >= as.Date( "01022007",  "%d%m%Y") & 
                                     myData$Date <= as.Date( "02022007",  "%d%m%Y"))
        
        png("plot4.png", width = 480, height = 480)
        
        par(mfrow = c(2,2))
        
        plot(myData$TT, myData$Global_active_power, type = "l", 
             ylab = "Global Active Power", xlab = "")
        
        plot(myData$TT, myData$Voltage, type = "l", 
             ylab = "Voltage", xlab = "datetime")
        
        plot(myData$TT, myData$Sub_metering_1, type = "l", col = "black", 
             xlab = "",
             ylab = "Energy sub metering") 
        lines(myData$TT, myData$Sub_metering_2, type = "l", col = "red")
        lines(myData$TT, myData$Sub_metering_3, type = "l", col = "blue") 
        
        legend("topright", col = c("black","red", "blue"), 
               legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), 
               pch = c(NA, NA, NA),
               pt.cex = 1,
               cex = 0.9,
               bty = "n",
               lty = c(1, 1, 1))
        
        plot(myData$TT, myData$Global_reactive_power, type = "l", 
             ylab = "Global_reactive_powew", xlab = "datetime")
        
        
        dev.off()
        
        myData        
}