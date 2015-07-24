library(dplyr)
library(graphics)

main <- function() {
    
    # Load the data from the current folder if necessary
    NEI <-load.NEI.SCC.dataset()

    # Subset for Baltimore 
    emissions_by_year <- extract.baltimore.emissions.data(NEI)    
    
    # plot to the screen
    plot.data(emissions_by_year) 
 
    # plot to the png device
    write.png(emissions_by_year,"plot2.png")
}

# Checks if the variables are already available in the global env; 
# if so return them to avoid reloading of a large amount of data
load.NEI.SCC.dataset <- function() {
    ge <- globalenv()
    if( is.null(ge$NEI)  ) 
        NEI <- readRDS("summarySCC_PM25.rds")
    NEI
}

extract.baltimore.emissions.data <- function(NEI) {
    Baltimore.subset = subset(NEI,fips == "24510")
    by_year = group_by(Baltimore.subset,year)
    emissions_by_year = summarise(by_year,emissions = sum(Emissions,na.rm = T))
}

plot.data <- function(emissions_by_year) {
    
    plot(
        emissions_by_year$year,
        emissions_by_year$emissions 
        ,main = "PM2.5 Baltimore Total Emissions",
        xlab = "year",
        ylab ="Emissions in tons",
        type = "o", 
        pch= 20
    )

    fit <- lm(emissions_by_year$emissions ~ emissions_by_year$year)
    abline(fit,col="red", lwd=3)
    
}

write.png  <- function(emissions_by_year,filename) {
    png(file=filename,width=1024,height=768,units="px")
    plot.data(emissions_by_year)
    dev.off() 
}


