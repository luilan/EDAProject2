library(dplyr)
library(graphics)

main <- function() {
    
    # Load the data from the current folder if necessary
    NEI <-load.NEI.SCC.dataset()
    
    # Subset for Baltimore 
    emissions_by_year <- extract.US.emissions.data(NEI)    
    
    # plot to the screen
    plot.data(emissions_by_year) 
    
    # plot to the png device
    write.png(emissions_by_year,"plot1.png")
}

# Checks if the variables are already available in the global env; 
# if so return them to avoid reloading of a large amount of data
load.NEI.SCC.dataset <- function() {
    ge <- globalenv()
    if( is.null(ge$NEI)  ) 
        NEI <- readRDS("summarySCC_PM25.rds")
    NEI
}

extract.US.emissions.data <- function(NEI) {
    by_year = group_by(NEI,year)
    emissions_by_year = summarise(by_year,emissions = sum(Emissions,na.rm = T))
}

plot.data <- function(emissions_by_year) {
    
    plot(
    emissions_by_year$year,
    emissions_by_year$emissions/1000000, 
    main = "PM2.5 U.S. Total Emissions",
    xlab = "year",
    ylab ="Emissions in ML of tons",
    type = "o", 
    pch= 20
    )
    
    fit <- lm(emissions_by_year$emissions/1000000 ~ emissions_by_year$year)
    abline(fit,col="red", lwd=3)
    
}

write.png  <- function(emissions_by_year,filename) {
    png(file=filename,width=1024,height=768,units="px")
    plot.data(emissions_by_year)
    dev.off() 
}





# library(dplyr)
# 
# ## Load the dataset
# NEI <- readRDS("summarySCC_PM25.rds")
# SCC <- readRDS("Source_Classification_Code.rds")
# 
# ## Group the dataset by year and sum the PM2.5 emissions
# by_year = group_by(NEI,year)
# emissions_by_year = summarise(by_year,emissions = sum(Emissions,na.rm = T))
# 
# ## Plot the PM2.5 emissions by year in all U.S.
# plot1 <- function(emissions_by_year) {
# 
#     plot(
#     emissions_by_year$year,
#     emissions_by_year$emissions/1000000, 
#     main = "PM2.5 U.S. Total Emissions",
#     xlab = "year",
#     ylab ="Emissions in ML of tons",
#     type = "o", 
#     pch= 20
#     )
# 
#     fit <- lm(emissions_by_year$emissions/1000000 ~ emissions_by_year$year)
#     abline(fit,col="red", lwd=3)
#     
# }
# 
# 
# ## Write to the PNG device 
# png(file="plot1.png",width=480,height=480,units="px")
# plot1(emissions_by_year)
# dev.off() 
# 
# 
# 
# 
# 
# 
# 
