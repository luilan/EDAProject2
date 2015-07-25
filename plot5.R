library(dplyr)
library(ggplot2)

main <- function() {
    
    # Load the data from the current folder 
    Data <-load.NEI.SCC.dataset()
    NEI  <- Data[[1]]
    SCC  <- Data[[2]]
    
    # Subset for Baltimore 
    merged <- extract.baltimore.vehicles.emissions.data(NEI,SCC)    
    
    # Group and sum emissions from the merged data
    emissions_by_year <- group.by.year.and.sum(merged)
    
    # Create plot
    g <- plot(emissions_by_year) 
    
    # Write to png file
    write.png(g,"plot5.png") 
    
    # Print on Screen
    print(g)
}

# Checks if the variables are already available in the global env; 
# if so return them to avoid reloading of a large amount of data
load.NEI.SCC.dataset <- function() {
    ge <- globalenv()
    if( is.null(ge$NEI)  ) 
        NEI <- readRDS("summarySCC_PM25.rds")
    if( is.null(ge$NEISCC)  ) 
        SCC <- readRDS("Source_Classification_Code.rds")
    list(NEI,SCC)
}


extract.baltimore.vehicles.emissions.data <- function(NEI,SCC) {
    Baltimore.subset = subset(NEI,NEI$fips == "24510" )
    vehicles.emissions = SCC[ grep("Vehicles",SCC$SCC.Level.Two) ,c(1,2,3,7)]
    merged = merge(Baltimore.subset,vehicles.emissions,by.x="SCC",by.y="SCC")
}

group.by.year.and.sum <- function(merged) {
    by_year = group_by(merged,year,fips)
    emissions_by_year = summarise(by_year,emissions = sum(Emissions,na.rm = T))
    emissions_by_year
}

plot <- function(emissions_by_year) {
    g <- ggplot(emissions_by_year,aes(year,emissions))
    g <- g + 
        geom_line(color="steelblue",size=2) + 
        geom_smooth(method ="lm",se = F,size = 1,colour = "red") +
        labs(title = "Baltimore motor vehicle emissions") +
        labs(x="Year") + 
        labs(y="Emissions in Tons") +
        theme(
            axis.title=element_text(size=16),
            plot.title = element_text(size = 18)
        )
    g
 }

write.png  <- function(g,filename) {
    png(file=filename,width=1024,height=768,units="px")
    print(g)
    dev.off() 
    g
}


