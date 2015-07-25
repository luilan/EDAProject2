library(dplyr)
library(ggplot2)
library(grid)

main <- function() {
    
    # Load the data from the current folder if necessary
    Data <-load.NEI.SCC.dataset()
    NEI  <- Data[[1]]
    SCC  <- Data[[2]]
    
    # Subset for Baltimore and LA County, filter for vehicle emissions only and merge
    emissions_by_year_type <- extract.baltimore.emissions.data(NEI,SCC)    
    
    # Create plot
    g <- plot(emissions_by_year_type) 
    
    # Write to png file
    write.png(g,"plot3.png") 
    
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


extract.baltimore.emissions.data <- function(NEI,SCC) {
     Baltimore.subset = subset(NEI,fips == "24510")
     by_type = group_by(Baltimore.subset,type,year)
     emissions_by_type = summarise(by_type,emissions = sum(Emissions,na.rm = T))
}


plot <- function(emissions_by_type_year) {
    g <- ggplot(emissions_by_type_year,aes(year,emissions))
    g <- g + geom_line(color="steelblue",size=2) + facet_grid(. ~ type) 
    g <- g + labs(title = "Baltimore PM2.5 Emissions by Type") 
    g <- g + labs(x="Year") + labs(y="Emissions in Tons") 
    g <- g + theme_grey() + 
        theme(
        axis.title=element_text(size=16),
        plot.title = element_text(size = 18),
        strip.text.x = element_text(  size = 16,hjust = 0.5, vjust = 0.5)
        ,panel.margin = unit(c(5),"mm")
        ,plot.margin = unit(c(5,5,5,5),"mm")
        )
}

write.png  <- function(g,filename) {
    png(file=filename,width=1024,height=768,units="px")
    print(g)
    dev.off() 
    g
}
