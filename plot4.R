library(dplyr)
library(ggplot2)
library(grid)

main <- function() {
    
    # Load the data from the current folder 
    Data <-load.NEI.SCC.dataset()
    NEI  <- Data[[1]]
    SCC  <- Data[[2]]
    
    # Subset for US coal combustion
    merged <- extract.US.coal.combustion.data(NEI,SCC)    
    
    # Group and sum emissions from the merged data
    emissions_by_year <- group.by.year.and.sum(merged)
    
    # Create plot
    g <- plot(emissions_by_year) 
    
    # Write to png file
    write.png(g,"plot4.png") 
    
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


extract.US.coal.combustion.data <- function(NEI,SCC) {
    coal.combustion = SCC[grep("Fuel Comb.*?coal",SCC$EI.Sector,ignore.case = T),c(1,4)]
    merged = merge(NEI,coal.combustion,by.x="SCC",by.y="SCC")
}

group.by.year.and.sum <- function(merged) {
    by_year = group_by(merged,year)
    emissions_by_year = summarise(by_year,emissions = sum(Emissions,na.rm = T))
    emissions_by_year
}

plot <- function(emissions_by_year) {
    g <- ggplot(emissions_by_year,aes(year,emissions/1000))
    g <- g + 
        geom_line(color="steelblue",size=2) + 
        geom_smooth(method ="lm",se = F,size = 1,colour = "red") +
        labs(title = "U.S. Total Emissions by Coal Combustion") +
        labs(x="Year") + 
        labs(y="Emissions in thousands of tons") +
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
