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













####################################################




# For info on formatting:
# http://docs.ggplot2.org/0.9.3/theme.html
## Load the dataset
# NEI <- readRDS("summarySCC_PM25.rds")
# SCC <- readRDS("Source_Classification_Code.rds")
# 
# 
# coal.combustion = SCC[grep("coal",SCC$Short.Name,ignore.case = T),c(1,3)]
# merged = merge(NEI,coal.combustion,by.x="SCC",by.y="SCC")
# 
# 
# ## Group the dataset by year and sum the PM2.5 emissions
# by_year = group_by(merged,year)
# emissions_by_year = summarise(by_year,emissions = sum(Emissions,na.rm = T))
# 
# ## Plot the PM2.5 emissions by year in all U.S.
# plot4 <- function(emissions_by_year) {
#     
#     plot(
#         emissions_by_year$year,
#         emissions_by_year$emissions/1000, 
#         main = "PM2.5 U.S. Total Emissions by Coal Combustion",
#         xlab = "year",
#         ylab ="Emissions in Thousands of tons",
#         type = "o", 
#         pch= 20
#     )
#     
#     fit <- lm(emissions_by_year$emissions/1000 ~ emissions_by_year$year)
#     abline(fit,col="red", lwd=3)
#     
# }
# 
# 
# plot4(emissions_by_year)


# library(ggplot2)
# library(scales)
# library(grid)
# 
# 
# # For info on formatting:
# # http://docs.ggplot2.org/0.9.3/theme.html
# ## Load the dataset
# NEI <- readRDS("summarySCC_PM25.rds")
# 
# Baltimore.subset = subset(NEI,fips == "24510")
# by_type = group_by(Baltimore.subset,type,year)
# emissions_by_type = summarise(by_type,emissions = sum(Emissions,na.rm = T))
# 
# plot3 <- function(emissions_by_type) {
# 
# g <- ggplot(emissions_by_type,aes(year,emissions))
# g <- g + geom_line(color="steelblue",size=2) + facet_grid(. ~ type) 
# g <-  g + labs(title = "Baltimore PM2.5 Emissions by Type") 
# g <-  g + labs(x="Year") + labs(y="Emissions in Tons") 
# g <- g + theme_grey() + theme(
#     #axis.text=element_text(size=16),
#     axis.title=element_text(size=16),
#     plot.title = element_text(size = 18),
#     strip.text.x = element_text(  size = 16,hjust = 0.5, vjust = 0.5)
#     ,panel.margin = unit(c(5),"mm")
#     ,plot.margin = unit(c(5,5,5,5),"mm")
#  
#                             ) 
# print(g)
# 
# }
# 
# 
# ## Write to the PNG device 
# png(file="plot3.png",width=1024,height=768,units="px")
# plot3(emissions_by_type)
# dev.off() 
# 
