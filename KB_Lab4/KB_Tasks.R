# NAME = Kidus Y Berhe
# Class = GEOG 491/891
# Lab 4 - Making static maps

library(tidyverse)
library(sf)
library(raster)
library(dplyr)
library(spData)
library(grid)
library(tmap)  
library(RColorBrewer)
library(leaflet)
library(ggplot2)
library(GISTools)

####contents of comments:-------------------------------------------------------
#!# = Overall task
## = Subdivisions of Tasks
# = regular comment on work or line
#!!! = important / note this
#----->ANS = next line will contain answer to sub-task

#!# Reading in required data Task 1: -------------------------------------------

lanCount <- sf::read_sf("./data/lancaster_county.shp") %>% sf::st_make_valid() 

streams <- sf::read_sf("./data/Streams_303_d_.shp") %>% sf::st_make_valid() 
sf::st_crs(streams) <- crs(lanCount)

parks <- sf::read_sf("./data/State_Park_Locations.shp") %>% sf::st_make_valid() 
sf::st_crs(parks) <- crs(lanCount)

countBounds <- sf::read_sf("./data/County_Boundaries-_Census.shp") %>% sf::st_make_valid()  
sf::st_crs(countBounds) <- crs(lanCount)

neCounties <- read.csv(file = './data/ne_counties.csv')

muniBounds <- sf::read_sf("./data/Municipal_Boundaries.shp")%>% sf::st_make_valid() 
sf::st_crs(muniBounds) <- crs(lanCount)

lanDEM <- raster("./data/lc_dem.tif") 
raster::crs(lanDEM) <- crs(lanCount)


#!# Task 1: Good FrankenMap ----------------------------------------------------

## I. creating state data frame: calculations and join
#!!! data used will be percent of 20 to 29 year olds in Nebraska
pop20s <- neCounties %>% mutate( Mpop20to19 = M20Y + M21Y + M22to24Y + M25to29Y) #calculating total male 20 year olds
pop20s <- pop20s %>% mutate( Fpop20to19 = F20Y + F21Y + F22to24Y + F25to29Y) #calculating total female 20 year olds
pop20s <- pop20s %>% mutate( Tot20spop = Mpop20to19 + Fpop20to19) %>% mutate( perc20sTot = (Tot20spop / Total) * 100 ) #calculating total 20s and ratio of total pop

joinNE_pop20s <- merge(countBounds, pop20s, by.x= "NAME10", by.y = "NAME10" ) #joining variables to add social data to spatial
colr <- brewer.pal(5, "PuBuGn")
firstMap <- tm_shape(joinNE_pop20s) + tm_fill(col = "perc20sTot", title = "NE % 20-29yrs", style="equal" ,
                                              palettte = colr) +
  tm_borders(col = "black", lwd = 1.8, lty = "solid")+ tm_scale_bar(breaks = c(0,25,50,75,100), 
                                                                    text.size = 0.5, position = c("right", "top"),
                                                                    bg.color = "grey" ) + 
  tm_layout(main.title = "Nebraska Counties - Lancaster Highlighted")
firstMap #inset map using joined data and frankenmap template. 
#!!! attempted different methods of changing the colour to purple, blue and green however was unable, 
#!!! it always returned yellow, orange and red.

## II. creating county data frame: intersect and map

lanMuniCount <- st_intersection(muniBounds,lanCount)
lanAll <- st_intersection(streams,lanMuniCount)
lanPark <- st_intersection(parks, lanCount)

secondMap <- tm_shape(lanMuniCount)+
  tm_fill("NAME",legend.show = FALSE, alpha = 1)+
  tm_text("NAME", size = 0.5)+
  tm_shape(lanAll)+
  tm_lines("Impairment", alpha = 1) + 
  tm_layout(legend.text.size = 0.5, legend.outside.size = 0.5, legend.outside = TRUE)+
  tm_shape(lanPark)+
  tm_symbols(col = "AreaName", alpha = 1, size = 1)+
  tm_layout(legend.outside = TRUE ,legend.outside.position = c("right")) + 
  tm_shape(lanDEM) + 
  tm_raster(alpha = 0.4, palette = colorRampPalette(c("#f7f7f7", "#969696", "#252525"))(12),
            legend.show = F) + tm_compass(type = "8star", position = c("right", "top"), size = 2) +
  tm_layout(main.title="  Map of Lancaster County", title.size = 1.1) +
  tm_scale_bar(breaks = c(0,5,10,15,20), 
               text.size = 0.5, position = c("right", "bottom"))
secondMap

## III. creating final Map:

Lancaster.region = st_bbox(c(xmin = -96.91394, xmax = -96.46363,
                             ymin = 40.52302, ymax = 41.04612),
                           crs = st_crs(lanCount)) %>% st_as_sfc()

Inset.map <- tm_shape(Lancaster.region) +
  tm_borders(lwd = 5, col="Blue")
Inset.map
finalFirstMap <- firstMap + Inset.map
finalFirstMap

secondMap
print(finalFirstMap, vp = viewport(0.7, 0.24, width = 0.55, height = 0.55))


#!# Reading in required data Task 2: -------------------------------------------
#the data was created from larger files from QGIS after attempting with rstuido but computer couldn't handel it
#QGis was used to clip the larger files

# AREA: Hokkaido, Japan
hokBounds <- sf::read_sf("./hokkaido/Hokkaido_admin1.shp") %>% sf::st_make_valid()#Hokkaido boundaries 
hokBounds %>% sf::st_transform(., "ESRI:102154") #ensuring UTM 54N

hokRail <- sf::read_sf("./hokkaido/hotosm_jpn_north_railways_lines.shp") %>% sf::st_make_valid() #Hokkaido Railway network
sf::st_crs(hokRail) <- crs(hokBounds)

hokWater <- sf::read_sf("./hokkaido/hotosm_jpn_north_waterways_polygons.shp") %>% sf::st_make_valid() 
sf::st_crs(hokWater) <- crs(hokBounds)

hokDEM <- raster("./hokkaido/Hokkaido_DEM.tif") 
raster::crs(hokDEM) <- crs(hokBounds)


#!# Task 2: Map of our choosing: Hokkaido Railways, water and DEM --------------


hokMap <- tm_shape(hokBounds)+
  tm_borders(col = "black", alpha = 0.2)+
  tm_shape(hokRail)+
  tm_lines(col = "red", alpha = 1)+
  tm_add_legend('line', col = "red",
                labels = "Hokkaido Railway")+
  tm_layout(legend.position = c(0.8,0.1))+
  tm_shape(lanPark)+
  tm_symbols(col = "AreaName", alpha = 1, size = 1)+
  tm_layout(legend.outside = TRUE ,legend.outside.position = c("right")) + 
  tm_shape(lanDEM) + 
  tm_raster(alpha = 0.4, palette = colorRampPalette(c("#f7f7f7", "#969696", "#252525"))(12),
            legend.show = F) + tm_compass(type = "8star", position = c("right", "top"), size = 2) +
  tm_layout(main.title="  Map of Lancaster County", title.size = 1.1) +
  tm_scale_bar(breaks = c(0,5,10,15,20), 
               text.size = 0.5, position = c("right", "bottom"))

tm_shape(hokWater)+
  tm_polygons(col = "blue", alpha = 1, size = 1)+
  tm_borders(col = "blue", alpha = 1)
  


tm_add_legend('symbol', 
              col = RColorBrewer::brewer.pal(4, "YlOrRd"),
              border.col = "grey40",
              size = bubble_sizes,
              labels = c('0-15 mln','15-25 mln','25-35 mln','35-40 mln'),
              title="Population Estimate")

