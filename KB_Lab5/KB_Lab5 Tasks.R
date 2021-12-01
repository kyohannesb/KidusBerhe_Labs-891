
#   NAME = KIDUS Y BERHE
#   Lab 05 - Dynamic mapping in Leaflet

library(tidyverse)
library(sf)
library(tmap)
library(stringr)
library(leaflet)
library(raster)
library(dplyr)
library(spData)
library(rgdal)
library(spdep)

####contents of comments:-------------------------------------------------------
#!# = Overall task
## = Subdivisions of Tasks
# = regular comment on work or line
#!!! = important / note this
#----->ANS = next line will contain answer to sub-task


#!# Reading in the required data:-----------------------------------------------

#Reading in Spatial data (and validating)
counties <- sf::read_sf("./data/County_Boundaries.shp") %>% sf::st_make_valid()
sf::st_crs(counties)

myStates <- sf::read_sf("./data/myStates.shp")
myStates <- myStates %>% sf::st_transform(., "EPSG:4326")
sf::st_crs(myStates) 

hokBounds <- sf::read_sf("./data/Hokkaido_admin1.shp") %>% sf::st_make_valid()#Hokkaido boundaries 
hokBounds <- hokBounds %>% sf::st_transform(., "EPSG:4326") #ensuring UTM 54N
sf::st_crs(hokBounds)

hokRail <- sf::read_sf("./data/hotosm_jpn_north_railways_lines.shp") %>% sf::st_make_valid() #Hokkaido Railway network
hokRail <- hokRail %>% sf::st_transform(., "EPSG:4326") #reprojecting 

hokWater <- sf::read_sf("./data/hotosm_jpn_north_waterways_polygons.shp") %>% sf::st_make_valid() 
hokWater <- hokWater %>% sf::st_transform(., "EPSG:4326")#reprojecting 

hokDEM <- raster("./data/Hokkaido_DEM.tif") 
hokDEM <- projectRaster(hokDEM, crs= "EPSG:4326") #reprojecting DEM
sf::st_crs(hokDEM)

#Reading in aspatial data
bmps <- read_csv("./data/BMPreport2016_landbmps.csv")


#!# Task 1: Lab2 - task 2.3 BMPs total cost with leaflet------------------------

#creating "IDs" from available data for join

FIPtemp <- bmps
FIPtemp <- FIPtemp %>% mutate(., FIPSCode = stringr::str_sub(GeographyName, 1, 5)) #shorten FIPS code
FIPtemp <- FIPtemp %>% mutate(., FIP = as.numeric(as.character(FIPtemp$FIPSCode))) #turns values in to numbers then adds them back to the list
summary(FIPtemp$FIP) #verify it is numeric and give quick summary (should not have 0s)

GEOID10temp <- counties
GEOID10temp <- GEOID10temp %>% mutate(., GEOID10_NUM = as.numeric(as.character(GEOID10temp$GEOID10))) #turns values in to numbers then adds them back to the list
summary(GEOID10temp$GEOID10_NUM) #verify it is numeric and give quick summary (should not have 0s)

#Calculating total costs of BMPs 
groupBmpTemp <- FIPtemp%>% group_by(StateAbbreviation, FIP) %>% summarise(totalCost = sum(Cost, na.rm=T))

#joining the created data sets
joinCountBMP <- left_join(GEOID10temp, groupBmpTemp, by = c("GEOID10_NUM" = "FIP")) #joining the datasets
class(joinCountBMP)
sf::st_crs(joinCountBMP)

#----->ANS
#map using leaflet

bins <- c(0, 26930000, 53858600, 80790000, 107718000, 161576000)
colo <- c("#c7e9b4", "#7fcdbb", "#41b6c4", "#2c7fb8", "#253494")
pallet <- colorBin(colo, domain = joinCountBMP$totalCost, bins = bins)
Base_Map <- providers$Esri.NatGeoWorldMap

labels <- sprintf(
  "<strong>%s</strong><br/>Total County Cost:$%g",
  joinCountBMP$NAME10, joinCountBMP$totalCost) %>% lapply(htmltools::HTML)

task1_Map <- leaflet(joinCountBMP) %>% 
  setView(-75, 39, 6) %>% #(lng, lat and zoom)
  addProviderTiles(Base_Map, group = "Base_Map") %>%
  addProviderTiles(providers$Stamen.Terrain, group = "Terrain") %>%
  addPolygons(fillColor = ~pallet(totalCost),
              weight = 2,
              opacity = 1,
              color = "black",
              dashArray = 1,
              fillOpacity = 1,
              highlightOptions = highlightOptions( weight = 5,
                                                   color = "yellow",
                                                   bringToFront = TRUE),
              label = labels,
              labelOptions = labelOptions(style = list(
                "font.weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>% 
  addLegend(pal = pallet, values = ~totalCost, opacity = 0.7, title = "Total BMP cost ($)",
            position = "bottomright") %>%
  addLayersControl(baseGroups = c("Base_Map", "Terrain"),
    position = "topleft")

#----->ANS
task1_Map


#!# Task 2: Lab3 - bonus task 2: Cholorpleth of LISA ---------------------------

statePop18_61 <- myStates %>% mutate(POP18_61 = DP0040001 - DP0060001)# calculates the raw value of pop between 18 and 61
statePop18_61 <- statePop18_61 %>% mutate(POPrt18_61 = (POP18_61 / DP0010001)*100) #adds column with percentage of total pop (18-61)
statePop18_61$POPrt18_61 #shows the percentages of the population between 18 and 61
statePop18_61 <- statePop18_61 %>% sf::st_transform(., "EPSG:4326")

#creating spatial weights
neighB <- spdep::poly2nb(statePop18_61, queen = TRUE)
weightState <- nb2listw(neighB, style="W", zero.policy=FALSE)
weightState$weights[1]

#creating and checking moran's I
POP18_61MC <- moran.mc(statePop18_61$POPrt18_61, weightState, nsim=999)
POP18_61MC
plot(POP18_61MC, main="", las=1)
MIplot <- moran.plot(statePop18_61$POPrt18_61, weightState, zero.policy=TRUE, plot=TRUE)

#pulling the required data (making vector to hold values)
vect1 <- as.vector(scale(statePop18_61$POPrt18_61))
labs1 <- as.character(statePop18_61$NAMELSAD10) #pulling names to show when data is plotted
#creating table with the values presented on moran plot. x= xaxis, wx= yaxis (Spatial lag)  
moranPop18_61 <- moran.plot(vect1, weightState, labels = labs1)
moranPop18_61 #table showing completed values

locM18_61 <- localmoran(statePop18_61$POPrt18_61, weightState)[, 5]

#categorize and add LISA rankings to table
lisaPop18_61 <- moranPop18_61 %>% 
  mutate(., LISA = ifelse((x>=0 & wx >= 0) , "HH",
                          ifelse((x < 0 & wx>= 0) , "LH",
                          ifelse((x >=0 & wx < 0) , "HL",
                          ifelse((x< 0 & wx < 0) ,"LL", NA))))) %>%
  mutate(., LISAnum = ifelse(LISA == "HH", 1,
                             ifelse(LISA =="HL",2,
                                    ifelse(LISA =="LH",3,
                                           ifelse(LISA =="LL", 4, NA)))))

lisaPop18_61 #checking if table was made properly

#joining the LISA back to file with geometry

finalLISA <- merge(statePop18_61, lisaPop18_61, by.x= "NAMELSAD10", by.y = "labels")
finalLISA


#creating leaflet map with LISA values
bins1 <-  c(0, 1, 2, 3, 4)
colo1 <- c("#d7191c", "#fdae61", "#abd9e9", "#2c7bb6")
pallet3 <- colorBin(colo, domain = finalLISA$LISAnum, bins = bins1)

task2_Map <- leaflet(finalLISA) %>%
  setView(-96, 41, 6) %>%
  addTiles(., group = "Base") %>%
  addProviderTiles(providers$Stamen.Terrain, group = "Terrain") %>%
  addProviderTiles(providers$Esri.NatGeoWorldMap, 
                   group = "National Geo Map") %>%
  addPolygons(fillColor = ~pallet3(finalLISA$LISAnum),
              weight = 2,
              opacity = 1,
              color = "black",
              dashArray = 1,
              fillOpacity = 1,
              highlightOptions = highlightOptions( weight = 5,
                                                   color = "yellow",
                                                   bringToFront = TRUE),
              popup = paste0("P-value: ", locM18_61) ,
              label = finalLISA$NAMELSAD10) %>%
  addLegend(pal = pallet3, values = finalLISA$LISA,
    title = "LISA Values:1=HH 2=HL 3=LH 4=LL",
    position = "bottomleft") %>%
  addLayersControl(baseGroups = c("Base", "Terrain", "National Geo Map"),
                   position = "bottomright",
                   options = layersControlOptions(collapsed = FALSE))

#----->ANS
task2_Map



#!# Task 3: Lab4 - task 2: multi-map with leaflet ------------------------------

pal2 <- colorNumeric(c("#d9d9d9","#969696", "#525252", "#252525" ), values(hokDEM),
                    na.color = "transparent")

label1 <- sprintf(
  "<strong>Municiplality Name: %s</strong><br/>Admin 2 P_Code: %s",
  hokBounds$ADM2_EN, hokBounds$ADM2_PCODE) %>% lapply(htmltools::HTML)
label2 <- sprintf(
  "<strong>Kanji Name: %s</strong><br/> Transport type: %s",
  hokRail$name, hokRail$railway) %>% lapply(htmltools::HTML)


task3_Map <- leaflet() %>%
  setView(142.46, 43.21, 7) %>% 
  addProviderTiles(providers$Esri.NatGeoWorldMap, group = "English") %>%
  addTiles(., group = "Base") %>%
  addPolygons(data = hokBounds,
              fillColor = "green",
              weight = 2,
              opacity = 0.5,
              color = "black",
              dashArray = 1,
              fillOpacity = 0.5,
              highlightOptions = highlightOptions( weight = 5,
                                                   color = "yellow",
                                                   bringToFront = FALSE),
              label = label1,
              labelOptions = labelOptions(style = list("font.weight" = "normal", padding = "3px 8px"),
                                          textsize = "15px",
                                          direction = "auto"),
              group = "municipalities") %>%
  addPolylines(data = hokRail,
               color = "red",
               fillOpacity = 1,
               dashArray = 1,
               highlightOptions = highlightOptions( weight = 10,
                                                    color = "white",
                                                    bringToFront = TRUE),
               label = label2,
               labelOptions = labelOptions(style = list(
                 "font.weight" = "normal", padding = "3px 8px"),
                 textsize = "15px",
                 direction = "auto"),
               group = "Hokkaido Trainway") %>%
  addCircleMarkers (141.357828, 43.08349, radius = 10, #adding the "one thing"
                    color = "blue",
                    popup = "Sapporo",
                    fillOpacity = 1,
                    label = "Capital of Hokkaido",
                    labelOptions = labelOptions(style = list("font.weight" = "normal", padding = "3px 8px"),
                                                textsize = "15px",
                                                direction = "auto"),
                    group = "Prefecture Capital") %>%
  #addRasterImage(hokDEM, colors = ~pal2(values), opacity = 1,
                 #group = "Hokkaido DEM") %>%
  addLayersControl(baseGroups = c("Base", "English"),
                   overlayGroups = c("municipalities", "Hokkaido Trainway", "Prefecture Capital"),
                   position = "bottomright",
                   options = layersControlOptions(collapsed = FALSE))
#!!! note: The raster still caused issues and after trying for too long to fix it i had to stop because of time

#----->ANS

task3_Map






#!# All Maps : -----------------------------------------------------------------

task1_Map

task2_Map

task3_Map
