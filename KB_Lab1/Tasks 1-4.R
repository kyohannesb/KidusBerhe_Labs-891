### Lab 1 Tasks: 1-4
#All tasks will be found under their respective comment labeling
#! Answers to questions will be in associated document (Word document)

##loading packages and data (assuming first part is not run)-----------------------------
library(tidyverse)
library(ggplot2)
library(sf)
library(sp)

##data
p.counties <- "./data/County_Boundaries.shp"
p.stations <- "./data/Non-Tidal_Water_Quality_Monitoring_Stations_in_the_Chesapeake_Bay.shp"

##reading in the files
d.counties <- sf::read_sf(p.counties)
d.stations <- sf::read_sf(p.stations)

# check data validity
d.stations %>% sf::st_is_valid()
d.counties %>% sf::st_is_valid() ## Dr.PB: returns false for one feature, so we need to fix

## Dr.PB: fix it "in place"
d.counties <- d.counties %>% sf::st_make_valid()
#check validity again
d.counties %>% sf::st_is_valid()
## TASK 1: Basic Data Manipulation: --------------------------------------------

###Task 1.1.........percent land area

#creating new variable/data set with total area to do work on, keep original "safe"
totArea.county <- d.counties %>% mutate(totalArea = ALAND10 + AWATER10)
#calculating percentage area of total area for each county
totArea.county <- totArea.county %>% mutate(perctLand = (ALAND10/totalArea)*100)
#displays only the percent of land area without needing to see table
totArea.county%>%
  as_tibble() %>% dplyr::select(-geometry) %>% 
  summarise(perctLand)

###Task1.2.........percent water area and max

#calculate percent of land that is water
totArea.county <- totArea.county %>% mutate(perctWater = (AWATER10/totalArea)*100)
#group the data by state then show only  rows for maximum water percentage of land
wtrPctMax <- totArea.county %>%
  as_tibble() %>% dplyr::select(-geometry) %>% 
  group_by(STATEFP10) %>%
  slice(which.max(perctWater)) 
wtrPctMax #quick view
#reduce the amount of data that is viewed to just what was asked in the question
wtrPctMax %>% dplyr::select(STATEFP10, NAMELSAD10,perctWater ) %>% head()

###Task 1.3........count of counties in each state

totArea.county %>%
  as_tibble() %>% dplyr::select(-geometry) %>% 
  group_by(STATEFP10) %>%
  count(STATEFP10)
#output shows "n" which is count for all sub data in each group

###Task 1.4.......Station with shortest name in study area
#station with the shortest name:
min(d.stations$STATION_NA)


## TASK 2: Plotting attribute data----------------------------------------------

###Task 2.1..............Scatterplot for water and land area with state ID colours 

totArea.county %>%
  ggplot(., aes(x = AWATER10, y = ALAND10)) +
  geom_point(size=1.5, aes(colour = STATEFP10)) + #use colour, not fill, as the latter keeps them the same colour (nothing to fill)
  labs(title = "Relationship between land and water for all counties")+
  scale_x_discrete(name ="Water Area (m2)")+
  scale_y_discrete(name ="Land Area (m2)")

###Task 2.2............Histogram of drainage area for all stations

d.stations %>%
  ggplot(., aes(x = Drainage_A)) +
  geom_histogram(aes(fill = MAJOR_WATE)) + #I thought it would be useful to show which major water sources are responsible using colour
  labs(title = "Drainage Area for all stations")+
  scale_x_discrete(name ="Drainage Area (m2)")

###Task 2.3............Histogram with state variable as colour

#check crs of both files
totArea.county %>% sf::st_crs() == d.stations %>% sf::st_crs()
#intersecting the datas to join the state ID
station.county <- sf::st_intersection(d.stations, totArea.county)
#plotting histogram with state colours
station.county %>%
  ggplot(., aes(x = Drainage_A)) +
  geom_histogram(aes(fill = STATEFP10)) + #I thought it would be useful to show which major water sources are responsible using colour
  labs(title = "Drainage Area for all stations - State Coloured")+
  scale_x_discrete(name ="Drainage Area(m2)")


## TASK 3: Write a function------------------------------------------------------

###Task 3.1............write a function

x = c(1, 0, -1) #<-----please place vectors of choice here
#for copying/testing: c(1, 0, -1) , c(10, 100, 1000), c(.1, .001, 1e8), c("a", "b", "c").

task3.function <- function(x){
  if (is.numeric(x)==TRUE){
    mean <- mean(x)
    med <- median(x)
    max <- max(x)
    min <- min(x)
    sort<- sort(x)
    vec.List <- list("mean" = mean, "Median" = med, "Maximum"= max, "Minimum"=min, "Sorted"=sort)
    return(vec.List)
  } else{
    print("error: not numeric!") #created error statement
  }
}

task3.function(x)


## TASK 4: more complex spatial analysis---------------------------------------

## Task 4.1..........calculate number of monitoring station in each state

station.county 
station.county %>%
  as_tibble() %>% dplyr::select(-geometry) %>% 
  group_by(STATEFP10) %>%
  count(STATEFP10)
#output id a list showing state FIPs and "n" = count. As this is the station file it groups and counts. 

## Task 4.2...........average size of New York counties in Study area

#select new york data only
newYork.area <- subset(station.county, STATEFP10==36)
#summarise average into an output
newYork.area %>%
  as_tibble() %>% dplyr::select(-geometry) %>% 
  summarise(avgSizCount.NewYrk = mean(totalArea))
#produces a result that is the average of the counties of New York within the study area.


## Task 4.3...........State with greatest drainage area
station.county

drainageMax <- station.county %>%
  as_tibble() %>% dplyr::select(-geometry) %>% 
  group_by(STATEFP10) %>%
  summarise(avgDrain = mean(Drainage_A)) 
drainageMax
topDrain <- drainageMax %>%
  slice(which.max(avgDrain))
topDrain
#returns the max value as well as the state FIP. state with Max is Pennsylvania

