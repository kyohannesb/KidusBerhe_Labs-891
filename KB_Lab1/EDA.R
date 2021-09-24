
#Exploratory Data Analysis
## lab tutorial portion.

##Loading in the Packages and data-----------------------------------------------
#packages
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

##quick "glimpse" at the data
glimpse(d.counties)
glimpse(d.stations)

# check data validity
d.stations %>% sf::st_is_valid()
d.counties %>% sf::st_is_valid() ## Dr.PB: returns false for one feature, so we need to fix

## Dr.PB: fix it "in place"
d.counties <- d.counties %>% sf::st_make_valid()
#check validity again
d.counties %>% sf::st_is_valid() #"False" has been fixed, can be done by running same line from above again as well



##"Selecting" data - dplyr-------------------------------------------------------

#selecting specified attributes
d.counties %>% dplyr::select(GEOID10, ALAND10) %>% head()
#removing attributes using select and "-" before field name
d.counties %>% dplyr::select(-NAME10) %>% head()
#specifies actual range to keep, drops those outside
d.counties %>% dplyr::select(GEOID10:CLASSFP10) %>% head()
#specifies which ranges to remove
d.counties %>% dplyr::select(-(GEOID10:CLASSFP10)) %>% head()
#selects attributes that have "C" as first letter in column header
d.counties %>% dplyr::select(starts_with("C")) #still maintains geometry



##Grouping data with "Mutate" - dplyr--------------------------------------------

#calculation done in place
d.counties %>% group_by(STATEFP10) %>% mutate(stateLandArea = sum(ALAND10))
#summarise even more, removes geometry to make it easier
d.counties %>%
  as_tibble() %>% dplyr::select(-geometry) %>% #Dr.PB: this line converts the data because of wonky geometry
  group_by(STATEFP10) %>%
  summarise(stateLandArea = sum(ALAND10)) #brings up only state ID and new created column, states total land area 

##A diversion into plots, mixing them in - ggplot -------------------------------

d.counties %>%
  ggplot(., aes(x = as.factor(STATEFP10), y = ALAND10)) +
  geom_boxplot(aes(fill = STATEFP10))
  #visualise the earlier summarised data

#another example of a plot
d.counties %>%
  ggplot(., aes(x = ALAND10)) +
  geom_histogram(aes(fill = STATEFP10)) +
  labs(title = "Tutorial histogram plot")
  #produces histogram that shows count in relation to area with colours indicating state


## Spatial operation -----------------------------------------------------------

#checking CRS
d.counties %>% sf::st_crs()
  #note the GEOGCRS and "LengthUnit"
d.stations %>% sf::st_crs()
  #same projection
#to check projection in same line:
d.counties %>% sf::st_crs() == d.stations %>% sf::st_crs()
#only returns binary output but can be used as a quick confirmation. 

##selecting specific attributes using filter function - dplyr
#selecting only Delaware counties
dE.counties <- d.counties %>% dplyr::filter(STATEFP10 == 10) #10 is Delaware's FP code

##finding all stations within the selected Delaware's data set using "Intersect"
#selects those within bounds
dE.stations <- sf::st_intersection(d.stations, dE.counties)
#checking created selection/intersection
glimpse(dE.stations)
#plotting the data
plot(dE.stations)

#checking area of each county
dE.counties %>% st_area()
#returns calculated values in a vector showing units used as well

