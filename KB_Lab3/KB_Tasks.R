# NAME = Kidus Y Berhe
# Class = GEOG 491/891
# Lab 3 - Spatial autocorrelation, globally and locally

install.packages("rgdal")
library(spdep)
library(sf)
library(tidyverse)
library(tmap)
library(rgdal)

####contents of comments:-------------------------------------------------------
#!# = Overall task
## = Subdivisions of Tasks
# = regular comment on work or line
#!!! = important / note this
#----->ANS = next line will contain answer to sub-task

#!# Task 1: Spatial subset (4min, 7max, contigous, save as shapefile) ----------

## subseting selected states and creating shapefile

#allStates <- sf::read_sf("./data/County_2010Census_DP1.shp") #call in state census data

myStates <- allStates %>% dplyr::filter(str_starts(GEOID10,"31")|str_starts(GEOID10,"20")|str_starts(GEOID10,"29")|str_starts(GEOID10,"19")|str_starts(GEOID10,"46"))
#above code filters selected states into "myStates"
tmap::tm_shape(myStates) + tm_polygons() #quick plot to ensure all states were selected

myStates <- myStates %>% sf::st_transform(., "ESRI:102010")#changing the projection to equidistant (distance will matter for neighbourhoods)
st_write(myStates, "./data/myStates.shp")#saving the data as a shapefile
myStates <- sf::read_sf("./data/myStates.shp") #checking to see if it can still be read in
sf::st_crs(myStates) #checking the projection held
#----->ANS
tmap::tm_shape(myStates) + tm_polygons() #quick plot to ensure all states are still there


#!# Task 2: Select and normalise value: ----------------------------------------
#selecting pop above 18 and below 62. 

statePop18_61 <- myStates %>% mutate(POP18_61 = DP0040001 - DP0060001)# calculates the raw value of pop between 18 and 61
statePop18_61 <- statePop18_61 %>% mutate(POPrt18_61 = (POP18_61 / DP0010001)*100) #adds column with percentage of total pop (18-61)
#----->ANS
statePop18_61$POPrt18_61 #shows the percentages of the population between 18 and 61

#!# Task 3: make histogram of chosen variable ----------------------------------

#----->ANS
hist(statePop18_61$POPrt18_61) #histogram of the percent of population that 18-61 year olds
#this histogram shows that more states have 50% to 60% of 18-61 than lower or higher percentages

#!# Task 4: Choropleth map of variable ---------------------------------------

stategroups <- statePop18_61 %>% mutate(., FIP = stringr::str_sub(GEOID10, 1, 2))%>%
  group_by(FIP) %>% summarise()

#----->ANS
tm_shape(statePop18_61)+
  tm_fill("POPrt18_61",
          style = "equal",#equal scheme used as percentages will be better shown
          n = 6, #6 classes to show more distribution between 50% and 60%
          palette = "Blues") +
  tm_borders(alpha = 0.5)+
  tm_shape(stategroups)+
  tm_borders(col = "red", alpha = 1)
#plot showing the distribution of the percentages across the chosen states 
#groupby used to denote state boundaries
#had difficulty getting border to fill as FIP colour (auto assigned) so used this instead (after a couple of hours if twiking)


#!# Task 5 : Contiguity based weight matrix and plots --------------------------

#----->ANS
#creating neighbourhood using queen 
neighB <- spdep::poly2nb(statePop18_61, queen = TRUE)

## A: : standardising weights
weightState <- nb2listw(neighB, style="W", zero.policy=FALSE)
#----->ANS
weightState$weights[1] #test to see standardisation

## B: Plot histogram of neighbours

neighborsHist <- attr(weightState$weights,"comp")$d
#----->ANS
hist(neighborsHist)

## C: finding average values of neighbours

POP18_61.lag <- lag.listw(weightState, statePop18_61$POPrt18_61)
POP18_61.lag #check to see values are averaged

## D: Creating moran plot

POP18_61MC <- moran.mc(statePop18_61$POPrt18_61, weightState, nsim=999)
POP18_61MC
plot(POP18_61MC, main="", las=1)

#----->ANS
moran.plot(statePop18_61$POPrt18_61, weightState, zero.policy=TRUE, plot=TRUE)


#!# Task 6: Repeat task 5 using IDW weight method ------------------------------






