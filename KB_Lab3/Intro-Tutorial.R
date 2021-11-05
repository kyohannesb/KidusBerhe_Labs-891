# NAME = Kidus Y Berhe
# Class = GEOG 491/891
# Lab 3 - Spatial autocorrelation, globally and locally

library(spdep)
library(sf)
library(tidyverse)
library(tmap)

#Subsetting nebraska: ----------------------------------------------------------

allStates <- sf::read_sf("./data/County_2010Census_DP1.shp") #call in state census data
glimpse (allStates) #quick view of data

onlyNE <- allStates %>% dplyr::filter(stringr::str_starts(GEOID10, "31")) #selecting only Nebraska using FIP code in "GEOID10)
tmap::tm_shape(onlyNE) + tm_polygons() #quick plot to see data
sf::st_crs(onlyNE) #check crs

ne.projected <- onlyNE %>% sf::st_transform(., "ESRI:102010") #reproject as American Equidistant
tmap::tm_shape(ne.projected) + tm_polygons() #show new projected map

#Making neighborhood and testing it: -------------------------------------------

nb <- spdep::poly2nb(ne.projected, queen = TRUE) #make the neighbourhood
nb[[1]] #check "id" of "1's" neighbours
ne.projected$NAMELSAD10[1] # index 1 county name
nb[[1]] %>% ne.projected$NAMELSAD10[.] #names of the surrounding counties
# note the outcome is in the same order as the index output

lw <- nb2listw(nb, style="W", zero.policy=TRUE)
lw$weights[1]

neighbors <- attr(lw$weights,"comp")$d# use attr to get the count of neighbors in W
hist(neighbors) #histogram plot of neightbours

#Computing spatail lag and creating moran's I for data: ------------------------

F65.lag <- lag.listw(lw, ne.projected$DP0070003) #computing neighbour pop of females>65 for each polygon
#uses the created weights and chosen column from NE
F65.lag #shows the spatially lagged values

moran.test(ne.projected$DP0070003, lw) #this function creates moran's I value

MC<- moran.mc(ne.projected$DP0070003, lw, nsim=999) #tesint significance with MC instead (more accurate p-value)
MC #note different p-value 
plot(MC, main="", las=1) # Plot the distribution (note that this is a density plot instead of a histogram)

# Defining W using a distance band: --------------------------------------------
#computes neighbours based on distance to polygon center (centroid), not the polygon as a whole

coords <- ne.projected %>% as_Spatial() %>% coordinates()  #convert to spatail object then coordinates() functions pulls centroids
coords #the coordinates of all the centroids

s.dist <- dnearneigh(coords, 0, 50000) #define search distance, for this 50km (note coed is in meters)
#to create annulus (doughnut) add a distance for initial input. this will exclude everything but objects in that annulus

lw <- nb2listw(s.dist, style="W",zero.policy=T) #identify all neighbouring polygons for each polygon
MI <- moran.mc(ne.projected$DP0070003, lw, nsim=999, zero.policy=T) #MC simulation for accurate p-value
MI
plot(MI, main="", las=1)


#Moran’s plots: ----------------------------------------------------------------
#this will produce local indicators of autocorrolation, not global. (LISA's)

# use zero.policy = T because some polygons don't have neighbors
moran.plot(ne.projected$DP0070003, lw, zero.policy=TRUE, plot=TRUE)
#note that this is a function that creates the plot, this can be done manually as well




