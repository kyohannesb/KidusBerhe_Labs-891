#NAME = KIDUS BERHE  ----------- FINAL PROJECT CODE
library(tidyverse)
library(dplyr)
library(GISTools)
library(sf)
library(tmap)
library(rgdal)
library(raster)
library(spdep)
library(spData)
library(stringr)
library(leaflet)
library(RColorBrewer)


#reading in data: --------------------------------------------------------------

##administrative boundaries:
#National - admin 0
ethNatBnd <- sf::read_sf("./data/admin/eth_admbnda_adm0_csa_bofed_20201008.shp")
ethNatBnd <- ethNatBnd %>% sf::st_make_valid()
ethNatBnd %>% sf::st_is_valid()
ethNatBnd <- ethNatBnd %>% sf::st_transform(., "ESRI:102023")
#Regional states - admin 1
ethStatBnd <- sf::read_sf("./data/admin/eth_admbnda_adm1_csa_bofed_20201008.shp")
ethStatBnd <- ethStatBnd %>% sf::st_make_valid()
ethStatBnd %>% sf::st_is_valid()
ethStatBnd <- ethStatBnd %>% sf::st_transform(., "ESRI:102023")
#Zones - admin 2
ethZnBnd <- sf::read_sf("./data/admin/eth_admbnda_adm2_csa_bofed_20201008.shp")
ethZnBnd <- ethZnBnd %>% sf::st_make_valid()
ethZnBnd %>% sf::st_is_valid()
ethZnBnd <- ethZnBnd %>% sf::st_transform(., "ESRI:102023")
#Woredas/Districts - admin 3
ethDistBnd <- sf::read_sf("./data/admin/eth_admbnda_adm3_csa_bofed_20201027.shp")
ethDistBnd <- ethDistBnd %>% sf::st_make_valid()
ethDistBnd %>% sf::st_is_valid()
ethDistBnd <- ethDistBnd %>% sf::st_transform(., "ESRI:102023")

##Health Sites and facilities:
#health sites - smaller e.g: pharmacies
ethHlthSite <- sf::read_sf("./data/healthsites.shp")
ethHlthSite <- ethHlthSite %>% sf::st_make_valid()
ethHlthSite %>% sf::st_is_valid()
ethHlthSite <- ethHlthSite %>% sf::st_transform(., "ESRI:102023")
#health facilities - larger e.g: hospitals and clinics
ethHlthFaci <- sf::read_sf("./data/ETH_HealthFacility.shp")
ethHlthFaci <- ethHlthFaci %>% sf::st_make_valid()
ethHlthFaci %>% sf::st_is_valid()
ethHlthFaci <- ethHlthFaci %>% sf::st_transform(., "ESRI:102023")
#health facilities - Joined version of the above 2
ethHlth <- sf::read_sf("./data/ethHealthcenters.shp")
ethHlth <- ethHlth %>% sf::st_make_valid()
ethHlth %>% sf::st_is_valid()
ethHlth <- ethHlth %>% sf::st_transform(., "ESRI:102023")

## Capitals Towns and populations
#capital towns
ethCapTwns <- sf::read_sf("./data/ETH_CapitalTowns.shp")
ethCapTwns <- ethCapTwns %>% sf::st_make_valid()
ethCapTwns %>% sf::st_is_valid()
ethCapTwns <- ethCapTwns %>% sf::st_transform(., "ESRI:102023")

#woredas/district population estimates 2020
ethPop <- read_csv("./data/eth_admin3_pop_2020.csv")
ethPopNat <- read_csv("./data/eth_admin1_pop_2020.csv")
ethPopAD2 <- read_csv("./data/eth_admin2_pop_2020.csv")




# Joining and calculating required population data (DISTRICTS): -------------------------------------------------------
#adding population data to district shapefile
ethPopAll <- left_join(ethPop, ethPopNat, by = "ADM0_PCODE")
ethPopAll

ethDistPop <- left_join(ethDistBnd, ethPopAll, by = "ADM3_PCODE")
ethDistPop$Total.y #ensuring total national pop shows up on all cells

#claculating percent of total population for each district:

class(ethDistPop$Total.x)
class(ethDistPop$Total.y)

ethDistPop <- ethDistPop %>% mutate(percTot = ( as.numeric(Total.x) / Total.y )*100)
ethDistPop$percTot
class(ethDistPop$percTot)

tm_shape(ethDistPop) + tm_polygons(col = "percTot", n = 6, style = "jenks") #quick check to see plot


#calculating population density using polygon area
crs(ethDistPop) #check distance units
ethDistPop$CalAreasKmsq <- st_area(ethDistPop) / 1000000
ethDistPop$CalAreasKmsq
ethDistPop

#checking polygon area from given units to see if there is similarities
ethDistPop %>% dplyr::select(ADM3_EN.x, Shape_Area, CalAreasKmsq) #given values do not look correct when compared to online sources


ethDistPop <- ethDistPop %>% mutate(PopDens = (as.numeric(Total.x) / CalAreasKmsq )) #note units are in Km squared
ethDistPop$PopDens

tm_shape(ethDistPop) + tm_polygons(col = "PopDens", title = "Pop density. People/km^2", n = 6, style = "quantile" )

# Joining and calculating required population data (ZONES): -------------------------------------------------------
#adding population data to district shapefile
ethPopAll2 <- left_join(ethPopAD2, ethPopNat, by = "ADM0_PCODE")
ethPopAll2

ethZonePop <- left_join(ethZnBnd, ethPopAll2, by = "ADM2_PCODE")
ethZonePop$Total.y #ensuring total national pop shows up on all cells

#claculating percent of total population for each district:

class(ethZonePop$Total.x)
class(ethZonePop$Total.y)

ethZonePop <- ethZonePop %>% mutate(percTot = ( Total.x / Total.y )*100)
ethZonePop$percTot
class(ethZonePop$percTot)

#tm_shape(ethZnBnd) + tm_polygons() #checking zones
tm_shape(ethZonePop) + tm_polygons(col = "percTot") #quick check to see plot


#calculating polygon area
crs(ethZonePop) #check distance units
ethZonePop$CalAreasKmsq <- st_area(ethZonePop) / 1000000
ethZonePop$CalAreasKmsq
ethZonePop

#checking polygon area from given units to see if there is similarities
x<-ethZonePop %>% dplyr::select(ADM2_EN.x, Shape_Area, CalAreasKmsq) #given values (from column) do not look correct when compared to online sources
x

x<- ethZonePop %>% group_by(ADM1_EN.x) %>% summarise(sum(CalAreasKmsq))
x #checking to compare to online sources. Calculated option produced more accurate figures

#population density calculation
ethZonePop <- ethZonePop %>% mutate(PopDens = (Total.x / CalAreasKmsq )) #note units are in Km squared
max(ethZonePop$PopDens)

tm_shape(ethZonePop) + tm_polygons(col = "PopDens", title = "Pop density. People/km^2", n = 8, style = "quantile" ) #quick check to see plot with capital city

x <- ethZonePop %>% dplyr::select(ADM2_PCODE, ADM2_EN.x, PopDens) %>%
                               filter( ADM2_PCODE != 'ET1401') #filter out Addis due to excessively large value
tm_shape(x) + tm_polygons(col = "PopDens", title = "Pop density. People/km^2", n = 6, style = "quantile" ) #using quantile

#tm_shape(x) + tm_polygons(col = "PopDens", title = "Pop density. People/km^2", n = 6, style = "jenks" ) #using jenks to test


# Joining, buffer and intersecting healthsites with cities: --------------------
#spatially joining health site and facility data
#ethHealth <- full_join(ethHlthFaci, ethHlthSite, copy = TRUE)
#tm_shape(ethHlthFaci) + tm_dots () #checking to see if its is reprensented in full data
#tm_shape(ethHlthSite) + tm_dots () #checking to see if its is reprensented in full data
tm_shape(ethHlth) + tm_dots ()

#creating buffers at different distances to show scale of coverage (small [cities] and large [rural])
ethHlthBuff5 <- st_buffer(ethHlth, dist = 5000)
ethHlthBuff10 <- st_buffer(ethHlth, dist = 10000)
ethHlthBuff15 <- st_buffer(ethHlth, dist = 15000)
ethHlthBuff20 <- st_buffer(ethHlth, dist = 20000)

tm_shape(ethHlthBuff5) + tm_polygons()
tm_shape(ethHlthBuff10) + tm_polygons()
tm_shape(ethHlthBuff15) + tm_polygons()
tm_shape(ethHlthBuff20) + tm_polygons(col = "blue", alpha = 0.1)

#finding areas not within the buffer
ethCapTwns$HtlhAcc <- ifelse(st_contains(ethHlthBuff20, ethCapTwns, sparse = FALSE),
                             "Yes", "No")

#tm_shape(ethCapTwns) + tm_dots(col = "HtlhAcc", size = 0.1)
#ethCapTwns$HtlhAcc
#!!! attempted to show cities outside the healthcare buffer but was unable. 


z <- count(ethHlth)
z #total health sites
xz <- count(ethDistPop)
xz #total districts
yz <- z/xz
yz #this figure is the base quantity of health sites to districts


#plotting health sites and capitals separately
#Health site and pop density
tm_shape(x) + 
  tm_polygons(col = "PopDens", title = "Pop density. People/km^2", n = 6, style = "quantile" ) +
  tm_add_legend('fill', col = "blue", 
                labels = "20Km Buffer Healthsite") +
  tm_shape(ethHlthBuff20) + tm_polygons(col = "blue", alpha = 0.1)
#Capitals and pop density
tm_shape(x) + 
  tm_polygons(col = "PopDens", title = "Pop density. People/km^2", n = 6, style = "quantile" ) +
  tm_shape(ethCapTwns) + tm_dots(col = "black", size = 0.1) + 
  tm_add_legend('symbol', col = "black", 
                labels = "District Capitals") 

#plotting health sites with cities and boundaries- final map
tm_shape(x) + 
  tm_polygons(col = "PopDens", title = "Pop density. People/km^2", n = 6, style = "quantile" ) +
  tm_shape(ethCapTwns) + tm_dots(col = "black", size = 0.1) + 
  tm_add_legend('symbol', col = "black", 
                labels = "District Capitals") +
  tm_shape(ethHlthBuff20) + tm_polygons(col = "blue", alpha = 0.1) +
  tm_add_legend('fill', col = "blue", 
                labels = "20Km Buffer Healthsite") 

#plotting health sites with cities and boundaries and percent of pop- final map
tm_shape(ethZonePop) + 
  tm_polygons(col = "PopDens", title = "Pop density. People/km^2", n = 8, style = "jenks" ) +
  #tm_shape(ethCapTwns) + tm_dots(col = "black", size = 0.2) + 
  #tm_add_legend('symbol', col = "black", 
                #labels = "District Capitals") +
  tm_shape(ethHlthBuff20) + tm_polygons(col = "blue", alpha = 0.1) +
  tm_add_legend('fill', col = "blue", 
                labels = "20Km Buffer Healthsite") 


#plotting cities and boundaries and percent of pop- final map
tm_shape(ethZonePop) + 
  tm_polygons(col = "percTot", title = "Percent of total Pop", n = 6, style = "jenks" ) +
  tm_shape(ethCapTwns) + tm_dots(col = "black", size = 0.2) + 
  tm_add_legend('symbol', col = "black", 
                labels = "District Capitals") +
  tm_shape(ethHlthBuff20) + tm_polygons(col = "blue", alpha = 0.1) +
  tm_add_legend('fill', col = "blue", 
                labels = "20Km Buffer Healthsite") 






#Spatial weights matrix, moransI and LISA (DISTRICTS): -------------------------

is.na(ethDistPop$percTot) #checking over all na presence
which(is.na(ethDistPop$percTot)) #finding specific values with na

#dropping the na value to let the moran.mc work
ethDistPop1 <- ethDistPop %>% filter(percTot != is.na(percTot) )
ethDistPop1
is.na(ethDistPop1$percTot)
which(is.na(ethDistPop1$percTot))


#creating neighborhood for Districts. 
ngb <- spdep::poly2nb(ethDistPop1, queen = TRUE)
#assigning weights to the neighbours
lw <- nb2listw(ngb, style = "W", zero.policy = TRUE) #assigning weights using "W" style. 
lw$weights[1] #row-standardizing the weights
neighbors <- attr(lw$weights,"comp")$d #pulling count of neighbours to show distribution
hist(neighbors, main = "Histogram of Districts Neighbours") #quick plot to see neighbours.
#!!! note the large quantity of first class, likely due to the borders having many areas where districts only had few direct neighbours.

ethPopPerc.lag <- lag.listw(lw, ethDistPop1$percTot)
ethPopPerc.lag
class(ethDistPop1$percTot)
mcPopPerc.Test <- moran.test(ethDistPop1$percTot, lw)
mcPopPerc.Test #note the strangly small p-value. As such i shall use the moran.mc function to analytically cpmute p-value
mcPopPerc <- moran.mc(ethDistPop1$percTot, lw, nsim = 999)
mcPopPerc #!!! note more acceptable p-value

plot(mcPopPerc, main = "Morans'l - Percent of total pop", las = 1)
moran.plot(ethDistPop1$percTot, lw, zero.policy=TRUE, plot=TRUE)


vect1 <- as.vector(scale(ethDistPop1$percTot))
labs1 <- as.character(ethDistPop1$ADM3_EN.x)

moranEthPopPer <- moran.plot(vect1, lw, labels = labs1) # plot showing morans I with district names
#moranEthPopPer #table showing completed values

lisaEthPopPer <- moranEthPopPer %>% 
  mutate(., LISA = ifelse((x>=0 & wx >= 0) , "HH",
                          ifelse((x < 0 & wx>= 0) , "LH",
                                 ifelse((x >=0 & wx < 0) , "HL",
                                        ifelse((x< 0 & wx < 0) ,"LL", NA)))))
lisaEthPopPer
finalLISA <- merge(ethDistPop1, lisaEthPopPer, by.x= "ADM3_EN.x", by.y = "labels")
finalLISA
pal <- brewer.pal(4, "RdBu")
pal
#map of LISA for the population % of total pop to show the clustering of high population  and low population areas
tm_shape(finalLISA) +
  tm_polygons(col = "LISA",
              palette = pal,
              border.col = "black",
              style = "fixed",
              legend.hist = TRUE) +
  tm_layout(main.title = "LISA map of Ethiopia's District Population % of total", legend.outside = TRUE)





#Maps for the paper:------------------------------------------------------------
#Map of ethiopia states ethnic divisions
tm_shape(ethStatBnd) +  
  tm_polygons(col = "ADM1_EN", palette = "Set1",
              border.col = "black") + 
  tm_scale_bar() + 
  tm_layout(main.title = "Ethiopia State divisions")
#Map of ethiopia zones
tm_shape(ethZnBnd) + 
  tm_polygons(col = "lightblue", border.col = "black") + 
  tm_shape(ethCapTwns) + 
  tm_dots(col = "red", size = 0.06) +
  tm_add_legend('symbol', col = "red", 
                labels = "District Capitals")+
  tm_scale_bar() +
  tm_layout(main.title = "Ethiopia Zones with District Captials")
#Map of ethiopia districts
tm_shape(ethDistPop1) + 
  tm_polygons(col = "lightblue", border.col = "black", alpha = 0.5) + 
  tm_shape(ethCapTwns) + 
  tm_dots(col = "red", size = 0.06) +
  tm_add_legend('symbol', col = "red", 
                labels = "District Capitals")+
  tm_scale_bar() +
  tm_layout(main.title = "Ethiopia District with Captials")

#Map of pop Density zones
tm_shape(x) + 
  tm_polygons(col = "PopDens", palette = "PuBuGn", title = "Pop density. People/km^2"
              , n = 6, style = "quantile",
              border.col = "black",
              legend.hist = TRUE) + 
  tm_scale_bar()+
  tm_layout(main.title = "Ethiopia Zones population density", legend.outside = TRUE)

#Map of pop Density districts
tm_shape(ethDistPop1) + 
  tm_polygons(col = "PopDens", palette = "PuBuGn", title = "Pop density. People/km^2"
              , n = 6, style = "quantile",
              border.col = "black",
              legend.hist = TRUE) + 
  tm_scale_bar()+
  tm_layout(main.title = "Ethiopia Districts population density", legend.outside = TRUE)

#Map of pop % of total Zones
tm_shape(ethZonePop) + 
  tm_polygons(col = "percTot", palette = "PuBuGn", title = "%pop of total Pop"
              , n = 6, style = "quantile",
              border.col = "black",
              legend.hist = TRUE) + 
  tm_scale_bar() +
  tm_layout(main.title = "Ethiopia Zones population % of Total", legend.outside = TRUE)

#Map of pop % of total Districts
tm_shape(ethDistPop1) + 
  tm_polygons(col = "percTot", palette = "PuBuGn", title = "%pop of total Pop"
              , n = 6, style = "quantile",
              border.col = "black",
              legend.hist = TRUE) + 
  tm_scale_bar() +
  tm_layout(main.title = "Ethiopia Districts population % of Total", legend.outside = TRUE)

#Map of LISA of District pop% of total
tm_shape(finalLISA) +
  tm_polygons(col = "LISA",
              palette = pal,
              border.col = "black",
              style = "fixed",
              legend.hist = TRUE) +
  tm_scale_bar() +
  tm_layout(main.title = "LISA map of Ethiopia's District Population % of total", legend.outside = TRUE)

#map of health centers and districts
tm_shape(finalLISA) + 
  tm_polygons(col = "percTot", title = "Percent of total Pop", n = 6, style = "jenks",
              legend.hist = TRUE) +
  tm_shape(ethCapTwns) + tm_dots(col = "black", size = 0.2) + 
  tm_add_legend('symbol', col = "black", 
                labels = "District Capitals") +
  tm_shape(ethHlthBuff20) + tm_polygons(col = "blue", alpha = 0.1) +
  tm_add_legend('fill', col = "blue", alpha = 0.1,
                labels = "20Km Buffer Healthsite") +
  tm_scale_bar() +
  tm_layout(main.title = "Eth District-% of totalPop with capitals and Healthsite access", legend.outside = TRUE)

#map of health centers and cities
tm_shape(ethZonePop) + 
  tm_polygons(col = "percTot", title = "Percent of total Pop", n = 6, style = "jenks",
              legend.hist = TRUE) +
  tm_shape(ethCapTwns) + tm_dots(col = "black", size = 0.2) + 
  tm_add_legend('symbol', col = "black", 
                labels = "District Capitals") +
  tm_shape(ethHlthBuff20) + tm_polygons(col = "blue", alpha = 0.1) +
  tm_scale_bar() +
  tm_add_legend('fill', col = "blue", alpha = 0.1, 
                labels = "20Km Buffer Healthsite") +
  tm_layout(main.title = "Eth Zone-% of totalPop with capitals and Healthsite access", legend.outside = TRUE)
