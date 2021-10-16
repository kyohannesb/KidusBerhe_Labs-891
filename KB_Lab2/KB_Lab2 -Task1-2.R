
#   NAME = KIDUS Y BERHE
#   LAB 2 - GEOG 891 - R as a GIS(cience)


library(tidyverse)
library(sf)
library(tmap)
library(stringr)

####contents of comments:-------------------------------------------------------
#!# = Overall task
## = Subdivisions of Tasks
# = regular comment on work or line
#!!! = important / note this
#----->ANS = next line will contain answer to sub-task

#!# Reading in the required data:-----------------------------------------------

#Reading in Spatial data (and validating)
counties <- sf::read_sf("./Data/County_Boundaries.shp") %>% sf::st_make_valid()
dams <- sf::read_sf("./Data/Dam_or_Other_Blockage_Removed_2012_2017.shp") %>% sf::st_make_valid()
streams <- sf::read_sf("./Data/Streams_Opened_by_Dam_Removal_2012_2017.shp") %>% sf::st_make_valid()
#Reading in aspatial data
bmps <- read_csv("./Data/BMPreport2016_landbmps.csv")

#!# EDA: ---------------------------------------------------------------

 ##Glimpsing/quick viewing data and ensuring validity after making valid
glimpse(bmps) #does not need validation as it has no geometry to validate
glimpse(counties)
counties %>% sf::st_is_valid()
glimpse(dams)
dams %>% sf::st_is_valid()
glimpse(streams)
streams %>% sf::st_is_valid()

 ##quick summary check of wanted and missing data
summary(bmps$Cost) #NA's show up for cost
summary(bmps$TotalAmountCredited) #shows no missing values
summary(bmps$StateAbbreviation) #no missing values #!!!- returns type, not number summary
#contains large quantity of repeated values (can be grouped)
#!!! bmps%>%summary #this provides a total summary if needed (all in one)

summary(dams$YEAR) #some missing data, more 0's than years (median is 0.0)
summary(dams$STATE) #duplicated values that can be grouped. 
#!!! dams%>%summary #this provides a total summary if needed

summary(streams$LengthKM)
tm_shape(streams) + tm_lines()



#!# Task 1: Aspatial operations ------------------------------------------------

 ##Task 1.1 : ---> Summary stats for Cost for each state (BMPs)

#grouping the table by states and then calculating total cost in one line. 
stateCost_bmp_NA <- bmps %>% dplyr::select(StateAbbreviation, Cost) #selecting the data i want
#----->ANS
tapply(stateCost_bmp_NA$Cost, stateCost_bmp_NA$StateAbbreviation, summary) #making a stat summary using tapply()
#other methods returned too many values or did not separate by state visually in the output
#!!! The Na values are still present as that can be useful information as well

stateCost_bmp <- na.omit(stateCost_bmp_NA) #Removes NA values
tapply(stateCost_bmp$Cost, stateCost_bmp$StateAbbreviation, summary) #summary statistics for each state cost using tapply()
#!!! No NA values for a more "refined" summary


  ##TASK1.2 ; ---> Scatterplot of Cost vs TotalAmountCredited (in acres only)

onlyAcres <- subset(bmps, Unit == "Acres", na.rm = T) #selecting and assigning on the rows with "Acres" used as units
onlyAcres #showing variable in console to quick check units
#bmps #!!! remove initial hast-tag of this line to compare with original bmps table
onlyAcres <- na.omit(onlyAcres) #removed NA values
summary(onlyAcres$Cost) #quick summary

#----->ANS
onlyAcres %>%
  dplyr::filter(., Cost > 0 & Cost < 5000000) %>% #limiting the graph, omitting most extreme results
  #the limitations were made visually, using dplyr::filter
  dplyr::filter(., TotalAmountCredited > 0 & TotalAmountCredited < 30000)%>% #limiting the graph, omitting most extreme results
  ggplot(., aes(x = Cost, y = TotalAmountCredited)) +
  #geom_smooth()+ #!!!regression line, if required
  geom_point(size = 0.8)


  ##TASK1.3 : ---> Box-plot of State vs TotalAmountCredited using only cover cropping areas

#creating workable variable to preseve original data
onlyCovCrop <- bmps %>% mutate(., BMPbasicNM = stringr::str_sub(BMPShortName, 1, 9))%>%
  subset(., BMPbasicNM == "covercrop") #create new variable with only cover crop information
#done by subseting BMPshortname using character number (cross referenced with a table search, same rows returned).
#This was further subset to only include covercrop related informatiom 
#done this way so as to make fully sure "covercrop" was selected. 

onlyCovCrop %>%
  ggplot(., aes(x = StateAbbreviation, y = TotalAmountCredited)) +
  geom_boxplot(aes(fill = StateAbbreviation))
#plot with all values represented

#----->ANS
onlyCovCrop %>%
  dplyr::filter(., TotalAmountCredited > 0 & TotalAmountCredited < 6000) %>%
  ggplot(., aes(x = StateAbbreviation, y = TotalAmountCredited)) +
  geom_boxplot(aes(fill = StateAbbreviation))
#this plot has limited axis to sure the box plots a little better (could not remove too much before data became overly confusing). 
#result is still heavily skewed due to the "0's" in the data however as this is not NA it is being counted as data (by me)
#no instruction to remove like in next task

  ##TASK1.4 : ---> scatterplot of dam data, year on x axis and state on y axis

#removing year 0 values
damBuilt <- dams %>% subset(., YEAR != 0) #subset the dam data, selecting all that are not 0.
#!!! this also removed all the NA values for the "YEAR" column
damBuilt #shows all observations due to few quantity

#----->ANS
damBuilt %>%  ggplot(., aes(x = YEAR, y = STATE)) +
  geom_point(size = 5, shape = 10)
#only 2 states have had dams built from this data, showing in the plot and "damBuilt" variable 
#"shape" was chosen due to showing exact location on the graph while having a large visible size.

  
   ##TASK1.5 : ---> Aspatial visualisation of linked datasets - student choice
tibbleDams <- damBuilt%>% 
  as_tibble()%>%dplyr::select(-geometry) #make the table a tibble and remove geometry to reduce errors

dams12_17 <- tibbleDams%>%
  subset(., DamRemoval = 2012:2016)%>% mutate(ActiveYrs = DamRemoval - YEAR) #Subset to 2012-2016 while calculating number of years "active"
#only keeps data with "YEAR" because of calculation, NA and 0's removed

joined_DamStream <- left_join(dams12_17, streams, by = c("COMID" = "ComID")) 
"LengthKM" %in% colnames(joined_DamStream) #check to ensure wanted column is present, couldn't find it in opened table
#the tables had many similar columns however with many cases where one has no data while the other does for the same area

joined_DamStream <- joined_DamStream %>% mutate(DA_Sqkm = DA_SqMi.x * 2.58999) #calculating the square KM of the dams
joined_DamStream <- joined_DamStream %>% mutate(LengthM = LengthKM * 1000) #calculating the length in meters

"DA_Sqkm" %in% colnames(joined_DamStream) #checking to see if columns have been added
"LengthM" %in% colnames(joined_DamStream)

#----->ANS
joined_DamStream %>%  ggplot(., aes(x = DA_Sqkm, y = LengthM, size = ActiveYrs)) + #tried colour but could not figure out how to seperate by individual point
  geom_point(alpha = 1, shape = 19) +
  theme_gray(base_size = 15)+
  labs(x = "Dam Area (Km2)", y = "Length of Resulting Stream (m)", subtitle = "Area vs created stream length of Removed dams")
#graph is showing the Area (km2)and length of streams created by dam removal. 
#i wanted to see if the size of the dam held weight to the stream created. 
#I added the years active to show more info on the useage of each point/dam



#!# Task 2: Spatial operations -------------------------------------------------

    ##TASK2.1 : --->  5 longest streams

top5StrmTst <- streams %>% arrange(desc(LengthKM)) %>% #arrange the coloumn of length to largest on top
  dplyr::select(ComID, FCode, GNIS_Name, LengthKM) %>% #bring up only the columns that are wanted
  head(., 5) #output shows the top 5 lengths of streams and their associated names and ID codes
top5StrmTst #shows created table
top5StrmTst %>% sf::st_length(.,) #will show lengths in meters computed spatially using geometry
#!!! the legnthKm and the calculated length does not match up, the recorded size of the second stream maybe wrong
#!!! 2nd line, Length says 6.54km but geometry says 0.209 km (209m)

#----->ANS
top5Strm <- streams %>% arrange(desc(sf::st_length(.,))) %>% #spatially find legnths and arrange by descending
  dplyr::select(ComID, FCode, GNIS_Name, LengthKM) %>% #bring up only the columns that are wanted
  head(., 5) #only top 5 brought up
top5Strm #!!!this will show TRUE top 5
top5Strm %>% sf::st_length(.,)
#!!! second object from earlier table is replaced by longer stream (according to the geometry)

    
    ##TASK2.2 : --->  3 counties with total greatest stream leangth

#creating variable to hold grouped and calculated values.
TotStrmCounty <- streams %>% mutate(., GEOlength = sf::st_length(.,)) %>% #creates a column for the spatial length
  group_by(FCode) %>%  #grouping by "FCode" under assumption this is FIPS code (could not locate names)
  summarise(TOTlnthStrm = sum(GEOlength)) %>% #creating total lengths using groups made
  arrange(desc(TOTlnthStrm)) #sorting based on total stream length

#----->ANS
TotStrmCounty %>% dplyr::select(FCode, TOTlnthStrm)%>% #selecting what to show
  head(., 3) #showing only top three
#output shows the county FCode and the associated total stream length within them  
  


    ##TASK2.3 : ---> Join and plot counties' and BMPs total cost

FIPtemp <- bmps
FIPtemp <- FIPtemp %>% mutate(., FIPSCode = stringr::str_sub(GeographyName, 1, 5)) #shorten FIPS code
FIPtemp <- FIPtemp %>% mutate(., FIP = as.numeric(as.character(FIPtemp$FIPSCode))) #turns values in to numbers then adds them back to the list
summary(FIPtemp$FIP) #verify it is numeric

GEOID10temp <- counties
GEOID10temp <- GEOID10temp %>% mutate(., GEOID10_NUM = as.numeric(as.character(GEOID10temp$GEOID10))) #turns values in to numbers then adds them back to the list
summary(GEOID10temp$GEOID10_NUM)

joinCountBMP <- left_join(GEOID10temp, FIPtemp, by = c("GEOID10_NUM" = "FIP")) #joining the datasets

#!!! Below was my final attempt to group and map the data. I tried for a few hours and could not find a solution
#!!! the code seemed to crash rstudio on my laptop. Other attempts weren't any better. 
#!! As i had other home work i had no choice but to skip the plot, though the code is below
#!!! please let me know where my issue was

#grpCountjoin <- joinCountBMP %>%group_by(StateAbbreviation) %>% summarise(totalCost = sum(Cost))
#tm_shape(grpCountjoin) + tm_polygons(col = "totalCost")



#Uncompleted (time)------------------------ 
#At this point i had worked on this for 2 days and had other assignments due the same day, cutting it VERY close
#I will continue to work on this and try to update Github to see if it works

    ##TASK2.4 : ---> Closest stream segment

rmvdDams <- dams%>% subset(., DamRemoval = 2012:2016)


    ##TASK2.5 : ---> How many removed dams 


  







