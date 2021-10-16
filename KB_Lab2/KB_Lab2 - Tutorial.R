library(tidyverse)
library(sf)
library(tmap)

# Joins :-----------------------------------------------------------------------
#make tabel names
t.names <- tibble(key = c(1, 2, 3),
                  name = c("Huey", "Dewey", "Louis"))
#make table scores
t.scores <- tibble(name = c("Louis", "Huey", "Dewey"),
                   grade = c(99, 45, 33))
#join created tables
t.joined <- left_join(t.names, t.scores, by = "name")
t.joined

#Left join - make tabels
t.wonkyNames <- tibble(nombre = c("Dewey", "Louis", "Huey"),
                       x = rep(999),
                       favoriteFood = c("banana", "apple", "carrot"))
#join to existsing using left join with different syntax for "by" due to no matching names.
#code tells R that "nombre" can be used as "Name" as the column names are different
t.joined2 <- left_join(t.names, t.wonkyNames, by = c("name" = "nombre"))
t.joined2

# Working with Tabular data :---------------------------------------------------
#working with data - huge amount of observations, few variables.
bmps <- read_csv("./Data/BMPreport2016_landbmps.csv")
glimpse(bmps)

# Dr.PB : edit the bmps variable in place, which isn't always best practices
#removing the unwanted information found within variables, in the case the Geographyname/FIPS code and creating new column containing numbers only
#!!does not delete, column is added to end of the table
bmps <- bmps %>% mutate(., FIPS.trimmed = stringr::str_sub(GeographyName, 1, 5))

# using summarise and other stats to calculate and visualise the data
#finding total cost by BMP
#then plotting result
bmps %>% group_by(BMPType) %>% summarise(totalCost = sum(Cost)) %>%
  ggplot(., aes(x = BMPType, y = totalCost)) +
  geom_bar(stat = "identity") +
  theme_minimal()
#graph does not function properly as there is missing data in the information used
#!!!EDA is useful for this as it would show the missing data before any major work is done.

summary(bmps$Cost)
#produced a large amount of "NA's" which can be removed. 

#note the sum function code, ".rm" is used to delete objects from memory, in this case the "NA", it needs to be made true.
#rm can selectively remove elements/variables within an environment
#it is a parameter used by functions, not a function or an operation.
bmps %>% group_by(BMPType) %>% summarise(totalCost = sum(Cost, na.rm = T)) %>%
  ggplot(., aes(x = BMPType, y = totalCost)) +
  geom_bar(stat = "identity") +
  theme_minimal()
#graph now shows values properly as there is not missing data/NA's there to confuse or impede the code. 

#grouping by multiple varibles at the same time.
twofactors <- bmps %>% group_by(StateAbbreviation, Sector) %>% summarise(totalCost = sum(Cost))
#this groups by State and Sector while also calculating the sum cost, all in one line. 
glimpse(twofactors)
#produced table does not have all sum values due to missing data, however is grouped by state and type/sector.


# Making some plots :-----------------------------------------------------------

#this sends/pipes the bmps table to the inline created plot. 
#produces a simple plot showing states grouped and their associated "AmountCredited"
#however the plot is "skewed"/erroneous (note y axis quantity due to outliers)
bmps %>% ggplot(., aes(x = StateAbbreviation, y = AmountCredited)) + #determines the axis
  geom_boxplot(aes(fill = StateAbbreviation))#determines aesthetic (matching state)

#subset data to be visualised
bmps %>%
  dplyr::filter(., AmountCredited > 1 & AmountCredited < 100) %>% #this line limits the y-axis, placed before calling axis in
  ggplot(., aes(x = StateAbbreviation, y = AmountCredited)) + #determines the axis
  geom_boxplot(aes(fill = StateAbbreviation)) #determines aesthetic (matching state)

#plots can be given many dimensions using "facet" commands from ggplot.
#dimensions in this case are the types of sectors
bmps %>%
  dplyr::filter(., AmountCredited > 1 & AmountCredited < 100) %>%
  ggplot(., aes(x = StateAbbreviation, y = AmountCredited)) +
  geom_boxplot(aes(fill = StateAbbreviation)) +
  facet_grid(Sector~.) #determines which facet to use, notes these are elements that have been grouped together, not columns.

# Spatial intersection :--------------------------------------------------------

#using %in% to determine is and element is within another element. 

x <- c(1, 2, 3, 4, 5) #mock table
7 %in% x #is 7 in x (above), should be false
2 %in% x #is 2 in x, should be true
c(4, 99, 1) %in% x #is this vector in x, should be a mixed result, (T, F, T)
#this can be used to quickly check data for specific elements they wish to use, without having to visualise it all. 


# Using tmap on data and validating :-------------------------------------------

counties <- sf::read_sf("./Data/County_Boundaries.shp")#read in the file/data
counties %>% sf::st_is_valid() #check validity
counties <- counties %>% sf::st_make_valid() #validate the invalid data
#function tries to create a valid representation of an invalid geometry without losing any of the input elements. 
#Already-valid geometries are returned without further intervention
tm_shape(counties) + tm_polygons(col = "ALAND10") #simple map of counties, will not work without all valid geometry

