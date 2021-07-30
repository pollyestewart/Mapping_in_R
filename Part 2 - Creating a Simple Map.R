library(dplyr)
library(raster) # sadly conflicts with dplyr 
detach("package:raster", unload = TRUE) # so I remove it 
library(rgeos)
library(geosphere)
library(leaflet)
library(zoo)

###############################################################################################################
#----------- Read in data from Part 1 R Script
# OR just read in the already prepped files :D
setwd("...")

# Read in data prepped previously
data <- read.csv("data_for_map_prepped.csv")
point_data <- read.csv("point_data_prepped.csv")


###############################################################################################################
#----------- Data prep
# Simple chloropleth map
data_Borough <- data %>%
  na.omit() %>%
  select(-Ward_Name, -MSOA_Name, -LSOA_Name, -lat, -lon) %>%
  group_by(date, time, Borough_Name) %>%
  summarise_all(funs(mean))

data_Ward <- data %>%
  na.omit() %>%
  select(-Borough_Name, -MSOA_Name, -LSOA_Name, -lat, -lon) %>%
  group_by(date, time, Ward_Name) %>%
  summarise_all(funs(mean))

data_MSOA <- data %>%
  na.omit() %>%
  select(-Borough_Name, -Ward_Name, -LSOA_Name, -lat, -lon) %>%
  group_by(date, time, MSOA_Name) %>%
  summarise_all(funs(mean))

data_LSOA <- data %>%
  na.omit() %>%
  select(-Borough_Name, -Ward_Name, -MSOA_Name, -lat, -lon) %>%
  group_by(date, time, LSOA_Name) %>%
  summarise_all(funs(mean))

library(raster)

# Read in Shapefile data 
# Borough is the largest polygon shape for london and the most recognisable e.g. Highbury & Islington
Borough <- shapefile("London_Borough_Excluding_MHW.shp") 
Borough <- spTransform(Borough, CRS("+proj=longlat +datum=WGS84 +no_defs"))

# Wards are zones within boroughs
Ward <- shapefile("London_Ward_CityMerged.shp") 
Ward <- spTransform(Ward, CRS("+proj=longlat +datum=WGS84 +no_defs"))

# MSOAs are zones within wards
MSOA <- shapefile("MSOA_2011_London_gen_MHW.shp") 
MSOA <- spTransform(MSOA, CRS("+proj=longlat +datum=WGS84 +no_defs"))

# LSOAs (Lower Super Output Areas) - a very nice and small granularity :)
LSOA <- shapefile("LSOA_2011_London_gen_MHW.shp") 
LSOA <- spTransform(LSOA, CRS("+proj=longlat +datum=WGS84 +no_defs"))

###############################################################################################################
# -------- Data prep specific using our data for the map

# A simple static map, so we need to summarise the data for each polygon
# filters to set
date_filter <- "09/06/2018"
time_filter <- "Midday"

# Select granularity of map
ForMap <- data_Borough

# Map prep
ForMap[is.na(ForMap)] <- 0

# Filter polygon data for map
ForMap <- ForMap %>%
  ungroup() %>%
  filter(date == date_filter & time == time_filter) 

# Filter point data for map
ForMap_points <- point_data %>%
  ungroup() %>%
  filter(date == date_filter & time == time_filter) 

###############################################################################################################
# -------- Data prep for maps

# Merge dataframe with Shapefile using the name of the polygon you found previously
m <- merge(Borough, ForMap, by.x = 'NAME', by.y = "Borough_Name")

# Fill in blank polygons with the value before to it
m$data <- na.locf(m$data)

# Set palette for chloropleth map
bins <- c(130, 110, 100, 90, 70, 60, 50, 30, 20, 0)
pal <- colorBin("YlOrRd", domain = ForMap$data, bins = bins)


###############################################################################################################
#----- create a choropleth map with points!

# Map!
leaflet()  %>% 
  addTiles() %>% 
  addPolygons(data = m, 
              weight = 0.1,
              color = 'white',
              opacity = 0.5,
              fillOpacity = 0.7,
              fillColor = ~pal(data),
              highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
              popup = paste("MSOA Name: ", m$NAME, "<br>",
                            "data: ", m$data, "<br>")) %>%
  addMarkers(data = ForMap_points,
             lng=~lon,
             lat=~lat,
             popup = paste("Time: ", ForMap_points$time))




