library(dplyr)
library(raster) # sadly conflicts with dplyr 
library(rgeos)
library(geosphere)
library(rgdal)

###############################################################################################################
#----------- Set Working Directory ----------
setwd("...")

#----------- Read in data -----------
data <- read.csv("inputs/data_for_map.csv")
point_data <- read.csv("inputs/point_data.csv")

# I have split these up so that you create a shapefile and a regular dataframe 
# regular dataframe that we will add info from the shapefile too
data_analysis <- data

# a Large SpatialPointsDataFrame that contains our data
data_shapefile <- data

#------- Read in shapefile ---------
# I got these from London Data Store: https://data.london.gov.uk/dataset/london-area-classification
# Borough is the largest polygon shape for london and the most recognisable e.g. Highbury & Islington
Borough <- shapefile("London_Borough_Excluding_MHW.shp") 

# Wards are zones within boroughs
Ward <- shapefile("London_Ward_CityMerged.shp") 

# MSOAs are zones within wards
MSOA <- shapefile("MSOA_2011_London_gen_MHW.shp") 

# LSOAs (Lower Super Output Areas) - a very nice and small granularity :)
LSOA <- shapefile("LSOA_2011_London_gen_MHW.shp") 

#------- Read in point data shapefile
CAZ <- shapefile("lp-consultation-oct-2009-central-activities-zone.shp")

###############################################################################################################
# ------- map projections for polygons ( & points ) ---------
# Set the projection of the shapefile
Borough <- spTransform(Borough, CRS("+proj=longlat +datum=WGS84 +no_defs"))
Ward <- spTransform(Ward, CRS("+proj=longlat +datum=WGS84 +no_defs"))
MSOA <- spTransform(MSOA, CRS("+proj=longlat +datum=WGS84 +no_defs"))
LSOA <- spTransform(LSOA, CRS("+proj=longlat +datum=WGS84 +no_defs"))

# Show where the lat long coordinates are in the dataframe to be turned into a shapefile
coordinates(data_shapefile) <- ~lon+lat

# Set the projection of the dataframe turned shapefile
proj4string(data_shapefile) <- CRS("+proj=longlat +datum=WGS84 +no_defs")

# Check the projections are the same and that they will match together, should be 'TRUE'
identical(proj4string(data_shapefile), proj4string(Borough)) 
identical(proj4string(data_shapefile), proj4string(Ward)) 
identical(proj4string(data_shapefile), proj4string(MSOA)) 
identical(proj4string(data_shapefile), proj4string(LSOA)) 

# Transform polygon shape to the same projection as the data I am using
CAZ <- spTransform(CAZ, CRS("+proj=longlat +datum=WGS84 +no_defs"))

###############################################################################################################
# ------- finding distance between points ------
# I want to find the average distance between the 'centroid' or 'middle' of my polygons and the centre of London
# to do this I am firstly finding the centroid of my polygons
# then I am finding the centroid of my "CAZ" polygon. 
# CAZ is London's Central Activities Zone, which I am assuming at the centre of this point is my 'middle' of London
# Finally, I am finding the distance between my zone polygon centroids and my CAZ polygon centroid
# This is a proxy for the distance to the city centre

# ------- Centroids ---------
# Find the centroid of 'middle' of the polygon shape
centroid_CAZ <- gCentroid(CAZ, byid = TRUE)

# Find distance between the centre of the polygon (Ward in this case) and the CAZ
Ward$Centroid <- gCentroid(Ward, byid = TRUE)
Ward@data$Distance_from_CAZ <- distm(Ward$Centroid@coords, centroid_CAZ@coords, fun = distHaversine)
Ward@data$Centroid <- NULL



###############################################################################################################
# ---------- Map back to original dataframe --------

# Create new dataframe that identifies the correct polygon for each row in the dataframe
points_with_zones_Borough <- over(data_shapefile, Borough)
points_with_zones_Ward <- over(data_shapefile, Ward)
points_with_zones_MSOA <- over(data_shapefile, MSOA)
points_with_zones_LSOA <- over(data_shapefile, LSOA)

# Assign the name of the Borough/Ward/LSOA to the main dataframe. Check the 'points_with_zones' dataframe to make sure you are pulling across the correct field, as the name will depend on the shapefile you are using
data_analysis$Borough_Name <- points_with_zones_Borough$NAME
data_analysis$Ward_Name <- points_with_zones_Ward$NAME
data_analysis$MSOA_Name <- points_with_zones_MSOA$MSOA11NM
data_analysis$LSOA_Name <- points_with_zones_LSOA$LSOA11NM

# Assign created variable to my data_analysis dataframe (Kilometers)
data_analysis$Distance_to_CAZ <- points_with_zones_Ward$Distance_from_CAZ /1000

#write.csv(data_analysis, "data_for_map_prepped.csv", row.names = FALSE)

###############################################################################################################
# ---------- Calculate distance between points from dataframe ---------
centroid_CAZ_lon <- centroid_CAZ@coords[,1]
centroid_CAZ_lat <- centroid_CAZ@coords[,2]

# There's probably a million packages for this but this works fine too!!!
# Calculate distance in kilometers between two points
earth.dist <- function (long1, lat1, long2, lat2)
{
  rad <- pi/180
  a1 <- lat1 * rad
  a2 <- long1 * rad
  b1 <- lat2 * rad
  b2 <- long2 * rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R <- 6378.145
  d <- R * c
  return(d)
}

point_data$Distance_to_CAZ <- earth.dist(point_data$lon, point_data$lat, centroid_CAZ_lon, centroid_CAZ_lat)


#write.csv(point_data, "point_data_prepped.csv", row.names = FALSE)











