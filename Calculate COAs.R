#### CALCULATE COAs ####
# Previous Script: Detection Reassignment

# Now that we have our scattered detection points based on receiver detection probability (new.det.data.csv), we will import them into the {VTrack} package and use these locations to estimate COAs. 

library(dplyr)
library(lubridate)
library(sf)
library(tidyverse)
library(VTrack)


## IMPORT DATA ## 

detdata <- read.csv("new.det.data.csv", header = T) # import reassigned detection locations 
detdata$receiver_name <- sub("^", "VR2W-", detdata$receiver_sn) # add VR2W prefix to receiver name as per VTrack requirements
detdata$transmitter_id <- sub("^", "A69-1601-", detdata$transmitter_id) # add prefix to transmitter_id as per VTrack requirements
detdata$detection_timestamp <- as.POSIXct(detdata$detection_timestamp, format = "%m/%d/%Y %H:%M:%S", tz = "America/New_York")
#change column names as per VTRACK requirements 
detdata$latitude <- detdata$lat
detdata$longitude <- detdata$lon
detdata$station_name <- detdata$recstation
namevector <- c("sensor_value", "sensor_unit") # add columns required for VTrack
detdata[ , namevector] <- NA
detdata <- st_as_sf(detdata, coords = c("longitude", "latitude"), crs = 4326) # add coordinate system 
detdata$longitude<-st_coordinates(detdata)[,1] # get coordinates
detdata$latitude<-st_coordinates(detdata)[,2] # get coordinates
detdata <- detdata %>% # select only columns that we need 
  dplyr::select(transmitter_id, receiver_name, detection_timestamp, station_name, sensor_value, sensor_unit, longitude, latitude)

Data <- VTrack::setupData(Tag.Detections = detdata, 
                          Tag.Metadata = tag_metadata, 
                          Station.Information = recinfo, 
                          source = "IMOS", 
                          crs = sp::CRS("+init=epsg:4326"))


## CALCULATE COAs ## 

COAs <- COA(Data, 
            timestep = 30, 
            split = TRUE)

# write files for each individuals' COAs to have on hand 
write.csv(COAs[["64475"]], "COA64475_new.csv")
write.csv(COAs[["64477"]], "COA64477_new.csv")
write.csv(COAs[["64479"]], "COA64479_new.csv")
write.csv(COAs[["64480"]], "COA64480_new.csv")
write.csv(COAs[["64481"]], "COA64481_new.csv")
write.csv(COAs[["64482"]], "COA64482_new.csv")
write.csv(COAs[["64483"]], "COA64483_new.csv")
write.csv(COAs[["64484"]], "COA64484_new.csv")

# make a dataframe for each turtle 
id_64475 <- COAs[["64475"]]
id_64477 <- COAs[["64477"]]
id_64479 <- COAs[["64479"]]
id_64480 <- COAs[["64480"]]
id_64481 <- COAs[["64481"]]
id_64482 <- COAs[["64482"]]
id_64483 <- COAs[["64483"]]
id_64484 <- COAs[["64484"]]

# combine into one dataframe
COAall <- rbind(id_64475, id_64477, id_64479, id_64480, id_64481, id_64482, id_64483, id_64484)
write.csv(COAall, "COAall.csv")

# check that COAs don't fall over land 
COAall <- sf::st_as_sf(COAall, coords=c("Longitude.coa","Latitude.coa"), crs = 4326)
COAall$Longitude.coa <- st_coordinates(COAall)[,1] # get coordinates
COAall$Latitude.coa <- st_coordinates(COAall)[,2] # get coordinates
COAall <- st_transform(COAall, 3395)
Study_Area <- st_read(dsn = "bimini shapefile/bimini_shape.shp", quiet = TRUE) # shapefile of study area
Study_Area <- st_transform(Study_Area, 3395) # transform to common CRS
land <- lengths(st_intersects(COAall, Study_Area)) > 0  
COA.land <- COAall[land,] # no COAs over land 


## CALCULATE COAs FOR SUBSET DATA ##

detdata_sub <- read.csv("new.det.data_sub.csv", header = T)
detdata_sub$receiver_name <- sub("^", "VR2W-", detdata_sub$receiver_sn)
detdata_sub$transmitter_id <- sub("^", "A69-1601-", detdata_sub$transmitter_id)
detdata_sub$detection_timestamp <- as.POSIXct(detdata_sub$detection_timestamp, format = "%m/%d/%Y %H:%M:%S", tz = "America/New_York")
detdata_sub$latitude <- detdata_sub$lat
detdata_sub$longitude <- detdata_sub$lon
detdata_sub$station_name <- detdata_sub$recstation
namevector <- c("sensor_value", "sensor_unit")
detdata_sub[ , namevector] <- NA
detdata_sub <- st_as_sf(detdata_sub, coords = c("longitude", "latitude"), crs = 4326) 
detdata_sub$longitude<-st_coordinates(detdata_sub)[,1] 
detdata_sub$latitude<-st_coordinates(detdata_sub)[,2] 
detdata_sub <- detdata_sub %>%
  dplyr::select(transmitter_id, receiver_name, detection_timestamp, station_name, sensor_value, sensor_unit, longitude, latitude)

Data <- VTrack::setupData(Tag.Detections = detdata_sub, 
                          Tag.Metadata = tag_metadata, 
                          Station.Information = recinfo, 
                          source = "IMOS", 
                          crs = sp::CRS("+init=epsg:4326"))

COAs_sub <- COA(Data, 
                timestep = 30, 
                split = TRUE)

id_64475_sub <- COAs_sub[["64475"]]
id_64477_sub <- COAs_sub[["64477"]]
id_64479_sub <- COAs_sub[["64479"]]

write.csv(id_64475_sub, "COA64475_new_sub.csv")
write.csv(id_64477_sub, "COA64477_new_sub.csv")
write.csv(id_64479_sub, "COA64479_new_sub.csv")

COA_sub <- rbind(id_64475, id_64477, id_64479)
write.csv(COA_sub, "COA_sub.csv")


## COA LOCATION ERRORS ##

# Prior to being run in the dBBMM, the acoustic COA data will need to have a column added that indicates the location error of the points. 
# For this error, we will use the 50% detection probability distance. This is 185 m in reef habitat and 350 m in bank habitat
# Take the COAall and COAall_sub dataframes into ArcGIS and draw polygons around the COAs associated with receivers in reef and bank habitat. 
# Use the intersect tool to get two dataframes that contain reef locations and bank locations and export. The files will be easier to manage and join together in Excel. 
# Note: the column names will be changed from having "_" to "."
# Save the new.csv files with the location errors and import into R

COAall_loc <- read.csv("COAall_loc.err.csv", header = T)
COAall_sub_loc <- read.csv("COAall_sub_loc.err.csv", header = T)

# Next Script: Satellite Filtering 
