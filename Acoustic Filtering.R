#### ACOUSTIC FILTERING ####

# Filter raw acoustic detection data, get summary stats, and create abacus plots to identify if any transmitters were shed prior to battery life ending 
# Data were filtered for false detections by Maurits P.M. van Zinnicq Bergmann prior to us accessing the data

library(dplyr)
library(lubridate)
library(tidyverse)
library(VTrack)
library(sf)


## IMPORT RAW ACOUSTIC DETECTION DATA AND CLEAN UP ##

detdata <- read.csv("turtle_detections_2.csv", header = T) 
summary(detdata)  # check out data and classes
detdata$detection_timestamp <- as.POSIXct(detdata$detection_timestamp_est, format = "%m/%e/%y %H:%M", tz = "America/New_York")  # convert datetime to POSIXct in America/New York time
detdata$receiver_sn <- factor(detdata$receiver_sn) # convert station column to categorical variable
detdata$transmitter_id <- factor(detdata$transmitter_id) # convert Transmitter.ID column to categorical 
detdata$detstation <- detdata$station # rename station so it is different than recdata station 
detdata <- detdata %>% # select just the columns we are interested in 
  dplyr::select(detection_timestamp, transmitter_id, receiver_sn, detstation)
str(detdata) # view the data class types for detdata
write.csv(detdata, "detdata.csv") # write a new csv file for detdata


## IMPORT ACOUSTIC RECEIVER DATA AND CLEAN UP ##

recdata <- read.csv("rec.move.coord.csv", header = T)
recdata$receiver_sn <- factor(recdata$receiver_sn) # convert station column to categorical variable
recdata$deploy_date_time <- as.POSIXct(recdata$deploy_date_time, format = "%m/%e/%y %H:%M", tz = "America/New_York") 
recdata$recover_date_time <- as.POSIXct(recdata$recover_date_time, format = "%m/%e/%y %H:%M", tz = "America/New_York") 
recdata <- st_as_sf(recdata, coords = c("deploy_long_WGS", "deploy_lat_WGS"), crs = 4326) # define coordinate system; creates a new Simple Features spatial object with a WGS84 geographic coordinate system assigned. "EPSG" is a code shorthand representing many geographic and projected coordinate systems.
recdata$long<-st_coordinates(recdata)[,1] # get coordinates
recdata$lat<-st_coordinates(recdata)[,2] # get coordinates
recdata$deploy_lat <- recdata$lat # rename columns
recdata$deploy_long <- recdata$long # rename columns
recdata$recstation <- recdata$station # rename station so different than detdata station (confirm that recstation and detstation are the same)
recdata <- recdata %>% # retain only columns that we are interested in 
  dplyr::select(receiver_sn, deploy_date_time, recover_date_time, deploy_lat, deploy_long, recstation, habitat)
write.csv(recdata, "recdata.csv") # create a csv of recdata


## COMBINE ACOUSTIC DETECTION AND RECEIVER DATA ##

# this matches up each detection with the lat/long of the receiver the turtle was detected on. Since some receivers moved locations over the course of the study, a conditional join based on the time window is performed #

# Perform conditional join based on time window (deploy_time to recover_time)
det <- left_join(recdata, detdata, by = "receiver_sn") %>% 
  filter(detection_timestamp >= deploy_date_time & detection_timestamp <= recover_date_time) %>% 
  dplyr::select(detection_timestamp, transmitter_id, receiver_sn, deploy_lat, deploy_long, recstation, detstation, habitat)
write.csv(det, "det.csv") # write a new csv for combined Detection and Receiver Data


## IMPORT TAG METADATA - needed for {VTrack Package} ##
tag_metadata <- read.csv("tag_metadata.csv", header = T)
tag_metadata$ReleaseDate <- as.POSIXct(paste(tag_metadata$Date, tag_metadata$Time), format = "%m/%e/%y %H:%M", tz = "America/New_York") # combine date and time, convert datetime to POSIXct in America/New York time 
tag_metadata <- st_as_sf(tag_metadata, coords = c("release_longitude", "release_latitude"), crs = 4326) 
tag_metadata$release_longitude <- st_coordinates(tag_metadata)[,1]
tag_metadata$release_latitude <- st_coordinates(tag_metadata)[,2] 


## RESTRICT ACOUSTIC DATASET TO TURTLES OF INTEREST ##

# study focuses on nine turtles that were tagged simulataneously with satellite and acoustic transmitters

Both <- c("64475", "64476", "64477", "64479", "64480","64481", "64482", "64483", "64484") # create vector containing the turtle IDs that are needed
det2 <- det %>% # filter Det data based on the IDs in above created vector
  filter(transmitter_id %in% Both) 


## FILTER OUT FIRST 24 HOURS PER INDIVIDUAL AS ACCLIMATION PERIOD ## 

det64475 <- det2 %>%
  filter(transmitter_id == "64475") %>%
  filter(detection_timestamp > "2017-05-03 14:16:00")

det64476 <- det %>%
  filter(transmitter_id == "64476") %>%
  filter(detection_timestamp > "2017-05-04 15:56:00")

det64477 <- det2 %>%
  filter(transmitter_id == "64477") %>%
  filter(detection_timestamp > "2017-05-03 14:13:00")

det64479 <- det2 %>%
  filter(transmitter_id == "64479") %>%
  filter(detection_timestamp > "2017-05-04 15:58:00")

det64480 <- det2 %>%
  filter(transmitter_id == "64480") %>%
  filter(detection_timestamp > "2017-05-04 16:05:00")

det64481 <- det2 %>%
  filter(transmitter_id == "64481") %>%
  filter(detection_timestamp > "2017-05-03 14:15:00")

det64482 <- det2 %>%
  filter(transmitter_id == "64482") %>%
  filter(detection_timestamp > "2017-05-03 14:14:00")

det64483 <- det2 %>%
  filter(transmitter_id == "64483") %>%
  filter(detection_timestamp > "2017-05-04 16:01:00")

det64484 <- det2 %>%
  filter(transmitter_id == "64484") %>%
  filter(detection_timestamp > "2017-05-04 15:55:00")

filtdet9 <- rbind(det64475, det64476, det64477, det64479, det64480, det64481, det64482, det64483, det64484)


## FILTER OUT DETECTIONS BETWEEN 05/04 AND 05/06 ## 

# maintenance resulted in 1/3 of receivers being offline, so all detections are being removed from this time period to reduce bias

filtdet9 <- filtdet9 %>% 
  filter(detection_timestamp <= "2017-05-04 08:21:00" | detection_timestamp >= "2017-05-06 17:37:00")
write_csv(filtdet9, "filtdet9.csv")


## SUMMARY STATS ##

# Number of individual detections, first detection, last detection 
IndDet <- filtdet9 %>%
  group_by(transmitter_id) %>%
  dplyr::summarize(NumDet = n(), 
                   FirstDet = min(detection_timestamp), 
                   LastDet = max(detection_timestamp))

# Detections per station 
Stations <- filtdet9 %>%
  group_by(detstation) %>%
  summarize(n = n())

# Detections per station per individual 
det64475 %>% # change out ID 
  group_by(detstation) %>%
  dplyr::summarize(StNum = n())


## PREP DATA FOR {VTrack} PACKAGE TO GET MORE SUMMARY STATS AND MAKE ABACUS PLOTS ##

# The {VTrack} package needs things set up in a particular way: 
# tag_metadata (taginfo) - tag information 
# detections9 (IMOSdata) - filtered detection data for the 9 turtles 
# recinfo (statinfo) - receiver and station information 

# Filter tag metadata to only include the 9 turtles were are interested in 
tag_metadata <- tag_metadata %>%
  filter(tag_id %in% Both) # "Both" vector created previously 

# Get detetion data in the right format for {VTrack}
detections9 <- filtdet9
detections9$receiver_name <- sub("^", "VR2W-", detections9$receiver_sn) # add VR2W prefix to receiver name as per VTRACK requirements
detections9$transmitter_id <- sub("^", "A69-1601-", detections9$transmitter_id) # add prefix to transmitter_id as per VTRACK requirements
detections9$detection_timestamp <- as.POSIXct(detections9$detection_timestamp, format = "%m/%e/%y %H:%M", tz = "America/New_York") # change timestamp to POSIXct class
detections9$latitude <- detections9$deploy_lat # change column names as per VTRACK requirements 
detections9$longitude <- detections9$deploy_long
detections9$station_name <- detections9$recstation
namevector <- c("sensor_value", "sensor_unit") # add columns for sensors (columns required for VTRACK)
detections9[ , namevector] <- NA # fill columns with NAs
detections9 <- st_as_sf(detections9, coords = c("longitude", "latitude"), crs = 4326) 
detections9$longitude<-st_coordinates(detections9)[,1]
detections9$latitude<-st_coordinates(detections9)[,2] 
detections9 %>% # select only columns that we need 
  dplyr::select(transmitter_id, receiver_name, detection_timestamp, station_name, sensor_value, sensor_unit, longitude, latitude)

# Get receiver data in the right format for {VTrack}
recinfo <- recdata # rename recdata to recinfo as per package
recinfo <- recinfo %>% # rename columns as specified by VTrack
  rename(deploymentdatetime_timestamp = deploy_date_time) %>%
  rename(recoverydatetime_timestamp = recover_date_time) %>%
  rename(station_name = recstation) %>%
  rename(station_latitude = deploy_lat) %>%
  rename(station_longitude = deploy_long)
recinfo$receiver_name <- sub("^", "VR2W-", recinfo$receiver_sn) # add prefix
# add additional columns with information as per VTrack:
addcol1 <- "installation_name"
recinfo[ , addcol1] <- "Bimini"
addcol2 <- "project_name"
recinfo[ , addcol2] <- "BBFSF.Bim"
addcol3 <- "status"
recinfo[ , addcol3] <- "deployed"


## IMPORT DATA INTO {VTrack} ##

# use setupData to import all necessary data into package 
# detection data was set up in "IMOS" style 

Data <- VTrack::setupData(Tag.Detections = detections9, 
                          Tag.Metadata = tag_metadata, 
                          Station.Information = recinfo, 
                          source = "IMOS", 
                          crs = sp::CRS("+init=epsg:4326"))


detsummary <- detectionSummary(Data) # summary stats
detsummary$Overall # view overall summary stats
write.csv(detsummary[["Overall"]], "sumstats_overall.csv")


## ABACUS PLOTS ##

# All individuals over time
abacusPlot(Data, 
           det.col = "black", 
           tag.col = "red")


# Each individual in different panels 
abacusPlot(Data, 
           det.col = "black", 
           tag.col = "red", 
           id = c("64475", "64476", "64477", "64479", "64480","64481", "64482", "64483", "64484"), 
           facet = TRUE)

# Each individual alone 
abacusPlot(Data, 
           det.col = "black", 
           tag.col = "red", 
           id = "64475", # change ID per turtle
           facet = TRUE)


## FILTER DETECTION DATA FOR MATCHING TEMPORAL DURATION ##

# There are 3 turtles that were tracked longer with acoustic telemetry than satellite telemetry. In order to compare UDs between methods within the same time period, we will subset the acoustic data for these 3 turtles to match the shorter tracking duration time frame of the satellite data. 
detsub64475 <- filtdet9 %>%
  filter(transmitter_id == "64475") %>%
  filter(detection_timestamp < "2017-07-17 01:27:28") # satellite tracking ended here
detsub64477 <- filtdet9 %>%
  filter(transmitter_id == "64477") %>%
  filter(detection_timestamp < "2017-06-26 01:56:41") # satellite tracking ended here
detsub64479 <- filtdet9 %>%
  filter(transmitter_id == "64479") %>%
  filter(detection_timestamp < "2017-08-12 10:09:07") # satellite tracking ended here
detsub64480 <- filtdet9 %>%
  filter(transmitter_id == "64480")
detsub64481 <- filtdet9 %>%
  filter(transmitter_id == "64481")
detsub64482 <- filtdet9 %>%
  filter(transmitter_id == "64482") 
detsub64483 <- filtdet9 %>%
  filter(transmitter_id == "64483") 
detsub64484 <- filtdet9 %>%
  filter(transmitter_id == "64484")

filtdet9_sub <- rbind(detsub64475, detsub64477, detsub64479, detsub64480, detsub64481, detsub64482, detsub64483, detsub64484)
write_csv(filtdet9_sub, "filtdet9_sub.csv")


## Next script = Detection Reassignment


