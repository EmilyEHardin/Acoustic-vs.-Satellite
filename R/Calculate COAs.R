#### CALCULATE COAs ####
# Previous Script: Detection Reassignment

# Now that we have our scattered detection points based on receiver detection probability (new.det.data.csv), we will import them into the {VTrack} package and use these locations to estimate COAs. 

library(dplyr)
library(lubridate)
library(sf)
library(tidyverse)
library(VTrack)


## IMPORT DATA ## 
# Import if needed, prepare dataframe as needed for VTrack 

# detdata <- read.csv("new.det.data.csv", header = T) # import reassigned detection locations 
# detdata$detection_timestamp <- as.POSIXct(detdata$detection_timestamp, format = "%m/%d/%y %H:%M, tz = "America/New_York")
detdata <- new.det.data
detdata$receiver_name <- sub("^", "VR2W-", detdata$receiver_sn) # add VR2W prefix to receiver name as per VTrack requirements
detdata$transmitter_id <- sub("^", "A69-1601-", detdata$transmitter_id) # add prefix to transmitter_id as per VTrack requirements
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
  dplyr::select(transmitter_id, receiver_name, detection_timestamp, station_name, sensor_value, sensor_unit, longitude, latitude, dist_m)

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
write.csv(COAs[["64475"]], "COA64475.csv")
write.csv(COAs[["64477"]], "COA64477.csv")
write.csv(COAs[["64479"]], "COA64479.csv")
write.csv(COAs[["64480"]], "COA64480.csv")
write.csv(COAs[["64481"]], "COA64481.csv")
write.csv(COAs[["64482"]], "COA64482.csv")
write.csv(COAs[["64483"]], "COA64483.csv")
write.csv(COAs[["64484"]], "COA64484.csv")

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
Study_Area <- st_read(dsn = "bimini shapefile/bimini_shape.shp", quiet = TRUE) # shapefile of study area
land <- lengths(st_intersects(COAall, Study_Area)) > 0  
COA.land <- COAall[land,] # no COAs over land 


## FIND LOCATION ERROR FOR COAS ##

# dist_m is the distance between the receiver the detection was logged on and the new coordinates after being reassigned by the probability distribution 
range(new.det.data$dist_m) # 0 - 750 m 
median(new.det.data$dist_m) # 177 m 
mean(new.det.data$dist_m) # 216.65 m 

# COA error will be equal to the mean distance from the receiver location to the newly assigned location for all detections that were used to calculate a particular COA
# subset data for each individual 
newdata.64475 <- new.det.data%>%
  filter(transmitter_id == "64475") 
# split each individual data set into 30-min timesteps to match the COAs 
from=round(min(newdata.64475$detection_timestamp),"hour")-hours(1) 
to=round(max(newdata.64475$detection_timestamp),"hour")+hours(1)
breaks=seq(from, to, by="30 min")
split.64475 <- split(newdata.64475,cut(newdata.64475$detection_timestamp, breaks))
# remove any time steps that have no detections/data
split.64475 <- split.64475[which(lapply(split.64475, nrow) != 0)] 
# calculate mean distance within each COA/30-min timestep and convert to dataframe, rename column title
COAerr.64475 <- sapply(split.64475, function(x) mean(x$dist_m))
COAerr.64475 <- as.data.frame(COAerr.64475)
COAerr.64475 <- rename(COAerr.64475, Err = COAerr.64475)

# repeat for every individual 
# Turtle C 
newdata.64477 <- new.det.data%>%
  filter(transmitter_id == "64477") 
# split each individual data set into 30-min timesteps to match the COAs 
from=round(min(newdata.64477$detection_timestamp),"hour")-hours(1) 
to=round(max(newdata.64477$detection_timestamp),"hour")+hours(1)
breaks=seq(from, to, by="30 min")
split.64477 <- split(newdata.64477,cut(newdata.64477$detection_timestamp, breaks))
# remove any time steps that have no detections/data
split.64477 <- split.64477[which(lapply(split.64477, nrow) != 0)] 
# calculate mean distance within each COA/30-min timestep and convert to dataframe 
COAerr.64477 <- sapply(split.64477, function(x) mean(x$dist_m))
COAerr.64477 <- as.data.frame(COAerr.64477)
COAerr.64477 <- rename(COAerr.64477, Err = COAerr.64477)

# Turtle D
newdata.64479 <- new.det.data%>%
  filter(transmitter_id == "64479") 
# split each individual data set into 30-min timesteps to match the COAs 
from=round(min(newdata.64479$detection_timestamp),"hour")-hours(1) 
to=round(max(newdata.64479$detection_timestamp),"hour")+hours(1)
breaks=seq(from, to, by="30 min")
split.64479 <- split(newdata.64479,cut(newdata.64479$detection_timestamp, breaks))
# remove any time steps that have no detections/data
split.64479 <- split.64479[which(lapply(split.64479, nrow) != 0)] 
# calculate mean distance within each COA/30-min timestep and convert to dataframe 
COAerr.64479 <- sapply(split.64479, function(x) mean(x$dist_m))
COAerr.64479 <- as.data.frame(COAerr.64479)
COAerr.64479 <- rename(COAerr.64479, Err = COAerr.64479)

# Turtle E
newdata.64480 <- new.det.data%>%
  filter(transmitter_id == "64480") 
# split each individual data set into 30-min timesteps to match the COAs 
from=round(min(newdata.64480$detection_timestamp),"hour")-hours(1) 
to=round(max(newdata.64480$detection_timestamp),"hour")+hours(1)
breaks=seq(from, to, by="30 min")
split.64480 <- split(newdata.64480,cut(newdata.64480$detection_timestamp, breaks))
# remove any time steps that have no detections/data
split.64480 <- split.64480[which(lapply(split.64480, nrow) != 0)] 
# calculate mean distance within each COA/30-min timestep and convert to dataframe 
COAerr.64480 <- sapply(split.64480, function(x) mean(x$dist_m))
COAerr.64480 <- as.data.frame(COAerr.64480)
COAerr.64480 <- rename(COAerr.64480, Err = COAerr.64480)

# Turtle F
newdata.64481 <- new.det.data%>%
  filter(transmitter_id == "64481") 
# split each individual data set into 30-min timesteps to match the COAs 
from=round(min(newdata.64481$detection_timestamp),"hour")-hours(1) 
to=round(max(newdata.64481$detection_timestamp),"hour")+hours(1)
breaks=seq(from, to, by="30 min")
split.64481 <- split(newdata.64481,cut(newdata.64481$detection_timestamp, breaks))
# remove any time steps that have no detections/data
split.64481 <- split.64481[which(lapply(split.64481, nrow) != 0)] 
# calculate mean distance within each COA/30-min timestep and convert to dataframe 
COAerr.64481 <- sapply(split.64481, function(x) mean(x$dist_m))
COAerr.64481 <- as.data.frame(COAerr.64481)
COAerr.64481 <- rename(COAerr.64481, Err = COAerr.64481)

# Turtle G
newdata.64482 <- new.det.data%>%
  filter(transmitter_id == "64482") 
# split each individual data set into 30-min timesteps to match the COAs 
from=round(min(newdata.64482$detection_timestamp),"hour")-hours(1) 
to=round(max(newdata.64482$detection_timestamp),"hour")+hours(1)
breaks=seq(from, to, by="30 min")
split.64482 <- split(newdata.64482,cut(newdata.64482$detection_timestamp, breaks))
# remove any time steps that have no detections/data
split.64482 <- split.64482[which(lapply(split.64482, nrow) != 0)] 
# calculate mean distance within each COA/30-min timestep and convert to dataframe 
COAerr.64482 <- sapply(split.64482, function(x) mean(x$dist_m))
COAerr.64482 <- as.data.frame(COAerr.64482)
COAerr.64482 <- rename(COAerr.64482, Err = COAerr.64482)

# Turtle H 
newdata.64483 <- new.det.data%>%
  filter(transmitter_id == "64483") 
# split each individual data set into 30-min timesteps to match the COAs 
from=round(min(newdata.64483$detection_timestamp),"hour")-hours(1) 
to=round(max(newdata.64483$detection_timestamp),"hour")+hours(1)
breaks=seq(from, to, by="30 min")
split.64483 <- split(newdata.64483,cut(newdata.64483$detection_timestamp, breaks))
# remove any time steps that have no detections/data
split.64483 <- split.64483[which(lapply(split.64483, nrow) != 0)] 
# calculate mean distance within each COA/30-min timestep and convert to dataframe 
COAerr.64483 <- sapply(split.64483, function(x) mean(x$dist_m))
COAerr.64483 <- as.data.frame(COAerr.64483)
COAerr.64483 <- rename(COAerr.64483, Err = COAerr.64483)

# Turtle I 
newdata.64484 <- new.det.data%>%
  filter(transmitter_id == "64484") 
# split each individual data set into 30-min timesteps to match the COAs 
from=round(min(newdata.64484$detection_timestamp),"hour")-hours(1) 
to=round(max(newdata.64484$detection_timestamp),"hour")+hours(1)
breaks=seq(from, to, by="30 min")
split.64484 <- split(newdata.64484,cut(newdata.64484$detection_timestamp, breaks))
# remove any time steps that have no detections/data
split.64484 <- split.64484[which(lapply(split.64484, nrow) != 0)] 
# calculate mean distance within each COA/30-min timestep and convert to dataframe 
COAerr.64484 <- sapply(split.64484, function(x) mean(x$dist_m))
COAerr.64484 <- as.data.frame(COAerr.64484)
COAerr.64484 <- rename(COAerr.64484, Err = COAerr.64484)

# combine all of the COA error dataframe together in order, then merge with the COA data frame 
COAerr <- rbind(COAerr.64475, COAerr.64477, COAerr.64479, COAerr.64480, COAerr.64481, COAerr.64482, COAerr.64483, COAerr.64484)
COAall$Err <- COAerr$Err
write.csv(COAall, "COAall_loc.err.csv")

# Location error summary:
range(COAall$Err) # 0 - 748 m 
median(COAall$Err) # 204 m 
mean(COAall$Err) # 215.801


## CALCULATE COAs FOR SUBSET DATA ##
# Import if needed, prepare dataframe as needed for VTrack 

# detdata_sub <- read.csv("new.det.data_sub.csv", header = T)
# detdata_sub$detection_timestamp <- as.POSIXct(detdata_sub$detection_timestamp, format = "%m/%d/%y %H:%M", tz = "America/New_York")
detdata_sub <- new.det.data_sub
detdata_sub$receiver_name <- sub("^", "VR2W-", detdata_sub$receiver_sn)  # add VR2W prefix to receiver name as per VTrack requirements
detdata_sub$transmitter_id <- sub("^", "A69-1601-", detdata_sub$transmitter_id) # add prefix to transmitter_id as per VTrack requirements
# change column names as per VTRACK requirements 
detdata_sub$latitude <- detdata_sub$lat
detdata_sub$longitude <- detdata_sub$lon
detdata_sub$station_name <- detdata_sub$recstation
namevector <- c("sensor_value", "sensor_unit")
detdata_sub[ , namevector] <- NA
detdata_sub <- st_as_sf(detdata_sub, coords = c("longitude", "latitude"), crs = 4326) 
detdata_sub$longitude<-st_coordinates(detdata_sub)[,1] 
detdata_sub$latitude<-st_coordinates(detdata_sub)[,2] 
detdata_sub <- detdata_sub %>%
  dplyr::select(transmitter_id, receiver_name, detection_timestamp, station_name, sensor_value, sensor_unit, longitude, latitude, dist_m)

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

# check to see if COAs fall over land 
COA_sub <- sf::st_as_sf(COA_sub, coords=c("Longitude.coa","Latitude.coa"), crs = 4326)
COA_sub$Longitude.coa <- st_coordinates(COA_sub)[,1] # get coordinates
COA_sub$Latitude.coa <- st_coordinates(COA_sub)[,2] # get coordinates
# Study_Area <- st_read(dsn = "bimini shapefile/bimini_shape.shp", quiet = TRUE) # shapefile of study area
land <- lengths(st_intersects(COA_sub, Study_Area)) > 0  
COA_sub.land <- COA_sub[land,] # 0 COAs fell over land 


## FIND LOCATION ERROR FOR COAS ##

# COA error will be equal to the mean distance from the receiver location to the newly assigned location for all detections used to calculate a particular COA
# subset data for each individual 
newdata.64475_sub <- new.det.data_sub%>%
  filter(transmitter_id == "64475") 
# split each individual data set into 30-min timesteps to match the COAs 
from=round(min(newdata.64475_sub$detection_timestamp),"hour")-hours(1) 
to=round(max(newdata.64475_sub$detection_timestamp),"hour")+hours(1)
breaks=seq(from, to, by="30 min")
split.64475_sub <- split(newdata.64475_sub,cut(newdata.64475_sub$detection_timestamp, breaks))
# remove any time steps that have no detections/data
split.64475_sub <- split.64475_sub[which(lapply(split.64475_sub, nrow) != 0)] 
# calculate mean distance within each COA/30-min timestep and convert to dataframe, rename column title
COAerr.64475_sub <- sapply(split.64475_sub, function(x) mean(x$dist_m))
COAerr.64475_sub <- as.data.frame(COAerr.64475_sub)
COAerr.64475_sub <- rename(COAerr.64475_sub, Err = COAerr.64475_sub)

# repeat for every individual 
# Turtle C 
newdata.64477_sub <- new.det.data_sub%>%
  filter(transmitter_id == "64477") 
# split each individual data set into 30-min timesteps to match the COAs 
from=round(min(newdata.64477_sub$detection_timestamp),"hour")-hours(1) 
to=round(max(newdata.64477_sub$detection_timestamp),"hour")+hours(1)
breaks=seq(from, to, by="30 min")
split.64477_sub <- split(newdata.64477_sub,cut(newdata.64477_sub$detection_timestamp, breaks))
# remove any time steps that have no detections/data
split.64477_sub <- split.64477_sub[which(lapply(split.64477_sub, nrow) != 0)] 
# calculate mean distance within each COA/30-min timestep and convert to dataframe 
COAerr.64477_sub <- sapply(split.64477_sub, function(x) mean(x$dist_m))
COAerr.64477_sub <- as.data.frame(COAerr.64477_sub)
COAerr.64477_sub <- rename(COAerr.64477_sub, Err = COAerr.64477_sub)

# Turtle D
newdata.64479_sub <- new.det.data_sub%>%
  filter(transmitter_id == "64479") 
# split each individual data set into 30-min timesteps to match the COAs 
from=round(min(newdata.64479_sub$detection_timestamp),"hour")-hours(1) 
to=round(max(newdata.64479_sub$detection_timestamp),"hour")+hours(1)
breaks=seq(from, to, by="30 min")
split.64479_sub <- split(newdata.64479_sub,cut(newdata.64479_sub$detection_timestamp, breaks))
# remove any time steps that have no detections/data
split.64479_sub <- split.64479_sub[which(lapply(split.64479_sub, nrow) != 0)] 
# calculate mean distance within each COA/30-min timestep and convert to dataframe 
COAerr.64479_sub <- sapply(split.64479_sub, function(x) mean(x$dist_m))
COAerr.64479_sub <- as.data.frame(COAerr.64479_sub)
COAerr.64479_sub <- rename(COAerr.64479_sub, Err = COAerr.64479_sub) 

# combine all of the COA error dataframe together in order, then merge with the COA data frame 
COAerr_sub <- rbind(COAerr.64475_sub, COAerr.64477_sub, COAerr.64479_sub)
COA_sub$Err <- COAerr_sub$Err
write.csv(COA_sub, "COAall_sub_loc.err.csv")

# Location error summary:
range(COA_sub$Err) # 0 - 748 m 
median(COA_sub$Err) # 208.29 m 
mean(COA_sub$Err) # 217.47

# Next Script: Satellite Filtering 
