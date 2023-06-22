## Supplementary Study
## This script follows the same process as the other individualized scripts for dBBMMs, but subsets the corrected satellite locations to only those locations falling within the 1% detection range of receivers

library(dplyr)
library(lubridate)
library(tidyverse)
library(VTrack)
library(sf)
library(move)


# Import satellite data - this data has already been filtered and a continuous time correlated random walk model has been applied with the {foieGras} package to correct Argos transmission locations. Locations over land have already been removed. 
sat2 <- read.csv("fG_noland.csv", header = TRUE)
sat2$timestamp<-as.POSIXct(sat2$date, format = "%m/%d/%Y %H:%M:%S", tz = "America/New_York")
sat2 <- sat2 %>%
  mutate(id = make.names(id)) %>%
  dplyr::select(id, timestamp, x, y, Location.Err) %>%
  filter(Location.Err < 3000) # filter out any transmissions with location errors > 3km 

# Import satellite subset data
sat_sub2 <- read.csv("fG_sub_noland.csv", header = TRUE)
sat_sub2$timestamp<-as.POSIXct(sat_sub2$date, format = "%m/%d/%Y %H:%M:%S", tz = "America/New_York")
sat_sub2 <- sat_sub2 %>%
  mutate(id = make.names(id)) %>%
  dplyr::select(id, timestamp, x, y, Location.Err)  %>%
  filter(Location.Err < 3000)

sat_sub2 %>%
  group_by(id) %>%
  summarize(n = n()) # all turtles have at least 60 locations 

# Import receiver data 
recdata2 <- read.csv("rec_final.csv", header = TRUE)
recdata2 <- st_as_sf(recdata2, coords = c("lon", "lat"), crs = 4326)
recdata2 <- st_transform(recdata2, 3395)
recdata2$lon<-st_coordinates(recdata2)[,1]
recdata2$lat<-st_coordinates(recdata2)[,2]

# Split receivers into reef and sand receivers
reef <- recdata2 %>% filter(habitat == "reef")
sand <- recdata2 %>% filter(habitat == "bank") 

Study_Area <- st_read(dsn = "bimini shapefile/bimini_shape.shp", quiet = TRUE)
land <- Study_Area[Study_Area$AltMode == "0", ] # extract just one layer from the collection 
land <- st_union(land) # make it a polygon layer
land <- st_transform(land, 3395)

# Calculate 1% detection range of array 
reef2 <- st_buffer(reef, 520) # buffer each reef point by 520 m (based on logistic regression in "Detection Reassignment")
sand2 <- st_buffer(sand, 1100) # buffer each bank point by 1100 m 
reef2 <- st_union(reef2) 
sand2 <- st_union(sand2) 
reef2 <- st_make_valid(reef2)
sand2 <- st_make_valid(sand2)
array.1 <- st_union(reef2, sand2)
array.1 <- st_difference(array.1, land)
st_area(array.1) # 95094844 [m^2] // 95.09 km2

# add geometry 
sat2 <- sf::st_as_sf(sat2, coords=c("x","y"), crs = 3395)
sat2$lon <- st_coordinates(sat2)[,1] # get coordinates
sat2$lat <- st_coordinates(sat2)[,2] # get coordinates

sat_sub2 <- sf::st_as_sf(sat_sub2, coords=c("x","y"), crs = 3395)
sat_sub2$lon <- st_coordinates(sat_sub2)[,1] # get coordinates
sat_sub2$lat <- st_coordinates(sat_sub2)[,2] # get coordinates

# keep only satellite locations that fall within the 1% detection range 
sat_in_rec <- lengths(st_intersects(sat2, array.1)) > 0  
sat_in_rec <- sat2[sat_in_rec,] # satellite locations within 1% detection range

sat_in_rec %>%
  group_by(id) %>%
  summarize(n = n(), 
            LastDate = max(timestamp)) # number of locations per turtle

sat_rec <- lengths(st_intersects(sat2, array.1)) > 0  
sat_not_rec <- sat2[!sat_rec,] # satellite points falling outside 1% detection range

sat_sub_in_rec <- lengths(st_intersects(sat_sub2, array.1)) > 0  
sat_sub_in_rec <- sat_sub2[sat_sub_in_rec,]

sat_sub_in_rec %>%
  group_by(id) %>%
  summarize(n = n(), 
            LastDate = max(timestamp)) # all turtles have at least 36 locations 


# visualize 
Before <- ggplot(data = land) +
  geom_sf(fill = "grey", col = "grey") +
  geom_sf(data = array.1, fill = "deepskyblue1", col = "deepskyblue1", lwd = 0.25, alpha = 0.3) +
  geom_point(data = sat2, aes(lon, lat, col = "grey28"), col = "grey28", size = 0.5, alpha = 0.8, show.legend = T) +
  coord_sf(xlim = c(-8838767.57, -8794239.77)) +
  scale_x_continuous(breaks = c(-79.1, -79.3, -79.5)) +
  scale_y_continuous(breaks = c(25.6, 25.8, 26.0)) +
  annotation_scale(style = "ticks", location = "br", text_cex = 0.9, line_width = 1, height = unit(0.35, "cm"), text_pad = unit(0.35, "cm"), pad_x = unit(0.35, "cm"), pad_y = unit(0.35, "cm")) +
  scale_color_manual(values = c("grey28" = "grey28"), labels = c(grey28 = "Satellite Locations"), name = NULL, guide = guide_legend(override.aes = list(linetype = "blank", shape = 20, fill = NA, size = 3.5))) + 
  annotate("text", x = -8838767.57, y = 2986515.99, label = "a)", size = 8, hjust = 0, vjust = 1) + 
  theme_bw() +
  theme(text = element_text(size = 15)) +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5)) +
  theme(axis.ticks.y = element_line(size = 0.5)) +
  theme(axis.ticks.x = element_line(size = 0.5)) +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  theme(legend.position = "bottom")
ggsave(filename = "Before.tiff", plot = Before, device = "tiff",path = "/Users/EmilyHardin/Desktop/Research/Acous.vs.Sat", width = 120, height = 120, units = c("mm"), dpi = 600)

After <- ggplot(data = land) +
  geom_sf(fill = "grey", col = "grey") +
  geom_sf(data = array.1, aes(fill = "deepskyblue1"), fill = "deepskyblue1", col = "deepskyblue1", lwd = 0.25, alpha = 0.3, show.legend = "polygon") +
  geom_point(data = sat_in_rec, aes(lon, lat), col = "grey28", size = 0.5, alpha = 0.8) +
  coord_sf(xlim = c(-8838767.57, -8794239.77)) +
  scale_x_continuous(breaks = c(-79.1, -79.3, -79.5)) +
  scale_y_continuous(breaks = c(25.6, 25.8, 26.0)) +
  annotation_scale(style = "ticks", location = "br", text_cex = 0.9, line_width = 1, height = unit(0.35, "cm"), text_pad = unit(0.35, "cm"), pad_x = unit(0.35, "cm"), pad_y = unit(0.35, "cm")) +
  scale_fill_identity(name = NULL, labels = c(deepskyblue1 = "Array Detection\nRange"), limits = c("deepskyblue1"), guide = guide_legend(override.aes = list(linetype = c("blank")), shape = c((NA)))) + 
  annotate("text", x = -8838767.57, y = 2986515.99, label = "a)", size = 8, hjust = 0, vjust = 1) + 
  theme_bw() +
  theme(text = element_text(size = 15)) +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5)) +
  theme(axis.ticks.y = element_line(size = 0.5)) +
  theme(axis.ticks.x = element_line(size = 0.5)) +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  theme(legend.position = "bottom")
ggsave(filename = "After.tiff", plot = After, device = "tiff",path = "/Users/EmilyHardin/Desktop/Research/Acous.vs.Sat", width = 120, height = 120, units = c("mm"), dpi = 600)

Both <- ggplot(data = land) +
  geom_sf(fill = "grey", col = "grey") +
  geom_sf(data = array.1, aes(fill = "deepskyblue1"), fill = "deepskyblue1", col = "deepskyblue1", lwd = 0.25, alpha = 0.3) +
  geom_point(data = sat_not_rec, aes(lon, lat, col = "darkred"), size = 0.5, alpha = 0.7, show.legend = T) +
  geom_point(data = sat_in_rec, aes(lon, lat, col = "grey28"), size = 0.5, alpha = 0.7, show.legend = T) +
  coord_sf(xlim = c(-8838767.57, -8794239.77)) +
  scale_x_continuous(breaks = c(-79.1, -79.3, -79.5)) +
  scale_y_continuous(breaks = c(25.6, 25.8, 26.0)) +
  annotation_scale(style = "ticks", location = "br", text_cex = 0.9, line_width = 1, height = unit(0.35, "cm"), text_pad = unit(0.35, "cm"), pad_x = unit(0.35, "cm"), pad_y = unit(0.35, "cm")) +
  scale_color_manual(values = c("grey28" = "grey28", "darkred" = "darkred"), labels = c(grey28 = "Within", darkred = "Outside"), name = "Satellite Locations", guide = guide_legend(override.aes = list(linetype = c("blank", "blank"), shape = c(20, 20), fill = c(NA, NA), size = c(3.5, 3.5)))) + 
  theme_bw() +
  theme(text = element_text(size = 12)) +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5)) +
  theme(axis.ticks.y = element_line(size = 0.5)) +
  theme(axis.ticks.x = element_line(size = 0.5)) +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  theme(legend.position = "right")
ggsave(filename = "Both.tiff", plot = Both, device = "tiff",path = "/Users/EmilyHardin/Desktop/Research/Acous.vs.Sat", width = 120, height = 120, units = c("mm"), dpi = 600)


## now that the satellite data has been subset, turtles that were tracked longer with satellite data than acoustic data were reassessed to see if the acoustic subset data needs to be adjusted given the "new" satellite data. This will require re-subsetting the acoustic detection data for Turtles A, C, and D to match the new last transmission times for the satellite data. COAs will need to be recalculated. 

# Import detection data 
detdata <- read.csv("turtle_detections_2.csv", header = T) 
detdata$detection_timestamp <- as.POSIXct(detdata$detection_timestamp_est, format = "%m/%e/%y %H:%M", tz = "America/New_York")  # convert datetime to POSIXct in America/New York time
detdata$receiver_sn <- factor(detdata$receiver_sn) # convert station column to categorical variable
detdata$transmitter_id <- factor(detdata$transmitter_id) # convert Transmitter.ID column to categorical 
detdata$detstation <- detdata$station # rename station so it is different than recdata station 
detdata <- detdata %>% # select just the columns we are interested in 
  dplyr::select(detection_timestamp, transmitter_id, receiver_sn, detstation)

recdata <- read.csv("rec.move.coord.csv", header = T)
recdata$receiver_sn <- factor(recdata$receiver_sn) # convert station column to categorical variable
recdata$deploy_date_time <- as.POSIXct(recdata$deploy_date_time, format = "%m/%e/%y %H:%M", tz = "America/New_York") 
recdata$recover_date_time <- as.POSIXct(recdata$recover_date_time, format = "%m/%e/%y %H:%M", tz = "America/New_York") 
recdata <- st_as_sf(recdata, coords = c("deploy_long_WGS", "deploy_lat_WGS"), crs = 4326) 
recdata$long<-st_coordinates(recdata)[,1] # get coordinates
recdata$lat<-st_coordinates(recdata)[,2] # get coordinates
recdata$deploy_lat <- recdata$lat # rename columns
recdata$deploy_long <- recdata$long # rename columns
recdata$recstation <- recdata$station # rename station so different than detdata station (confirm that recstation and detstation are the same)
recdata <- recdata %>% # retain only columns that we are interested in 
  dplyr::select(receiver_sn, deploy_date_time, recover_date_time, deploy_lat, deploy_long, recstation, habitat)

det <- left_join(recdata, detdata, by = "receiver_sn") %>% 
  filter(detection_timestamp >= deploy_date_time & detection_timestamp <= recover_date_time) %>% 
  dplyr::select(detection_timestamp, transmitter_id, receiver_sn, deploy_lat, deploy_long, recstation, detstation, habitat)

# this information will be needed for the COAs if not already imported 
tag_metadata <- read.csv("tag_metadata.csv", header = T)
tag_metadata$ReleaseDate <- as.POSIXct(paste(tag_metadata$Date, tag_metadata$Time), format = "%m/%e/%y %H:%M", tz = "America/New_York") # combine date and time, convert datetime to POSIXct in America/New York time 
tag_metadata <- st_as_sf(tag_metadata, coords = c("release_longitude", "release_latitude"), crs = 4326) 
tag_metadata$release_longitude <- st_coordinates(tag_metadata)[,1]
tag_metadata$release_latitude <- st_coordinates(tag_metadata)[,2] 

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


Turts <- c("64475", "64477", "64479") # create vector containing the turtle IDs that are needed
det2 <- det %>% # filter Det data based on the IDs in above created vector
  filter(transmitter_id %in% Turts) 

# Filter data to the proper timeframe that fits with the satellite data now that the satellite dataset has been subset to only those locations within the array (data sets should have equal timeframes)
det64475 <- det2 %>%
  filter(transmitter_id == "64475") %>%
  filter(detection_timestamp > "2017-05-03 14:16:00") %>% # 24 hours after release
  filter(detection_timestamp < "2017-07-15 01:04:58") # new last transmission

det64477 <- det2 %>%
  filter(transmitter_id == "64477") %>%
  filter(detection_timestamp > "2017-05-03 14:13:00") %>%
  filter(detection_timestamp < "2017-06-16 02:08:15")

det64479 <- det2 %>%
  filter(transmitter_id == "64479") %>%
  filter(detection_timestamp > "2017-05-04 15:58:00")  %>%
  filter(detection_timestamp < "2017-08-12 08:11:08")

filtdet3 <- rbind(det64475, det64477, det64479)

# maintenance resulted in 1/3 of receivers being offline, so all detections are being removed from this time period to reduce bias
filtdet3 <- filtdet3 %>% 
  filter(detection_timestamp <= "2017-05-04 08:21:00" | detection_timestamp >= "2017-05-06 17:37:00")

# Get detetion data in the right format for {VTrack}
detections3 <- filtdet3
detections3$receiver_name <- sub("^", "VR2W-", detections3$receiver_sn) # add VR2W prefix to receiver name as per VTRACK requirements
detections3$transmitter_id <- sub("^", "A69-1601-", detections3$transmitter_id) # add prefix to transmitter_id as per VTRACK requirements
detections3$detection_timestamp <- as.POSIXct(detections3$detection_timestamp, format = "%m/%e/%y %H:%M", tz = "America/New_York") # change timestamp to POSIXct class
detections3$latitude <- detections3$deploy_lat # change column names as per VTRACK requirements 
detections3$longitude <- detections3$deploy_long
detections3$station_name <- detections3$recstation
namevector <- c("sensor_value", "sensor_unit") # add columns for sensors (columns required for VTRACK)
detections3[ , namevector] <- NA # fill columns with NAs
detections3 <- st_as_sf(detections3, coords = c("longitude", "latitude"), crs = 4326) 
detections3$longitude<-st_coordinates(detections3)[,1]
detections3$latitude<-st_coordinates(detections3)[,2] 
detections3 %>% # select only columns that we need 
  dplyr::select(transmitter_id, receiver_name, detection_timestamp, station_name, sensor_value, sensor_unit, longitude, latitude)

# open "Detection Reassignment" and run the code up until the functions are inputted into the R environment, if not already 

filtdet3 <- as.data.frame(filtdet3) # convert from sf object to dataframe

bank.det <- filtdet3 %>%
  filter(habitat == "bank") # create dataframe of detections on sandy bank receivers
reef.det <- filtdet3 %>%
  filter(habitat == "reef") # create dataframe of detections on reef receivers

# run functions created above (via "Detection Reassignment" script) to reassign detection locations
set.seed(2022)
new.reef.sub <- r.reef(data = reef.det, coords = c('deploy_long','deploy_lat'), land_shp = Study_Area)
new.bank.sub <- r.bank(data = bank.det, coords = c('deploy_long','deploy_lat'), land_shp = Study_Area)

# these sf dataframes currently have the geometry associated with deploy_lat/deploy_long (i.e. receiver locations), but we need to reset the geometry to the newly assigned coordinates
new.reef.sub <- new.reef.sub %>%
  dplyr::select(-geometry) %>%
  st_as_sf(., coords=c("lon1","lat1"), crs = 4326, remove = FALSE) %>%
  st_transform(3395) # transform the CRS projected World Mercator, units = m

new.bank.sub <- new.bank.sub %>%
  dplyr::select(-geometry) %>%
  st_as_sf(., coords=c("lon1","lat1"), crs = 4326, remove = FALSE) %>%
  st_transform(3395) # transform the CRS projected World Mercator, units = m

# combine reef and bank data
new.det.sub <- rbind(new.bank.sub, new.reef.sub) %>%
  st_drop_geometry()
write.csv(new.det.sub, "new.det.sup.csv", row.names = FALSE)

detdata <- new.det.sub
detdata$receiver_name <- sub("^", "VR2W-", detdata$receiver_sn) # add VR2W prefix to receiver name as per VTrack requirements
detdata$transmitter_id <- sub("^", "A69-1601-", detdata$transmitter_id) # add prefix to transmitter_id as per VTrack requirements
# change column names as per VTRACK requirements 
detdata$station_name <- detdata$recstation
namevector <- c("sensor_value", "sensor_unit") # add columns required for VTrack
detdata[ , namevector] <- NA
detdata$longitude <- detdata$lon
detdata$latitude <- detdata$lat
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

# make a dataframe for each turtle 
id_64475 <- COAs[["64475"]]
id_64477 <- COAs[["64477"]]
id_64479 <- COAs[["64479"]]

# combine into one dataframe
COAall <- rbind(id_64475, id_64477, id_64479)

# check to see if COAs fall over land 
COAall <- sf::st_as_sf(COAall, coords=c("Longitude.coa","Latitude.coa"), crs = 4326)
COAall$Longitude.coa <- st_coordinates(COAall)[,1] # get coordinates
COAall$Latitude.coa <- st_coordinates(COAall)[,2] # get coordinates
Study_Area <- st_read(dsn = "bimini shapefile/bimini_shape.shp", quiet = TRUE) # shapefile of study area
land <- lengths(st_intersects(COAall, Study_Area)) > 0  
COA.land <- COAall[land,] # no COAs fall over land


## FIND LOCATION ERROR FOR COAS ##

# dist_m is the distance between the receiver the detection was logged on and the new coordinates after being reassigned by the probability distribution 
# COA error will be equal to the mean distance from the receiver location to the newly assigned location for all detections used to calculate a particular COA
# subset data for each individual 
newdata.64475 <- new.det.sub%>%
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
newdata.64477 <- new.det.sub%>%
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
newdata.64479 <- new.det.sub%>%
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

# combine all of the COA error dataframe together in order, then merge with the COA data frame 
COAerr <- rbind(COAerr.64475, COAerr.64477, COAerr.64479)
COAall$Err <- COAerr$Err
COAall$timestamp <- COAall$TimeStep.coa
COAall$Location.Err <- COAall$Err
COAall$Location.Err[COAall$Location.Err == 0] <- 0.01 # distances/location errors were calculate as whole numbers including 0, but the dBBMM cannot take a location error argument of 0; therefore replace any 0s with 0.01 m
COAall<- COAall %>%
  mutate(Tag.ID = make.names(Tag.ID)) %>%
  dplyr::select(Tag.ID, timestamp, Latitude.coa, Longitude.coa, Location.Err)

COAall_sub_loc <- st_drop_geometry(COAall)

checkAs <- COAall_sub_loc %>%
  group_by(Tag.ID) %>%
  summarise(relocations = length(timestamp))
checkAs #all good


# We want the window size for the dBBMM to equal approximatley 24 hours, so we need to caculate the median timestep to know what to send the window as. 
library(bayesmove)
sat_in_rec2 <- sat_in_rec %>%
  rename(date = timestamp) %>%
  bayesmove::prep_data(., coord.names = c("lon", "lat"), id = "id")
median(sat_in_rec2$dt, na.rm = T)/3600
# median timestep = 1.69 

# Since median timestep = ~1.7, we will set the window to 15 and the margins to 5 

# check number of locations, ensure enough for dBBMM (>15)
checkS <- sat_in_rec %>%
  group_by(id) %>%
  summarise(relocations = length(timestamp))
checkS # all good, all turtles have greater than 63 locations

checkS_sub <- sat_sub_in_rec %>%
  group_by(id) %>%
  summarise(relocations = length(timestamp))
checkS_sub # all good, all turtles have greater than 36 locations 

# make move object for satellite data
sat_in_rec <- st_drop_geometry(sat_in_rec)
satrec <- move(x = sat_in_rec$lon, 
               y = sat_in_rec$lat, 
               time= sat_in_rec$timestamp, 
               proj = CRS("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"), 
               data = sat_in_rec, 
               animal = sat_in_rec$id, 
               Location.Error = sat_in_rec$Location.Err)

sat_sub_in_rec <- st_drop_geometry(sat_sub_in_rec)
satrec_sub <- move(x = sat_sub_in_rec$lon, 
                   y = sat_sub_in_rec$lat, 
                   time= sat_sub_in_rec$timestamp, 
                   proj = CRS("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"), 
                   data = sat_sub_in_rec, 
                   animal = sat_sub_in_rec$id, 
                   Location.Error = sat_sub_in_rec$Location.Err)

# Pull out each individual 
As2 <- satrec[["X169267"]]
Gs2 <- satrec[["X169268"]]
Cs2 <- satrec[["X169269"]]
Is2 <- satrec[["X169270"]]
Bs2 <- satrec[["X169271"]]
Ds2 <- satrec[["X169272"]]
Es2 <- satrec[["X169273"]]
Hs2 <- satrec[["X169274"]]

Ass2 <- satrec_sub[["X169267"]]
Css2 <- satrec_sub[["X169269"]]
Bss2 <- satrec_sub[["X169271"]]
Dss2 <- satrec_sub[["X169272"]]
Gss2 <- satrec_sub[["X169268"]]
Iss2 <- satrec_sub[["X169270"]]
Ess2 <- satrec_sub[["X169273"]]
Hss2 <- satrec_sub[["X169274"]]

# make move objects for acoustic data - the full temporal duration acoustic data should be the same from the main study and previous analyses, so we only need to make a move object of the acoustic subset data and pull out Turtles A, C, and D
COAall_sub_loc <- as.data.frame(COAall_sub_loc) # convert to data frame 
acoustic_sub2 <- move(x = COAall_sub_loc$Longitude.coa, 
                      y = COAall_sub_loc$Latitude.coa, 
                      time= COAall_sub_loc$timestamp, 
                      proj = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"), 
                      data = COAall_sub_loc, 
                      animal = COAall_sub_loc$Tag.ID, 
                      Location.Error = COAall_sub_loc$Location.Err)

acoustic_sub2 <- spTransform(acoustic_sub2, CRSobj = "+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")

Aas2 <- acoustic_sub2[["X64475"]]
Cas2 <- acoustic_sub2[["X64477"]]
Das2 <- acoustic_sub2[["X64479"]]

## RUN THE dBBMMs PER TURTLE

# TURTLE A

# identify the extent of the rasters for all of the turtle's {move} objects
# from all rasters, identify the maxs and mins in the x and y directions 
# add/substract 20,000 to each to create a new, common raster for the turtle 
extent(As2)
extent(Ass2)
extent(Aa)
extent(Aas2)
raster.A2 <- raster(
  xmn = -8848853, 
  xmx = -8799711, 
  ymn = 2919971, 
  ymx = 2977624, 
  crs = CRS("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"),
  resolution = 50)

registerDoParallel(detectCores() - 2) # identify the number of core processors on your computer, subtract 2
getDoParWorkers() # allows you to run code in parellel on the amount of core processors you have (minus 2)

# for tracks that have gaps in data > 24 hrs, we want the dBBMM to ignore the variance associated with those gaps 
# satellite variance
V.As2 <- move::brownian.motion.variance.dyn(As2, 
                                            location.error=As2@data[["Location.Err"]], 
                                            window.size=15, 
                                            margin=5)
V.As2@interest[timeLag(As2,"hours") > 24] <- FALSE
# satellite subset variance
V.Ass2 <- move::brownian.motion.variance.dyn(Ass2, 
                                             location.error=Ass2@data[["Location.Err"]], 
                                             window.size=15, 
                                             margin=5)
V.Ass2@interest[timeLag(Ass2,"hours") > 24] <- FALSE

V.Aa <- move::brownian.motion.variance.dyn(Aa, 
                                           location.error=Aa@data[["Location.Err"]], 
                                           window.size=49, 
                                           margin=15)
V.Aa@interest[timeLag(Aa,"hours") > 24] <- FALSE
# acoustic subset variance 
V.Aas2 <- move::brownian.motion.variance.dyn(Aas2, 
                                             location.error=Aas2@data[["Location.Err"]], 
                                             window.size=49, 
                                             margin=15)
V.Aas2@interest[timeLag(Aas2,"hours") > 24] <- FALSE

# Run the dBBMM with the objects created above so that the variance in gaps is ignored
# satellite dBBMM
dBB.As2 <- brownian.bridge.dyn(V.As2, 
                               ext = 0.5, 
                               raster = raster.A2, 
                               location.error = As2@data[["Location.Err"]],
                               window = 15, 
                               margin = 5)
# satellite subset dBBMM
dBB.Ass2 <- brownian.bridge.dyn(V.Ass2, 
                                ext = 0.5, 
                                raster = raster.A2, 
                                location.error = Ass2@data[["Location.Err"]],
                                window = 15, 
                                margin = 5)
# acoustic dBBMM
dBB.Aa2 <- brownian.bridge.dyn(V.Aa, 
                               ext = 0.5, 
                               raster = raster.A2, 
                               location.error = Aa@data[["Location.Err"]],
                               window = 49, 
                               margin = 15)
# acoustic subset dBBMM
dBB.Aas2 <- brownian.bridge.dyn(V.Aas2, 
                                ext = 0.5, 
                                raster = raster.A2, 
                                location.error = Aas2@data[["Location.Err"]],
                                window = 49, 
                                margin = 15)

# save dBBMM objects 
save(x=dBB.Aa2, file="Aa2")
save(x=dBB.As2, file="As2")
save(x=dBB.Ass2, file="Ass2")
save(x=dBB.Aas2, file="Aas2")

load(file="Aa2")
# Acoustic 95% 
UD.Aa2.95 <- raster2contour(dBB.Aa2, levels = 0.95) 
UD.Aa2.95 <- st_as_sf(UD.Aa2.95, coords = c("long", "lat"), crs = 3395)
UD.Aa2.95 <- st_cast(UD.Aa2.95, "POLYGON")
UD.Aa2.95 <- st_make_valid(UD.Aa2.95)
UD.Aa2.95 <- st_difference(UD.Aa2.95, land)
st_area(UD.Aa2.95) ## 5963834 [m^2]
# Acoustic 50% 
UD.Aa2.50 <- raster2contour(dBB.Aa2, levels = 0.50) 
UD.Aa2.50 <- st_as_sf(UD.Aa2.50, coords = c("long", "lat"), crs = 3395)
UD.Aa2.50 <- st_cast(UD.Aa2.50, "POLYGON") 
UD.Aa2.50 <- st_make_valid(UD.Aa2.50)
UD.Aa2.50 <- st_difference(UD.Aa2.50, land)
st_area(UD.Aa2.50) ## 463880.4 [m^2]

load(file = "As2")
# Satellite 95% 
UD.As2.95 <- raster2contour(dBB.As2, levels = 0.95) 
UD.As2.95 <- st_as_sf(UD.As2.95, coords = c("long", "lat"), crs = 3395)
UD.As2.95 <- st_cast(UD.As2.95, "POLYGON") 
UD.As2.95 <- st_make_valid(UD.As2.95)
UD.As2.95 <- st_difference(UD.As2.95, land)
st_area(UD.As2.95) ## 46069795 [m^2]
# Satellite 50% 
UD.As2.50 <- raster2contour(dBB.As2, levels = 0.50) 
UD.As2.50 <- st_as_sf(UD.As2.50, coords = c("long", "lat"), crs = 3395)
UD.As2.50 <- st_cast(UD.As2.50, "POLYGON") 
UD.As2.50 <- st_make_valid(UD.As2.50)
UD.As2.50 <- st_difference(UD.As2.50, land)
plot(UD.As2.50)
st_area(UD.As2.50) ## 3952112 [m^2]

load(file = "Ass2")
# Satellite Subset 95% 
UD.Ass2.95 <- raster2contour(dBB.Ass2, levels = 0.95) 
UD.Ass2.95 <- st_as_sf(UD.Ass2.95, coords = c("long", "lat"), crs = 3395)
UD.Ass2.95 <- st_cast(UD.Ass2.95, "POLYGON") 
UD.Ass2.95 <- st_make_valid(UD.Ass2.95)
UD.Ass2.95 <- st_difference(UD.Ass2.95, land)
st_area(UD.Ass2.95) ## 46069795 [m^2]
# Satellite Subset 50% 
UD.Ass2.50 <- raster2contour(dBB.Ass2, levels = 0.50) 
UD.Ass2.50 <- st_as_sf(UD.Ass2.50, coords = c("long", "lat"), crs = 3395)
UD.Ass2.50 <- st_cast(UD.Ass2.50, "POLYGON") 
UD.Ass2.50 <- st_make_valid(UD.Ass2.50)
UD.Ass2.50 <- st_difference(UD.Ass2.50, land)
plot(UD.Ass2.50)
st_area(UD.Ass2.50) ## 3952112 [m^2]

load(file = "Aas2")
# Acoustic Subset 95% 
UD.Aas2.95 <- raster2contour(dBB.Aas2, levels = 0.95) 
UD.Aas2.95 <- st_as_sf(UD.Aas2.95, coords = c("long", "lat"), crs = 3395) 
UD.Aas2.95 <- st_cast(UD.Aas2.95, "POLYGON") 
UD.Aas2.95 <- st_make_valid(UD.Aas2.95)
UD.Aas2.95 <- st_difference(UD.Aas2.95, land)
st_area(UD.Aas2.95) ## 6103467 [m^2]
# Acoustic Subset 50% 
UD.Aas2.50 <- raster2contour(dBB.Aas2, levels = 0.50) 
UD.Aas2.50 <- st_as_sf(UD.Aas2.50, coords = c("long", "lat"), crs = 3395)
UD.Aas2.50 <- st_cast(UD.Aas2.50, "POLYGON")
UD.Aas2.50 <- st_make_valid(UD.Aas2.50)
UD.Aas2.50 <- st_difference(UD.Aas2.50, land)
st_area(UD.Aas2.50) ## 402238.1 [m^2]

# Matching Temporal Duration UD plots
A_equal2 <- ggplot() +
  geom_sf(data = land, fill = "grey", col = "grey") +
  geom_sf(data = UD.Ass2.95, aes(fill = "deepskyblue1"), col = "deepskyblue1", lwd = 0.25, alpha = 0.3, show.legend = "polygon") +
  geom_sf(data = UD.Ass2.50, aes(fill = "blue2"), col = "blue2", lwd = 0.25, alpha = 0.5, show.legend = "polygon") +
  geom_sf(data = UD.Aas2.95, aes(fill = "violet"), col = "violet", lwd = 0.25, alpha = 0.7, show.legend = "polygon") +
  geom_sf(data = UD.Aas2.50, aes(fill = "violetred"), col = "violetred", lwd = 0.25, alpha = 0.5, show.legend = "polygon") +
  geom_point(data = recdata2, aes(x = lon, y = lat, col = "grey28"), col = "grey28", size = 0.8, show.legend = T) +
  coord_sf(crs = st_crs(3395), xlim = c(-8810937.70, - 8833201.59), ylim = c(2937305.74, 2955739.88)) + 
  scale_x_continuous(breaks = c(-79.30, -79.20)) +
  scale_y_continuous(breaks = c(25.70, 25.78)) +
  annotation_scale(style = "ticks", location = "br", text_cex = 0.85, line_width = 1, height = unit(0.35, "cm"), text_pad = unit(0.15, "cm"), pad_x = unit(0.35, "cm"), pad_y = unit(0.35, "cm")) + 
  theme_bw() +
  theme(text = element_text(size = 15)) +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5)) + 
  theme(axis.ticks.y = element_line(size = 0.5)) + 
  theme(axis.ticks.x = element_line(size = 0.5)) +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  annotate("text", x = -8833201.59, y = 2955739.88, label = "Turtle A", size = 6.5, hjust = 0, vjust = 1) + 
  scale_fill_identity(name = "Utilization Distributions", labels = c(deepskyblue1 = "Satellite 95%", blue2 = "Satellite 50%", violet = "Acoustic 95%", violetred = "Acoustic 50%"), limits = c("violet", "violetred", "deepskyblue1", "blue2"), guide = guide_legend(override.aes = list(linetype = c(rep("blank", 4)), shape = c(rep(NA, 4))))) + 
  scale_color_manual(values = c("grey28" = "grey28"), labels = c(grey28 = "Acoustic\nReceivers"), name = NULL,guide = guide_legend(override.aes = list(linetype = "blank", shape = 20, fill = NA, size = 3.5))) + 
  theme(legend.position = "bottom", 
        legend.text = element_text(size = 14), 
        legend.title = element_text(size = 14))
ggsave(filename = "A_equal2.tiff", plot = A_equal2, device = "tiff",path = "/Users/EmilyHardin/Desktop/Research/Acous.vs.Sat", width = 360, height = 120, units = c("mm"), dpi = 600)

# Full Temporal Duration UD plots
A_full2 <- ggplot() +
  geom_sf(data = land, fill = "grey", col = "grey") +
  geom_sf(data = UD.As2.95, fill = "deepskyblue1", col = "deepskyblue1", lwd = 0.4, alpha = 0.3) +
  geom_sf(data = UD.As2.50, fill = "blue2", col = "blue2", lwd = 0.4, alpha = 0.5) +
  geom_sf(data = UD.Aa2.95, fill = "violet", col = "violet", lwd = 0.4, alpha = 0.7) +
  geom_sf(data = UD.Aa2.50, fill = "violetred", col = "violetred", lwd = 0.4, alpha = 0.5) +
  geom_point(data = recdata2, aes(x = lon, y = lat), col = "grey28", size = 0.8) +
  coord_sf(crs = st_crs(3395), xlim = c(-8810937.70, - 8833201.59), ylim = c(2937305.74, 2955739.88)) + 
  scale_x_continuous(breaks = c(-79.30, -79.20)) +
  scale_y_continuous(breaks = c(25.70, 25.78)) +
  annotation_scale(style = "ticks", location = "br", text_cex = 0.85, line_width = 1, height = unit(0.35, "cm"), text_pad = unit(0.15, "cm"), pad_x = unit(0.35, "cm"), pad_y = unit(0.35, "cm")) + 
  theme_bw() +
  theme(text = element_text(size = 15)) +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5)) + 
  theme(axis.ticks.y = element_line(size = 0.5)) + 
  theme(axis.ticks.x = element_line(size = 0.5)) +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  annotate("text", x = -8833201.59, y = 2955739.88, label = "Turtle A", size = 6.5, hjust = 0, vjust = 1)
ggsave(filename = "A_full2.tiff", plot = A_full2, device = "tiff",path = "/Users/EmilyHardin/Desktop/Research/Acous.vs.Sat", width = 120, height = 120, units = c("mm"), dpi = 600)

A_equal22 <- ggplot() +
  geom_sf(data = land, fill = "grey", col = "grey") +
  geom_sf(data = UD.Ass2.95, fill = "deepskyblue1", col = "deepskyblue1", lwd = 0.4, alpha = 0.3) +
  geom_sf(data = UD.Ass2.50, fill = "blue2", col = "blue2", lwd = 0.4, alpha = 0.5) +
  geom_sf(data = UD.Aas2.95, fill = "violet", col = "violet", lwd = 0.4, alpha = 0.7) +
  geom_sf(data = UD.Aas2.50, fill = "violetred", col = "violetred", lwd = 0.4, alpha = 0.5) +
  geom_point(data = recdata2, aes(x = lon, y = lat), col = "grey28", size = 0.8) +
  coord_sf(crs = st_crs(3395), xlim = c(-8810937.70, - 8833201.59), ylim = c(2937305.74, 2955739.88)) + 
  scale_x_continuous(breaks = c(-79.30, -79.20)) +
  scale_y_continuous(breaks = c(25.70, 25.78)) +
  annotation_scale(style = "ticks", location = "br", text_cex = 0.85, line_width = 1, height = unit(0.35, "cm"), text_pad = unit(0.15, "cm"), pad_x = unit(0.35, "cm"), pad_y = unit(0.35, "cm")) + 
  theme_bw() +
  theme(text = element_text(size = 15)) +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5)) + 
  theme(axis.ticks.y = element_line(size = 0.5)) + 
  theme(axis.ticks.x = element_line(size = 0.5)) +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  annotate("text", x = -8833201.59, y = 2955739.88, label = "Turtle A", size = 6.5, hjust = 0, vjust = 1)
ggsave(filename = "A_equal22.tiff", plot = A_equal22, device = "tiff",path = "/Users/EmilyHardin/Desktop/Research/Acous.vs.Sat", width = 120, height = 120, units = c("mm"), dpi = 600)


## TURTLE C ##
extent(Cs2)
extent(Css2)
extent(Ca)
extent(Cas)
raster.C <- raster(
  xmn = -8849049, 
  xmx = -8803241, 
  ymn = 2921181, 
  ymx = 2963890, 
  crs = CRS("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"),
  resolution = 50)

registerDoParallel(detectCores() - 2) 
getDoParWorkers()

# variance 
V.Cs2 <- move::brownian.motion.variance.dyn(Cs2, 
                                            location.error = Cs2@data[["Location.Err"]], 
                                            window.size=15, 
                                            margin=5)
V.Cs2@interest[timeLag(Cs2,"hours") > 24] <- FALSE

V.Css2 <- move::brownian.motion.variance.dyn(Css2, 
                                             location.error = Css2@data[["Location.Err"]], 
                                             window.size=15, 
                                             margin=5)
V.Css2@interest[timeLag(Css2,"hours") > 24] <- FALSE

V.Ca <- move::brownian.motion.variance.dyn(Ca, 
                                           location.error = Ca@data[["Location.Err"]], 
                                           window.size=49, 
                                           margin=15)
V.Ca@interest[timeLag(Ca,"hours") > 24] <- FALSE

V.Cas2 <- move::brownian.motion.variance.dyn(Cas2, 
                                             location.error = Cas2@data[["Location.Err"]], 
                                             window.size=49, 
                                             margin=15)
V.Cas2@interest[timeLag(Cas2,"hours") > 24] <- FALSE

# dBBMMs
dBB.Cs2 <- brownian.bridge.dyn(V.Cs2, 
                               ext = 0.5, 
                               raster = raster.C, 
                               location.error = Cs2@data[["Location.Err"]],
                               window = 15, 
                               margin = 5)
dBB.Css2 <- brownian.bridge.dyn(V.Css2, 
                                ext = 0.5, 
                                raster = raster.C, 
                                location.error = Css2@data[["Location.Err"]],
                                window = 15, 
                                margin = 5)
dBB.Ca2 <- brownian.bridge.dyn(V.Ca, 
                               ext = 0.5, 
                               raster = raster.C, 
                               location.error = Ca@data[["Location.Err"]],
                               window = 49, 
                               margin = 15)
dBB.Cas2 <- brownian.bridge.dyn(V.Cas2, 
                                ext = 0.5, 
                                raster = raster.C, 
                                location.error = Cas2@data[["Location.Err"]],
                                window = 49, 
                                margin = 15)
save(x=dBB.Ca2, file="Ca2")
save(x=dBB.Cs2, file="Cs2")
save(x=dBB.Css2, file="Css2")
save(x=dBB.Cas2, file="Cas2")


load(file="Ca2")
# Acoustic 95% 
UD.Ca2.95 <- raster2contour(dBB.Ca2, levels = 0.95) 
UD.Ca2.95 <- st_as_sf(UD.Ca2.95, coords = c("long", "lat"), crs = 3395)
UD.Ca2.95 <- st_cast(UD.Ca2.95, "POLYGON") 
UD.Ca2.95 <- st_make_valid(UD.Ca2.95)
UD.Ca2.95 <- st_difference(UD.Ca2.95, land)
st_area(UD.Ca2.95) ## 2844985
# Acoustic 50% 
UD.Ca2.50 <- raster2contour(dBB.Ca2, levels = 0.50) 
UD.Ca2.50 <- st_as_sf(UD.Ca2.50, coords = c("long", "lat"), crs = 3395)
UD.Ca2.50 <- st_cast(UD.Ca2.50, "POLYGON") 
UD.Ca2.50 <- st_make_valid(UD.Ca2.50)
UD.Ca2.50 <- st_difference(UD.Ca2.50, land)
st_area(UD.Ca2.50) ## 275295.8 

load(file = "Cs2")
# Satellite 95% 
UD.Cs2.95 <- raster2contour(dBB.Cs2, levels = 0.95) 
UD.Cs2.95 <- st_as_sf(UD.Cs2.95, coords = c("long", "lat"), crs = 3395) 
UD.Cs2.95 <- st_cast(UD.Cs2.95, "POLYGON") 
UD.Cs2.95 <- st_make_valid(UD.Cs2.95)
UD.Cs2.95 <- st_difference(UD.Cs2.95, land)
st_area(UD.Cs2.95) ## 906760.5
# Satellite 50% 
UD.Cs2.50 <- raster2contour(dBB.Cs2, levels = 0.50) 
UD.Cs2.50 <- st_as_sf(UD.Cs2.50, coords = c("long", "lat"), crs = 3395)
UD.Cs2.50 <- st_cast(UD.Cs2.50, "POLYGON") 
UD.Cs2.50 <- st_make_valid(UD.Cs2.50)
UD.Cs2.50 <- st_difference(UD.Cs2.50, land)
st_area(UD.Cs2.50) ## 72352.13

load(file = "Css2")
# Satellite Subset 95% 
UD.Css2.95 <- raster2contour(dBB.Css2, levels = 0.95) 
UD.Css2.95 <- st_as_sf(UD.Css2.95, coords = c("long", "lat"), crs = 3395) 
UD.Css2.95 <- st_cast(UD.Css2.95, "POLYGON") 
UD.Css2.95 <- st_make_valid(UD.Css2.95)
UD.Css2.95 <- st_difference(UD.Css2.95, land)
st_area(UD.Css2.95) ## 749949.6
# Satellite Subset 50% 
UD.Css2.50 <- raster2contour(dBB.Css2, levels = 0.50) 
UD.Css2.50 <- st_as_sf(UD.Css2.50, coords = c("long", "lat"), crs = 3395)
UD.Css2.50 <- st_cast(UD.Css2.50, "POLYGON") 
UD.Css2.50 <- st_make_valid(UD.Css2.50)
UD.Css2.50 <- st_difference(UD.Css2.50, land)
st_area(UD.Css2.50) ## 20731.39 

load(file = "Cas2")
# Acoustic Subset 95% 
UD.Cas2.95 <- raster2contour(dBB.Cas2, levels = 0.95) 
UD.Cas2.95 <- st_as_sf(UD.Cas2.95, coords = c("long", "lat"), crs = 3395) 
UD.Cas2.95 <- st_cast(UD.Cas2.95, "POLYGON") 
UD.Cas2.95 <- st_make_valid(UD.Cas2.95)
UD.Cas2.95 <- st_difference(UD.Cas2.95, land)
st_area(UD.Cas2.95) ## 1350145
# Acoustic Subset 50% 
UD.Cas2.50 <- raster2contour(dBB.Cas2, levels = 0.50) 
UD.Cas2.50 <- st_as_sf(UD.Cas2.50, coords = c("long", "lat"), crs = 3395)
UD.Cas2.50 <- st_cast(UD.Cas2.50, "POLYGON") 
UD.Cas2.50 <- st_make_valid(UD.Cas2.50)
UD.Cas2.50 <- st_difference(UD.Cas2.50, land)
st_area(UD.Cas2.50) ## 226665.5 

# Matching Temporal Duration UD plot
C_equal2<- ggplot() +
  geom_sf(data = land, fill = "grey", col = "grey") +
  geom_sf(data = UD.Cas2.95, fill = "violet", col = "violet", lwd = 0.4, alpha = 0.7) +
  geom_sf(data = UD.Css2.95, fill = "deepskyblue1", col = "deepskyblue1", lwd = 0.4, alpha = 0.3) +
  geom_sf(data = UD.Cas2.50, fill = "violetred", col = "violetred", lwd = 0.4, alpha = 0.5) +
  geom_sf(data = UD.Css2.50, fill = "blue2", col = "blue2", lwd = 0.4, alpha = 0.5) +
  geom_point(data = recdata2, aes(x = lon, y = lat), col = "grey28", size = 0.8) +
  coord_sf(crs = st_crs(3395), xlim = c(-8810937.70, - 8833201.59), ylim = c(2937305.74, 2955739.88)) + 
  scale_x_continuous(breaks = c(-79.30, -79.20)) + 
  scale_y_continuous(breaks = c(25.70, 25.78)) + 
  annotation_scale(style = "ticks", location = "br", text_cex = 0.85, line_width = 1, height = unit(0.35, "cm"), text_pad = unit(0.15, "cm"), pad_x = unit(0.35, "cm"), pad_y = unit(0.35, "cm")) + 
  theme_bw() +
  theme(text = element_text(size = 15)) +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5)) + 
  theme(axis.ticks.y = element_line(size = 0.5)) + 
  theme(axis.ticks.x = element_line(size = 0.5)) +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  annotate("text", x = -8833201.59, y = 2955739.88, label = "Turtle C", size = 6.5, hjust = 0, vjust = 1)
ggsave(filename = "C_equal2.tiff", plot = C_equal2, device = "tiff",path = "/Users/EmilyHardin/Desktop/Research/Acous.vs.Sat", width = 120, height = 120, units = c("mm"), dpi = 600)

# Full Temporal Duration UD plot
C_full2 <- ggplot() +
  geom_sf(data = land, fill = "grey", col = "grey") +
  geom_sf(data = UD.Ca2.95, fill = "violet", col = "violet", lwd = 0.4, alpha = 0.7) +
  geom_sf(data = UD.Cs2.95, fill = "deepskyblue1", col = "deepskyblue1", lwd = 0.4, alpha = 0.3) +
  geom_sf(data = UD.Ca2.50, fill = "violetred", col = "violetred", lwd = 0.4, alpha = 0.5) +
  geom_sf(data = UD.Cs2.50, fill = "blue2", col = "blue2", lwd = 0.4, alpha = 0.5) +
  geom_point(data = recdata2, aes(x = lon, y = lat), col = "grey28", size = 0.8) +
  coord_sf(crs = st_crs(3395), xlim = c(-8810937.70, - 8833201.59), ylim = c(2937305.74, 2955739.88)) + 
  scale_x_continuous(breaks = c(-79.30, -79.20)) + 
  scale_y_continuous(breaks = c(25.70, 25.78)) + 
  annotation_scale(style = "ticks", location = "br", text_cex = 0.85, line_width = 1, height = unit(0.35, "cm"), text_pad = unit(0.15, "cm"), pad_x = unit(0.35, "cm"), pad_y = unit(0.35, "cm")) + 
  theme_bw() +
  theme(text = element_text(size = 15)) +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5)) + 
  theme(axis.ticks.y = element_line(size = 0.5)) + 
  theme(axis.ticks.x = element_line(size = 0.5)) +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  annotate("text", x = -8833201.59, y = 2955739.88, label = "Turtle C", size = 6.5, hjust = 0, vjust = 1)
ggsave(filename = "C_full2.tiff", plot = C_full2, device = "tiff",path = "/Users/EmilyHardin/Desktop/Research/Acous.vs.Sat", width = 120, height = 120, units = c("mm"), dpi = 600)

## TURTLE D ##
extent(Ds2)
extent(Dss2)
extent(Da)
extent(Das)
raster.D2 <- raster(
  xmn = -8849101, 
  xmx = -8798970, 
  ymn = 2923448, 
  ymx = 2968556, 
  crs = CRS("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"),
  resolution = 50)

registerDoParallel(detectCores() - 2) 
getDoParWorkers()

# variance
V.Ds2 <- move::brownian.motion.variance.dyn(Ds2, 
                                            location.error = Ds2@data[["Location.Err"]], 
                                            window.size=15, 
                                            margin=5)
V.Ds2@interest[timeLag(Ds2,"hours") > 24] <- FALSE

V.Dss2 <- move::brownian.motion.variance.dyn(Dss2, 
                                             location.error = Dss2@data[["Location.Err"]], 
                                             window.size=15, 
                                             margin=5)
V.Dss2@interest[timeLag(Dss2,"hours") > 24] <- FALSE

V.Da <- move::brownian.motion.variance.dyn(Da, 
                                           location.error = Da@data[["Location.Err"]], 
                                           window.size=49, 
                                           margin=15)
V.Da@interest[timeLag(Da,"hours") > 24] <- FALSE

V.Das2 <- move::brownian.motion.variance.dyn(Das2, 
                                             location.error = Das2@data[["Location.Err"]], 
                                             window.size=49, 
                                             margin=15)
V.Das2@interest[timeLag(Das2,"hours") > 24] <- FALSE

# dBBMMs
dBB.Ds2 <- brownian.bridge.dyn(V.Ds2, 
                               ext = 0.5, 
                               raster = raster.D2, 
                               location.error = Ds2@data[["Location.Err"]],
                               window = 15, 
                               margin = 5)
dBB.Dss2 <- brownian.bridge.dyn(V.Dss2, 
                                ext = 0.5, 
                                raster = raster.D2, 
                                location.error = Dss2@data[["Location.Err"]],
                                window = 15, 
                                margin = 5)
dBB.Da2 <- brownian.bridge.dyn(V.Da, 
                               ext = 0.5, 
                               raster = raster.D2, 
                               location.error = Da@data[["Location.Err"]],
                               window = 49, 
                               margin = 15)
dBB.Das2 <- brownian.bridge.dyn(V.Das2, 
                                ext = 0.5, 
                                raster = raster.D2, 
                                location.error = Das2@data[["Location.Err"]],
                                window = 49, 
                                margin = 15)
save(x=dBB.Da2, file="Da2")
save(x=dBB.Ds2, file="Ds2")
save(x=dBB.Dss2, file="Dss2")
save(x=dBB.Das2, file="Das2")


load(file="Da2")
# Acoustic 95% 
UD.Da2.95 <- raster2contour(dBB.Da2, levels = 0.95) 
UD.Da2.95 <- st_as_sf(UD.Da2.95, coords = c("long", "lat"), crs = 3395) 
UD.Da2.95 <- st_cast(UD.Da2.95, "POLYGON") 
UD.Da2.95 <- st_make_valid(UD.Da2.95)
UD.Da2.95 <- st_difference(UD.Da2.95, land)
st_area(UD.Da2.95) ## 1898775
# Acoustic 50% 
UD.Da2.50 <- raster2contour(dBB.Da2, levels = 0.50) 
UD.Da2.50 <- st_as_sf(UD.Da2.50, coords = c("long", "lat"), crs = 3395)
UD.Da2.50 <- st_cast(UD.Da2.50, "POLYGON") 
UD.Da2.50 <- st_make_valid(UD.Da2.50)
UD.Da2.50 <- st_difference(UD.Da2.50, land)
st_area(UD.Da2.50) ## 239997.5 

load(file = "Ds2")
# Satellite 95% 
UD.Ds2.95 <- raster2contour(dBB.Ds2, levels = 0.95) 
UD.Ds2.95 <- st_as_sf(UD.Ds2.95, coords = c("long", "lat"), crs = 3395) 
UD.Ds2.95 <- st_cast(UD.Ds2.95, "POLYGON") 
UD.Ds2.95 <- st_make_valid(UD.Ds2.95)
UD.Ds2.95 <- st_difference(UD.Ds2.95, land)
st_area(UD.Ds2.95) ## 12315373
# Satellite 50% 
UD.Ds2.50 <- raster2contour(dBB.Ds2, levels = 0.50) 
UD.Ds2.50 <- st_as_sf(UD.Ds2.50, coords = c("long", "lat"), crs = 3395)
UD.Ds2.50 <- st_cast(UD.Ds2.50, "POLYGON") 
UD.Ds2.50 <- st_make_valid(UD.Ds2.50)
UD.Ds2.50 <- st_difference(UD.Ds2.50, land)
st_area(UD.Ds2.50) ## 1959905

load(file = "Dss2")
# Satellite Subset 95% 
UD.Dss2.95 <- raster2contour(dBB.Dss2, levels = 0.95) 
UD.Dss2.95 <- st_as_sf(UD.Dss2.95, coords = c("long", "lat"), crs = 3395) 
UD.Dss2.95 <- st_cast(UD.Dss2.95, "POLYGON") 
UD.Dss2.95 <- st_make_valid(UD.Dss2.95)
UD.Dss2.95 <- st_difference(UD.Dss2.95, land)
st_area(UD.Dss2.95) ## 13129745
# Satellite Subset 50% 
UD.Dss2.50 <- raster2contour(dBB.Dss2, levels = 0.50) 
UD.Dss2.50 <- st_as_sf(UD.Dss2.50, coords = c("long", "lat"), crs = 3395)
UD.Dss2.50 <- st_cast(UD.Dss2.50, "POLYGON") 
UD.Dss2.50 <- st_make_valid(UD.Dss2.50)
UD.Dss2.50 <- st_difference(UD.Dss2.50, land)
st_area(UD.Dss2.50) ## 2056484

load(file = "Das2")
# Acoustic Subset 95% 
UD.Das2.95 <- raster2contour(dBB.Das2, levels = 0.95) 
UD.Das2.95 <- st_as_sf(UD.Das2.95, coords = c("long", "lat"), crs = 3395) 
UD.Das2.95 <- st_cast(UD.Das2.95, "POLYGON")
UD.Das2.95 <- st_make_valid(UD.Das2.95)
UD.Das2.95 <- st_difference(UD.Das2.95, land)
st_area(UD.Das2.95) ## 2097217
# Acoustic Subset 50% 
UD.Das2.50 <- raster2contour(dBB.Das2, levels = 0.50) 
UD.Das2.50 <- st_as_sf(UD.Das2.50, coords = c("long", "lat"), crs = 3395)
UD.Das2.50 <- st_cast(UD.Das2.50, "POLYGON") 
UD.Das2.50 <- st_make_valid(UD.Das2.50)
UD.Das2.50 <- st_difference(UD.Das2.50, land)
st_area(UD.Das2.50) ## 260501.1

# Matching Temporal Duration UD plot
D_equal2 <- ggplot() +
  geom_sf(data = land, fill = "grey", col = "grey") +
  geom_sf(data = UD.Dss2.95, fill = "deepskyblue1", col = "deepskyblue1", lwd = 0.4, alpha = 0.3) +
  geom_sf(data = UD.Dss2.50, fill = "blue2", col = "blue2", lwd = 0.4, alpha = 0.5) +
  geom_sf(data = UD.Das2.95, fill = "violet", col = "violet", lwd = 0.4, alpha = 0.7) +
  geom_sf(data = UD.Das2.50, fill = "violetred", col = "violetred", lwd = 0.4, alpha = 0.5) +
  geom_point(data = recdata2, aes(x = lon, y = lat), col = "grey28", size = 0.8) +
  coord_sf(crs = st_crs(3395), xlim = c(-8810937.70, - 8833201.59), ylim = c(2937305.74, 2955739.88)) + 
  scale_x_continuous(breaks = c(-79.30, -79.20)) + 
  scale_y_continuous(breaks = c(25.70, 25.78)) + 
  annotation_scale(style = "ticks", location = "br", text_cex = 0.85, line_width = 1, height = unit(0.35, "cm"), text_pad = unit(0.15, "cm"), pad_x = unit(0.35, "cm"), pad_y = unit(0.35, "cm")) + 
  theme_bw() +
  theme(text = element_text(size = 15)) +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5)) + 
  theme(axis.ticks.y = element_line(size = 0.5)) + 
  theme(axis.ticks.x = element_line(size = 0.5)) +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  annotate("text", x = -8833201.59, y = 2955739.88, label = "Turtle D", size = 6.5, hjust = 0, vjust = 1)
ggsave(filename = "D_equa2l.tiff", plot = D_equal2, device = "tiff",path = "/Users/EmilyHardin/Desktop/Research/Acous.vs.Sat", width = 120, height = 120, units = c("mm"), dpi = 600)

# Full Temporal Duration UD plot
D_full2 <- ggplot() +
  geom_sf(data = land, fill = "grey", col = "grey") +
  geom_sf(data = UD.Ds2.95, fill = "deepskyblue1", col = "deepskyblue1", lwd = 0.4, alpha = 0.3) +
  geom_sf(data = UD.Ds2.50, fill = "blue2", col = "blue2", lwd = 0.4, alpha = 0.5) +
  geom_sf(data = UD.Da2.95, fill = "violet", col = "violet", lwd = 0.4, alpha = 0.7) +
  geom_sf(data = UD.Da2.50, fill = "violetred", col = "violetred", lwd = 0.4, alpha = 0.5) +
  geom_point(data = recdata2, aes(x = lon, y = lat), col = "grey28", size = 0.8) +
  coord_sf(crs = st_crs(3395), xlim = c(-8810937.70, - 8833201.59), ylim = c(2937305.74, 2955739.88)) + 
  scale_x_continuous(breaks = c(-79.30, -79.20)) + 
  scale_y_continuous(breaks = c(25.70, 25.78)) + 
  annotation_scale(style = "ticks", location = "br", text_cex = 0.85, line_width = 1, height = unit(0.35, "cm"), text_pad = unit(0.15, "cm"), pad_x = unit(0.35, "cm"), pad_y = unit(0.35, "cm")) + 
  theme_bw() +
  theme(text = element_text(size = 15)) +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5)) + 
  theme(axis.ticks.y = element_line(size = 0.5)) + 
  theme(axis.ticks.x = element_line(size = 0.5)) +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  annotate("text", x = -8833201.59, y = 2955739.88, label = "Turtle D", size = 6.5, hjust = 0, vjust = 1)
ggsave(filename = "D_full2.tiff", plot = D_full2, device = "tiff",path = "/Users/EmilyHardin/Desktop/Research/Acous.vs.Sat", width = 120, height = 120, units = c("mm"), dpi = 600)


## TURTLE G ##

extent(Gs2)
extent(Gss2)
extent(Ga)
raster.G2 <- raster(
  xmn = -8848608, 
  xmx = -8799662, 
  ymn = 2919116, 
  ymx = 2965853, 
  crs = CRS("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"),
  resolution = 50)

# variance
V.Gs2 <- move::brownian.motion.variance.dyn(Gs2, 
                                            location.error = Gs2@data[["Location.Err"]], 
                                            window.size=15, 
                                            margin=5)
V.Gs2@interest[timeLag(Gs2,"hours") > 24] <- FALSE

V.Gss2 <- move::brownian.motion.variance.dyn(Gss2, 
                                             location.error = Gss2@data[["Location.Err"]], 
                                             window.size=15, 
                                             margin=5)
V.Gss2@interest[timeLag(Gss2,"hours") > 24] <- FALSE

V.Ga <- move::brownian.motion.variance.dyn(Ga, 
                                           location.error = Ga@data[["Location.Err"]], 
                                           window.size=49, 
                                           margin=15)
V.Ga@interest[timeLag(Ga,"hours") > 24] <- FALSE

registerDoParallel(detectCores() - 2) 
getDoParWorkers()

# dBBMMs
dBB.Gs2 <- brownian.bridge.dyn(V.Gs2, 
                               ext = 0.5, 
                               raster = raster.G2, 
                               location.error = Gs2@data[["Location.Err"]],
                               window = 15, 
                               margin = 5)
dBB.Gss2 <- brownian.bridge.dyn(V.Gss2, 
                                ext = 0.5, 
                                raster = raster.G2, 
                                location.error = Gss2@data[["Location.Err"]],
                                window = 15, 
                                margin = 5)
dBB.Ga2 <- brownian.bridge.dyn(V.Ga, 
                               ext = 0.5, 
                               raster = raster.G2, 
                               location.error = Ga@data[["Location.Err"]],
                               window = 49, 
                               margin = 15)
save(x=dBB.Ga2, file="Ga2")
save(x=dBB.Gs2, file="Gs2")
save(x=dBB.Gss2, file="Gss2")


load(file="Ga2")
# Acoustic 95% 
UD.Ga2.95 <- raster2contour(dBB.Ga2, levels = 0.95) 
UD.Ga2.95 <- st_as_sf(UD.Ga2.95, coords = c("long", "lat"), crs = 3395) 
UD.Ga2.95 <- st_cast(UD.Ga2.95, "POLYGON") 
UD.Ga2.95 <- st_make_valid(UD.Ga2.95)
UD.Ga2.95 <- st_difference(UD.Ga2.95, land)
st_area(UD.Ga2.95) ## 1566981
# Acoustic 50% 
UD.Ga2.50 <- raster2contour(dBB.Ga2, levels = 0.50) 
UD.Ga2.50 <- st_as_sf(UD.Ga2.50, coords = c("long", "lat"), crs = 3395)
UD.Ga2.50 <- st_cast(UD.Ga2.50, "POLYGON") 
UD.Ga2.50 <- st_make_valid(UD.Ga2.50)
UD.Ga2.50 <- st_difference(UD.Ga2.50, land)
st_area(UD.Ga2.50) ## 236289.6 

load(file = "Gs2")
# Satellite 95% 
UD.Gs2.95 <- raster2contour(dBB.Gs2, levels = 0.95) 
UD.Gs2.95 <- st_as_sf(UD.Gs2.95, coords = c("long", "lat"), crs = 3395) 
UD.Gs2.95 <- st_cast(UD.Gs2.95, "POLYGON") 
UD.Gs2.95 <- st_make_valid(UD.Gs2.95)
UD.Gs2.95 <- st_difference(UD.Gs2.95, land)
st_area(UD.Gs2.95) ## 10118881
# Satellite 50% 
UD.Gs2.50 <- raster2contour(dBB.Gs2, levels = 0.50) 
UD.Gs2.50 <- st_as_sf(UD.Gs2.50, coords = c("long", "lat"), crs = 3395)
UD.Gs2.50 <- st_cast(UD.Gs2.50, "POLYGON")  
UD.Gs2.50 <- st_make_valid(UD.Gs2.50)
UD.Gs2.50 <- st_difference(UD.Gs2.50, land)
st_area(UD.Gs2.50) ## 784746.9

load(file = "Gss2")
# Satellite Subset 95% 
UD.Gss2.95 <- raster2contour(dBB.Gss2, levels = 0.95) 
UD.Gss2.95 <- st_as_sf(UD.Gss2.95, coords = c("long", "lat"), crs = 3395) 
UD.Gss2.95 <- st_cast(UD.Gss2.95, "POLYGON")
UD.Gss2.95 <- st_make_valid(UD.Gss2.95)
UD.Gss2.95 <- st_difference(UD.Gss2.95, land)
st_area(UD.Gss2.95) ## 12338136
# Satellite Subset 50% 
UD.Gss2.50 <- raster2contour(dBB.Gss2, levels = 0.50) 
UD.Gss2.50 <- st_as_sf(UD.Gss2.50, coords = c("long", "lat"), crs = 3395)
UD.Gss2.50 <- st_cast(UD.Gss2.50, "POLYGON") 
UD.Gss2.50 <- st_make_valid(UD.Gss2.50)
UD.Gss2.50 <- st_difference(UD.Gss2.50, land)
st_area(UD.Gss2.50) ## 1183044

# Matching Temporal Duration UD plot
G_equal2 <- ggplot() +
  geom_sf(data = land, fill = "grey", col = "grey") +
  geom_sf(data = UD.Gss2.95, fill = "deepskyblue1", col = "deepskyblue1", lwd = 0.4, alpha = 0.3) +
  geom_sf(data = UD.Gss2.50, fill = "blue2", col = "blue2", lwd = 0.4, alpha = 0.5) +
  geom_sf(data = UD.Ga2.95, fill = "violet", col = "violet", lwd = 0.4, alpha = 0.7) +
  geom_sf(data = UD.Ga2.50, fill = "violetred", col = "violetred", lwd = 0.4, alpha = 0.5) +
  geom_point(data = recdata2, aes(x = lon, y = lat), col = "grey28", size = 0.8) +
  coord_sf(crs = st_crs(3395), xlim = c(-8810937.70, - 8833201.59), ylim = c(2937305.74, 2955739.88)) + 
  scale_x_continuous(breaks = c(-79.30, -79.20)) + 
  scale_y_continuous(breaks = c(25.70, 25.78)) +
  annotation_scale(style = "ticks", location = "br", text_cex = 0.85, line_width = 1, height = unit(0.35, "cm"), text_pad = unit(0.15, "cm"), pad_x = unit(0.35, "cm"), pad_y = unit(0.35, "cm")) + 
  theme_bw() +
  theme(text = element_text(size = 15)) +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5)) + 
  theme(axis.ticks.y = element_line(size = 0.5)) + 
  theme(axis.ticks.x = element_line(size = 0.5)) +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  annotate("text", x = -8833201.59, y = 2955739.88, label = "Turtle G", size = 6.5, hjust = 0, vjust = 1)
ggsave(filename = "G_equal2.tiff", plot = G_equal2, device = "tiff",path = "/Users/EmilyHardin/Desktop/Research/Acous.vs.Sat", width = 120, height = 120, units = c("mm"), dpi = 600)

# Full Temporal Duration UD plot
G_full2 <- ggplot() +
  geom_sf(data = land, fill = "grey", col = "grey") +
  geom_sf(data = UD.Gs2.95, fill = "deepskyblue1", col = "deepskyblue1", lwd = 0.4, alpha = 0.3) +
  geom_sf(data = UD.Gs2.50, fill = "blue2", col = "blue2", lwd = 0.4, alpha = 0.5) +
  geom_sf(data = UD.Ga2.95, fill = "violet", col = "violet", lwd = 0.4, alpha = 0.7) +
  geom_sf(data = UD.Ga2.50, fill = "violetred", col = "violetred", lwd = 0.4, alpha = 0.5) +
  geom_point(data = recdata2, aes(x = lon, y = lat), col = "grey28", size = 0.8) +
  coord_sf(crs = st_crs(3395), xlim = c(-8810937.70, - 8833201.59), ylim = c(2937305.74, 2955739.88)) + 
  scale_x_continuous(breaks = c(-79.30, -79.20)) + 
  scale_y_continuous(breaks = c(25.70, 25.78)) +
  annotation_scale(style = "ticks", location = "br", text_cex = 0.85, line_width = 1, height = unit(0.35, "cm"), text_pad = unit(0.15, "cm"), pad_x = unit(0.35, "cm"), pad_y = unit(0.35, "cm")) + 
  theme_bw() +
  theme(text = element_text(size = 15)) +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5)) + 
  theme(axis.ticks.y = element_line(size = 0.5)) + 
  theme(axis.ticks.x = element_line(size = 0.5)) +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  annotate("text", x = -8833201.59, y = 2955739.88, label = "Turtle G", size = 6.5, hjust = 0, vjust = 1)
ggsave(filename = "G_full2.tiff", plot = G_full2, device = "tiff",path = "/Users/EmilyHardin/Desktop/Research/Acous.vs.Sat", width = 120, height = 120, units = c("mm"), dpi = 600)

## TURTLE I ##
extent(Is2)
extent(Iss2)
extent(Ia)
raster.I2 <- raster(
  xmn = -8849542, 
  xmx = -8797184, 
  ymn = 2919669, 
  ymx = 2970783, 
  crs = CRS("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"),
  resolution = 50)

# variance
V.Is2 <- move::brownian.motion.variance.dyn(Is2, 
                                            location.error = Is2@data[["Location.Err"]], 
                                            window.size=15, 
                                            margin=5)
V.Is2@interest[timeLag(Is2,"hours") > 24] <- FALSE

V.Iss2 <- move::brownian.motion.variance.dyn(Iss2, 
                                             location.error = Iss2@data[["Location.Err"]], 
                                             window.size=15, 
                                             margin=5)
V.Iss2@interest[timeLag(Iss2,"hours") > 24] <- FALSE

V.Ia <- move::brownian.motion.variance.dyn(Ia, 
                                           location.error = Ia@data[["Location.Err"]], 
                                           window.size=49, 
                                           margin=15)
V.Ia@interest[timeLag(Ia,"hours") > 24] <- FALSE

registerDoParallel(detectCores() - 2) 
getDoParWorkers()

# dBBMMs
dBB.Is2 <- brownian.bridge.dyn(V.Is2, 
                               ext = 0.5, 
                               raster = raster.I2, 
                               location.error = Is2@data[["Location.Err"]],
                               window = 15, 
                               margin = 5)
dBB.Iss2 <- brownian.bridge.dyn(V.Iss2, 
                                ext = 0.5, 
                                raster = raster.I2, 
                                location.error = Iss2@data[["Location.Err"]],
                                window = 15, 
                                margin = 5)
dBB.Ia2 <- brownian.bridge.dyn(V.Ia, 
                               ext = 0.5, 
                               raster = raster.I2, 
                               location.error = Ia@data[["Location.Err"]],
                               window = 49, 
                               margin = 15)
save(x=dBB.Ia2, file="Ia2")
save(x=dBB.Is2, file="Is2")
save(x=dBB.Iss2, file="Iss2")

load(file="Ia2")
# Acoustic 95% 
UD.Ia2.95 <- raster2contour(dBB.Ia2, levels = 0.95) 
UD.Ia2.95 <- st_as_sf(UD.Ia2.95, coords = c("long", "lat"), crs = 3395) 
UD.Ia2.95 <- st_cast(UD.Ia2.95, "POLYGON") 
UD.Ia2.95 <- st_make_valid(UD.Ia2.95)
UD.Ia2.95 <- st_difference(UD.Ia2.95, land)
st_area(UD.Ia2.95) ##  4797669
# Acoustic 50% 
UD.Ia2.50 <- raster2contour(dBB.Ia2, levels = 0.50) 
UD.Ia2.50 <- st_as_sf(UD.Ia2.50, coords = c("long", "lat"), crs = 3395)
UD.Ia2.50 <- st_cast(UD.Ia2.50, "POLYGON") 
UD.Ia2.50 <- st_make_valid(UD.Ia2.50)
UD.Ia2.50 <- st_difference(UD.Ia2.50, land)
st_area(UD.Ia2.50) ## 398288.6

load(file = "Is2")
# Satellite 95% 
UD.Is2.95 <- raster2contour(dBB.Is2, levels = 0.95) 
UD.Is2.95 <- st_as_sf(UD.Is2.95, coords = c("long", "lat"), crs = 3395) 
UD.Is2.95 <- st_cast(UD.Is2.95, "POLYGON") 
UD.Is2.95 <- st_make_valid(UD.Is2.95)
UD.Is2.95 <- st_difference(UD.Is2.95, land)
st_area(UD.Is2.95) ## 30673734
# Satellite 50% 
UD.Is2.50 <- raster2contour(dBB.Is2, levels = 0.50) 
UD.Is2.50 <- st_as_sf(UD.Is2.50, coords = c("long", "lat"), crs = 3395)
UD.Is2.50 <- st_cast(UD.Is2.50, "POLYGON") 
UD.Is2.50 <- st_make_valid(UD.Is2.50)
UD.Is2.50 <- st_difference(UD.Is2.50, land)
st_area(UD.Is2.50) ## 1185566

load(file = "Iss2")
# Satellite Subset 95% 
UD.Iss2.95 <- raster2contour(dBB.Iss2, levels = 0.95) 
UD.Iss2.95 <- st_as_sf(UD.Iss2.95, coords = c("long", "lat"), crs = 3395) 
UD.Iss2.95 <- st_cast(UD.Iss2.95, "POLYGON") 
UD.Iss2.95 <- st_make_valid(UD.Iss2.95)
UD.Iss2.95 <- st_difference(UD.Iss2.95, land)
st_area(UD.Iss2.95) ## 45165697
# Satellite Subset 50% 
UD.Iss2.50 <- raster2contour(dBB.Iss2, levels = 0.50) 
UD.Iss2.50 <- st_as_sf(UD.Iss2.50, coords = c("long", "lat"), crs = 3395)
UD.Iss2.50 <- st_cast(UD.Iss2.50, "POLYGON") 
UD.Iss2.50 <- st_make_valid(UD.Iss2.50)
UD.Iss2.50 <- st_difference(UD.Iss2.50, land)
st_area(UD.Iss2.50) ## 9263020

# Matching Temporal Duration UD plot
I_equal2 <- ggplot() +
  geom_sf(data = land, fill = "grey", col = "grey") +
  geom_sf(data = UD.Iss2.95, fill = "deepskyblue1", col = "deepskyblue1", lwd = 0.4, alpha = 0.3, aes(fill = "")) +
  geom_sf(data = UD.Iss2.50, fill = "blue2", col = "blue2", lwd = 0.4, alpha = 0.5, aes(fill = "")) +
  geom_sf(data = UD.Ia2.95, fill = "violet", col = "violet", lwd = 0.4, alpha = 0.7, aes(fill = "")) +
  geom_sf(data = UD.Ia2.50, fill = "violetred", col = "violetred", lwd = 0.4, alpha = 0.5, aes(fill = "")) +
  geom_point(data = recdata2, aes(x = lon, y = lat), col = "grey28", size = 0.8) +
  coord_sf(crs = st_crs(3395), xlim = c(-8849899.51, -8794239.77), ylim = c(2910311.29, 2956969.66)) + 
  scale_x_continuous(breaks = c(-79.4, -79.2)) +
  scale_y_continuous(breaks = c(25.45, 25.6, 25.75)) +
  annotation_scale(style = "ticks", location = "br", text_cex = 0.85, line_width = 1, height = unit(0.35, "cm"), text_pad = unit(0.15, "cm"), pad_x = unit(0.35, "cm"), pad_y = unit(0.35, "cm")) + 
  theme_bw() +
  theme(text = element_text(size = 15)) +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5)) + 
  theme(axis.ticks.y = element_line(size = 0.5)) + 
  theme(axis.ticks.x = element_line(size = 0.5)) +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  annotate("text", x = -8849899.51, y = 2956969.66, label = "Turtle I", size = 6.5, hjust = 0, vjust = 1)
ggsave(filename = "I_equal2.tiff", plot = I_equal2, device = "tiff",path = "/Users/EmilyHardin/Desktop/Research/Acous.vs.Sat", width = 120, height = 120, units = c("mm"), dpi = 600)

# Full Temporal Duration UD plot 
I_full2 <- ggplot() +
  geom_sf(data = land, fill = "grey", col = "grey") +
  geom_sf(data = UD.Is2.95, fill = "deepskyblue1", col = "deepskyblue1", lwd = 0.4, alpha = 0.3) +
  geom_sf(data = UD.Is2.50, fill = "blue2", col = "blue2", lwd = 0.4, alpha = 0.5) +
  geom_sf(data = UD.Ia2.95, fill = "violet", col = "violet", lwd = 0.4, alpha = 0.7) +
  geom_sf(data = UD.Ia2.50, fill = "violetred", col = "violetred", lwd = 0.4, alpha = 0.5) +
  geom_point(data = recdata2, aes(x = lon, y = lat), col = "grey28", size = 0.8) +
  coord_sf(crs = st_crs(3395), xlim = c(-8849899.51, -8794239.77), ylim = c(2910311.29, 2956969.66)) + 
  scale_x_continuous(breaks = c(-79.4, -79.2)) +
  scale_y_continuous(breaks = c(25.45, 25.6, 25.75)) +
  annotation_scale(style = "ticks", location = "br", text_cex = 0.85, line_width = 1, height = unit(0.35, "cm"), text_pad = unit(0.15, "cm"), pad_x = unit(0.35, "cm"), pad_y = unit(0.35, "cm")) + 
  theme_bw() +
  theme(text = element_text(size = 15)) +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5)) + 
  theme(axis.ticks.y = element_line(size = 0.5)) + 
  theme(axis.ticks.x = element_line(size = 0.5)) +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  annotate("text", x = -8849899.51, y = 2956969.66, label = "Turtle I", size = 6.5, hjust = 0, vjust = 1)

ggsave(filename = "I_full2.tiff", plot = I_full2, device = "tiff",path = "/Users/EmilyHardin/Desktop/Research/Acous.vs.Sat/From Mariana", width = 120, height = 120, units = c("mm"), dpi = 600)



## COMPARISONS OF ALL ODs ## 
# all OD areas were compiled in a dataframe
UD.Comp2 <- read.csv("UD.Comp2.csv", header = T)
# Comp.Full.Eq <- read.csv("Comp.Full.Eq.csv", header = T)

# Acoustic Matching Temporal Scales
mean(UD.Comp2$Equal_Ac_95, na.rm = T) ## 3.183
sd <- sd(UD.Comp2$Equal_Ac_95, na.rm = T) ## 2.092
sd/sqrt(5) ## 0.956
mean(UD.Comp2$Equal_Ac_50, na.rm = T) ## 0.3047966
sd <- sd(UD.Comp2$Equal_Ac_50, na.rm = T) ## 0.0393671
sd/sqrt(5)
# Satellite Matching Temporal Scale 
mean(UD.Comp2$Equal_Sat_95, na.rm = T) ## 195.696 (A, C, D, E, G, H, I)
sd <- sd(UD.Comp2$Equal_Sat_95, na.rm = T) ## 385.6383   
sd/sqrt(7)
mean(UD.Comp2$Equal_Sat_50, na.rm = T) ## 10.24263 (A, C, D, E, G, H, I)
sd <- sd(UD.Comp2$Equal_Sat_50, na.rm = T) ## 14.53882
sd/sqrt(7)

# Acoustic Full Temporal Scale 
mean(UD.Comp$Full_Ac_95, na.rm = T) ## 3.227929 (A, C, D, F, G, I)
sd <- sd(UD.Comp$Full_Ac_95, na.rm = T) ## 1.753581 
sd/sqrt(6)
mean(UD.Comp$Full_Ac_50, na.rm = T) ## 0.2993999 (A, C, D, F, G, I)
sd <- sd(UD.Comp$Full_Ac_50, na.rm = T) ## 0.06744268 
sd/sqrt(6)
# Satellite Full Temporal Scale 
mean(UD.Comp$Full_Sat_95, na.rm = T) ## 157.7311 (A, B, C, D, E, G, H, I)
sd <- sd(UD.Comp$Full_Sat_95, na.rm = T) ## 336.8726 
sd/sqrt(8)
mean(UD.Comp$Full_Sat_50, na.rm = T) ## 5.690106 (A, B, C, D, E, G, H, I)
sd <- sd(UD.Comp$Full_Sat_50, na.rm = T) ## 8.622876 
sd/sqrt(8)



## COMPARISONS OF ODs OF 5 TURTLES SUCCESSFULLY TRACKED WITH BOTH TELEMETRY METHODS ## 

# Only Turtles A, C, D, G, and I have both Acoustic and Satellite UDs and therefore only these individuals can be used in comparisons of tracking types 
# Create a new dataframe containing only these individuals 
UD.Comp2.5 <- UD.Comp2 %>%
  filter(ID == "A" | ID == "C" | ID == "D" | ID == "G" | ID == "I")

## BOXPLOTS ## 

# Matching Temporal Duration
UD2 <- read.csv("UD.Comp_Equal2.csv", header = T) # matching temporal duration UDs for 5 comparative turtles 
yl <- expression(paste("OD Size (",km^2,")",sep=""))

M.UD.box2 <- ggplot(UD2, aes(x = Method, y = UD, fill = Method)) + 
  geom_boxplot(outlier.colour = "black", outlier.shape = 21, outlier.size = 2, show.legend = T) +
  ylab(yl) +
  xlab("Matching Temporal Duration") +
  scale_fill_manual(values = c("grey40", "grey")) +
  theme(legend.position = "right", 
        legend.text = element_text(size = 8)) +
  theme_bw() +
  theme(text = element_text(size = 30)) +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5)) + 
  theme(axis.ticks.y = element_line(size = 0.75), 
        axis.ticks.y.right = element_blank(), 
        axis.text.y.right = element_blank()) + 
  theme(axis.ticks.x = element_line(size = 0.75)) +
  theme(axis.text.x=element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.spacing.x = unit(0, "line")) + 
  facet_wrap(~Contour) 
ggsave(filename = "M.UD.box2.tiff", plot = M.UD.box2, device = "tiff", path = "/Users/EmilyHardin/Desktop/Research/Acous.vs.Sat/Data & Code", width = 240, height = 240, units = c("mm"), dpi = 600)

# Full Temporal Duration
UD.Full2 <- read.csv("UD.Comp_Full2.csv", header = T)
F.UD.box2 <- ggplot(UD.Full2, aes(x = Method, y = UD, fill = Method)) + 
  geom_boxplot(outlier.colour = "black", outlier.shape = 21, outlier.size = 2, show.legend = T) +
  ylab(yl) +
  xlab("Full Temporal Duration") +
  scale_fill_manual(values = c("grey40", "grey")) +
  theme(legend.position = "right", 
        legend.text = element_text(size = 8)) +
  labs(fill = "Telemetry\nMethod") +
  theme_bw() +
  theme(text = element_text(size = 30)) +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5)) + 
  theme(axis.ticks.y = element_line(size = 0.75), 
        axis.ticks.y.right = element_blank(), 
        axis.text.y.right = element_blank()) + 
  theme(axis.ticks.x = element_line(size = 0.75)) +
  theme(axis.text.x=element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.spacing.x = unit(0, "line")) + 
  facet_wrap(~Contour)
ggsave(filename = "F.UD.box2.tiff", plot = F.UD.box2, device = "tiff", path = "/Users/EmilyHardin/Desktop/Research/Acous.vs.Sat/Data & Code", width = 240, height = 240, units = c("mm"), dpi = 600)


## SIGNIFICANCE TESTS ##

# Matching Temporal Duration - 95% 
t.test(UD.Comp2.5$Equal_Ac_95, UD.Comp2.5$Equal_Sat_95, paired = T, var.equal = F)
BEST.mat.95.BB <- BESTmcmc(UD.Comp2.5$Equal_Ac_95, UD.Comp2.5$Equal_Sat_95, parallel= F)
plot(BEST.mat.95.BB)
# 91.9% chance that satellite UDs are larger than acoustic UDs
# not significantly different (0.07) ##CLOSE##

# Matching Temporal Duration - 50% 
t.test(UD.Comp2.5$Equal_Ac_50, UD.Comp2.5$Equal_Sat_50, paired = T, var.equal = F)
BEST.mat.50.BB <- BESTmcmc(UD.Comp.5$Equal_Ac_50, UD.Comp.5$Equal_Sat_50, parallel = F)
plot(BEST.mat.50.BB)
# not significantly different (0.13)
# 90.1% different

# Full Temporal Duration - 95% 
t.test(UD.Comp2.5$Full_Ac_95, UD.Comp2.5$Full_Sat_95, paired = T, var.equal = F)
BEST.full.95.BB <- BESTmcmc(UD.Comp.5$Full_Ac_95, UD.Comp.5$Full_Sat_95, parallel = F)
plot(BEST.full.95.BB)
# not significantly different 

# Full Temporal Duration - 50% 
t.test(UD.Comp2.5$Full_Ac_50, UD.Comp2.5$Full_Sat_50, paired = T, var.equal = F)
BEST.full.50.BB <- BESTmcmc(UD.Comp.5$Full_Ac_50, UD.Comp.5$Full_Sat_50, parallel = F)
plot(BEST.full.50.BB)
# not significantly different 


## CALCULATING HOW MUCH LARGER SATELLITE ODs ARE THAN ACOUSTIC ODs ## 

# Matching Temporal Duration 95% 
UD.Comp2.5$Equal_Inc_95 <- UD.Comp2.5$Equal_Sat_95/UD.Comp2.5$Equal_Ac_95
mean(UD.Comp2.5$Equal_Inc_95, na.rm = T) # on average 6.5x times larger than acoustic 

# Matching Temporal Duration 50% 
UD.Comp2.5$Equal_Inc_50 <- UD.Comp2.5$Equal_Sat_50/UD.Comp2.5$Equal_Ac_50
mean(UD.Comp2.5$Equal_Inc_50, na.rm = T) # on average 9x larger than acoustic

# Full Temporal Duration 95% 
UD.Comp2.5$Full_Inc_95 <- UD.Comp2.5$Full_Sat_95/UD.Comp2.5$Full_Ac_95
mean(UD.Comp2.5$Full_Inc_95, na.rm = T) # on average 5.5x larger than acoustic 

# Full Temporal Duration 50%
UD.Comp2.5$Full_Inc_50 <- UD.Comp2.5$Full_Sat_50/UD.Comp2.5$Full_Ac_50
mean(UD.Comp2.5$Full_Inc_50, na.rm = T) # on average 4.6x  x larger than acoustic


## ASSESSING EFFECT OF FULL TEMPORAL DURATION VS. MATCHING TEMPORAL DURATION ##
Comp.Full.Eq <- read.csv("dat.csv", header = T)
# Matching vs. Full Temporal Duration 95% UDs
t.test(Comp.Full.Eq$Equal_95, Comp.Full.Eq$Full_95, paired = T, var.equal = F)
# t= 1.486
# df = 6
# p-value = 0.1878
# NOT significant
BEST <- BESTmcmc(Comp.Full.Eq$Equal_95, Comp.Full.Eq$Full_95, parallel = F)
plot(BEST)

# Equal vs. Full Temporal Duration 50% UDs
boxplot(Comp.Full.Eq$Equal_50, Comp.Full.Eq$Full_50) # E is a potential outlier
t.test(Comp.Full.Eq$Equal_50, Comp.Full.Eq$Full_50, paired = T, var.equal = F)
# t= 1.5936
# df = 6 
# p-value = 0.1621
# NOT significant 
BEST <- BESTmcmc(Comp.Full.Eq$Equal_50, Comp.Full.Eq$Full_50, parallel = F)
plot(BEST)

## CALCULATE OVERLAP OF ACOUSTIC AND SATELLITE ODs ## 
BA.index <- function(x,y){
  z<-sum(sqrt(x)*sqrt(y))
  z
}

# Turtle A
load(file="Aas2") # dBBMM object (dBB.Aas) for Turtle A's matching temporal duration acoustic data
load(file="Ass2") # dBBMM object (dBB.Ass) for Turtle A's matching temporal duration satellite data 
load(file= "Aa2") # dBBMM object (dBB.Aa) for Turtle A's full temporal duration acoustic data 
load(file= "As2") # dBBMM object (dBB.As) for Turtle A's full temporal duration satellite data 

# Matching 95% 
UD.Aas2 <- getVolumeUD(dBB.Aas2) # get UD
Aas.952 <- dBB.Aas2 # create new object 
Aas.952[UD.Aas2 > 0.95] <- 0 # replace raster values > 0.95 with 0
UD.Ass2 <- getVolumeUD(dBB.Ass2) # same process for satellite data
Ass.952 <- dBB.Ass2
Ass.952[UD.Ass2 > 0.95] <- 0
AAvec.952 <- raster::values(Aas.952) # get raster values 
ASvec.952 <- raster::values(Ass.952) # get raster values
BA.A.95.mat2 <- print(BA.index(AAvec.952, ASvec.952)) # 0.6339176

# Matching 50% 
UD.Aas2 <- getVolumeUD(dBB.Aas2)
Aas.502 <- dBB.Aas2
Aas.502[UD.Aas2 > 0.50] <- 0
UD.Ass2 <- getVolumeUD(dBB.Ass2)
Ass.502 <- dBB.Ass2
Ass.502[UD.Ass2 > 0.50] <- 0
AAvec.502 <- raster::values(Aas.502)
ASvec.502 <- raster::values(Ass.502)
BA.A.50.mat2 <- print(BA.index(AAvec.502, ASvec.502)) # 0.1984987

# Full 95% 
UD.Aa2 <- getVolumeUD(dBB.Aa2)
Aa.952 <- dBB.Aa2
Aa.952[UD.Aa2 > 0.95] <- 0
UD.As2 <- getVolumeUD(dBB.As2)
As.952 <- dBB.As2
As.952[UD.As2 > 0.95] <- 0
AAvec.952 <- raster::values(Aa.952)
ASvec.952 <- raster::values(As.952)
BA.A.95.full2 <- print(BA.index(AAvec.952, ASvec.952)) # 0.6418192

# Full 50% 
UD.Aa2 <- getVolumeUD(dBB.Aa2)
Aa.502 <- dBB.Aa2
Aa.502[UD.Aa2 > 0.50] <- 0
UD.As2 <- getVolumeUD(dBB.As2)
As.502 <- dBB.As2
As.502[UD.As2 > 0.50] <- 0
AAvec.502 <- raster::values(Aa.502)
ASvec.502 <- raster::values(As.502)
BA.A.50.full2 <- print(BA.index(AAvec.502, ASvec.502)) # 0.1984158


# Turtle C
load(file="Cas2")
load(file="Css2") 
load(file= "Ca2") 
load(file= "Cs2")

# Matching 95% 
UD.Cas2 <- getVolumeUD(dBB.Cas2)
Cas.952 <- dBB.Cas2
Cas.952[UD.Cas2 > 0.95] <- 0
UD.Css2 <- getVolumeUD(dBB.Css2)
Css.952 <- dBB.Css2
Css.952[UD.Css2 > 0.95] <- 0
CAvec.952 <- raster::values(Cas.952)
CSvec.952 <- raster::values(Css.952)
BA.C.95.mat2 <- print(BA.index(CAvec.952, CSvec.952)) # 0.5769751

# Matching 50%
UD.Cas2 <- getVolumeUD(dBB.Cas2)
Cas.502 <- dBB.Cas2
Cas.502[UD.Cas2 > 0.50] <- 0
UD.Css2 <- getVolumeUD(dBB.Css2)
Css.502 <- dBB.Css2
Css.502[UD.Css2 > 0.50] <- 0
CAvec.502 <- raster::values(Cas.502)
CSvec.502 <- raster::values(Css.502)
BA.C.50.mat2 <- print(BA.index(CAvec.502, CSvec.502)) # 0.04025234

# Full 95% 
UD.Ca2 <- getVolumeUD(dBB.Ca2)
Ca.952 <- dBB.Ca2
Ca.952[UD.Ca2 > 0.95] <- 0
UD.Cs2 <- getVolumeUD(dBB.Cs2)
Cs.952 <- dBB.Cs2
Cs.952[UD.Cs2 > 0.95] <- 0
CAvec.952 <- raster::values(Ca.952)
CSvec.952 <- raster::values(Cs.952)
BA.C.95.full2 <- print(BA.index(CAvec.952, CSvec.952)) # 0.6408069

# Full 50%
UD.Ca2 <- getVolumeUD(dBB.Ca2)
Ca.502 <- dBB.Ca2
Ca.502[UD.Ca2 > 0.50] <- 0
UD.Cs2 <- getVolumeUD(dBB.Cs2)
Cs.502 <- dBB.Cs2
Cs.502[UD.Cs2 > 0.50] <- 0
CAvec.502 <- raster::values(Ca.502)
CSvec.502 <- raster::values(Cs.502)
BA.C.50.full2 <- print(BA.index(CAvec.502, CSvec.502)) # 0.09666418

# Turtle D
load(file="Das2") 
load(file="Dss2") 
load(file= "Da2") 
load(file= "Ds2")

# Matching 95%
UD.Das2 <- getVolumeUD(dBB.Das2)
Das.952 <- dBB.Das2
Das.952[UD.Das2 > 0.95] <- 0
UD.Dss2 <- getVolumeUD(dBB.Dss2)
Dss.952 <- dBB.Dss2
Dss.952[UD.Dss2 > 0.95] <- 0
DAvec.952 <- raster::values(Das.952)
DSvec.952 <- raster::values(Dss.952)
BA.D.95.mat2 <- print(BA.index(DAvec.952, DSvec.952)) # 0.562111

# Matching 50% 
UD.Das2 <- getVolumeUD(dBB.Das2)
Das.502 <- dBB.Das2
Das.502[UD.Das2 > 0.50] <- 0
UD.Dss2 <- getVolumeUD(dBB.Dss2)
Dss.502 <- dBB.Dss2
Dss.502[UD.Dss2 > 0.50] <- 0
DAvec.502 <- raster::values(Das.502)
DSvec.502 <- raster::values(Dss.502)
BA.D.50.mat2 <- print(BA.index(DAvec.502, DSvec.502)) # 0.1868961

# Full 95%
UD.Da2 <- getVolumeUD(dBB.Da2)
Da.952 <- dBB.Da2
Da.952[UD.Da2 > 0.95] <- 0
UD.Ds2 <- getVolumeUD(dBB.Ds2)
Ds.952 <- dBB.Ds2
Ds.952[UD.Ds2 > 0.95] <- 0
DAvec.952 <- raster::values(Da.952)
DSvec.952 <- raster::values(Ds.952)
BA.D.95.full2 <- print(BA.index(DAvec.952, DSvec.952)) # 0.5711023

# Full 50% 
UD.Da2 <- getVolumeUD(dBB.Da2)
Da.502 <- dBB.Da2
Da.502[UD.Da2 > 0.50] <- 0
UD.Ds2 <- getVolumeUD(dBB.Ds2)
Ds.502 <- dBB.Ds2
Ds.502[UD.Ds2 > 0.50] <- 0
DAvec.502 <- raster::values(Da.502)
DSvec.502 <- raster::values(Ds.502)
BA.D.50.full2 <- print(BA.index(DAvec.502, DSvec.502)) # 0.1887278


# Turtle G 
load(file="Gas2") 
load(file="Gss2") 
load(file= "Ga2") 
load(file= "Gs2")

# Matching 95%
UD.Ga2 <- getVolumeUD(dBB.Ga2)
Ga.952 <- dBB.Ga2
Ga.952[UD.Ga2 > 0.95] <- 0
UD.Gss2 <- getVolumeUD(dBB.Gss2)
Gss.952 <- dBB.Gss2
Gss.952[UD.Gss2 > 0.95] <- 0
GAvec.952 <- raster::values(Ga.952)
GSvec.952 <- raster::values(Gss.952) 
BA.G.95.mat2 <- print(BA.index(GAvec.952, GSvec.952)) # 0.7098831

# Matching 50% 
UD.Ga2 <- getVolumeUD(dBB.Ga2)
Ga.502 <- dBB.Ga2
Ga.502[UD.Ga2 > 0.50] <- 0
UD.Gss2 <- getVolumeUD(dBB.Gss2)
Gss.502 <- dBB.Gss2
Gss.502[UD.Gss2 > 0.50] <- 0
GAvec.502 <- raster::values(Ga.502)
GSvec.502 <- raster::values(Gss.502)
BA.G.50.mat2 <- print(BA.index(GAvec.502, GSvec.502)) # 0.2871481


# Full 95%
UD.Ga2 <- getVolumeUD(dBB.Ga2)
Ga.952 <- dBB.Ga2
Ga.952[UD.Ga2 > 0.95] <- 0
UD.Gs2 <- getVolumeUD(dBB.Gs2)
Gs.952 <- dBB.Gs2
Gs.952[UD.Gs2 > 0.95] <- 0
GAvec.952 <- raster::values(Ga.952)
GSvec.952 <- raster::values(Gs.952) 
BA.G.95.full2 <- print(BA.index(GAvec.952, GSvec.952)) # 0.7562289

# Full 50% 
UD.Ga2 <- getVolumeUD(dBB.Ga2)
Ga.502 <- dBB.Ga2
Ga.502[UD.Ga2 > 0.50] <- 0
UD.Gs2 <- getVolumeUD(dBB.Gs2)
Gs.502 <- dBB.Gs2
Gs.502[UD.Gs2 > 0.50] <- 0
GAvec.502 <- raster::values(Ga.502)
GSvec.502 <- raster::values(Gs.502)
BA.G.50.full2 <- print(BA.index(GAvec.502, GSvec.502)) # 0.2846959


# Turtle I 
load(file="Ias2") 
load(file="Iss2") 
load(file= "Ia2") 
load(file= "Is2")

# Matching 95%
UD.Ia2 <- getVolumeUD(dBB.Ia2)
Ia.952 <- dBB.Ia2
Ia.952[UD.Ia2 > 0.95] <- 0
UD.Iss2 <- getVolumeUD(dBB.Iss2)
Iss.952 <- dBB.Iss2
Iss.952[UD.Iss2 > 0.95] <- 0
IAvec.952 <- raster::values(Ia.952)
ISvec.952 <- raster::values(Iss.952)
BA.I.95.mat2 <- print(BA.index(IAvec.952, ISvec.952)) # 0.3170792

# Matching 50% 
UD.Ia2 <- getVolumeUD(dBB.Ia2)
Ia.502 <- dBB.Ia2
Ia.502[UD.Ia2 > 0.50] <- 0
UD.Iss2 <- getVolumeUD(dBB.Iss2)
Iss.502 <- dBB.Iss2
Iss.502[UD.Iss2 > 0.50] <- 0
IAvec.502 <- raster::values(Ia.502)
ISvec.502 <- raster::values(Iss.502)
BA.I.50.mat2 <- print(BA.index(IAvec.502, ISvec.502)) # 0.1089383


# Full 95%
UD.Ia2 <- getVolumeUD(dBB.Ia2)
Ia.952 <- dBB.Ia2
Ia.952[UD.Ia2 > 0.95] <- 0
UD.Is2 <- getVolumeUD(dBB.Is2)
Is.952 <- dBB.Is2
Is.952[UD.Is2 > 0.95] <- 0
IAvec.952 <- raster::values(Ia.952)
ISvec.952 <- raster::values(Is.952)
BA.I.95.full2 <- print(BA.index(IAvec.952, ISvec.952)) # 0.4418516

# Full 50% 
UD.Ia2 <- getVolumeUD(dBB.Ia2)
Ia.502 <- dBB.Ia2
Ia.502[UD.Ia2 > 0.50] <- 0
UD.Is2 <- getVolumeUD(dBB.Is2)
Is.502 <- dBB.Is2
Is.502[UD.Is2 > 0.50] <- 0
IAvec.502 <- raster::values(Ia.502)
ISvec.502 <- raster::values(Is.502)
BA.I.50.full2 <- print(BA.index(IAvec.502, ISvec.502)) # 0.1404437

# Make dataframe of all overlap indices (both from main study and supplemental study)
dat <- read.csv("overlap2.csv", header = T)
mean(dat$S_E_50)
min(dat$S_E_50)
max(dat$S_E_50)

# Difference between supplemental study overlap indices and regular study indices 

# Matching 95% 
t.test(dat$S_E_95, dat$E_95, paired = T, var.equal = F)
BEST <- BESTmcmc(dat$S_E_95, dat$E_95, parallel = F)
plot(BEST)
# t = 2.3548, df = 4, p-value = 0.07811

# Matching 50% 
t.test(dat$S_E_50, dat$E_50, paired = T, var.equal = F)
BEST <- BESTmcmc(dat$S_E_95, dat$E_95, parallel = F)
plot(BEST)
# t = 2.2952, df = 4, p-value = 0.08338

# Full 95% 
t.test(dat$S_F_95, dat$F_95, paired = T, var.equal = F)
BEST <- BESTmcmc(dat$S_F_95, dat$F_95, parallel = F)
plot(BEST)
# t = 2.5292, df = 4, p-value = 0.06472

# Full 50% 
t.test(dat$S_F_50, dat$F_50, paired = T, var.equal = F)
BEST <- BESTmcmc(dat$S_F_50, dat$F_50, parallel = F)
plot(BEST)
# t = 1.609, df = 4, p-value = 0.1829

# Compare full temporal UDs from regular and supplementary study 
data <- read.csv("UDs_Comp.csv", header = T)

# Full 95% Satellite UDs
t.test(data$S_S_95, data$S_95, paired = T, var.equal = F)
BEST <- BESTmcmc(data$S_S_95, data$S_95, parallel = F)
plot(BEST)
# t = -1.759, df = 4, p-value = 0.1534

# Full 50% Satellite UDs 
t.test(data$S_S_50, data$S_50, paired = T, var.equal = F)
BEST <- BESTmcmc(data$S_S_50, data$S_50, parallel = F)
plot(BEST)
# t = -1.5661, df = 4, p-value = 0.1924

# Full 95% Acoustic UDs
t.test(data$S_A_95, data$A_95, paired = T, var.equal = F)
BEST <- BESTmcmc(data$S_A_95, data$A_95, parallel = F)
plot(BEST)
# t = -1.3519, df = 4, p-value = 0.2478

# Full 50% Acoustic UDs
t.test(data$S_A_50, data$A_50, paired = T, var.equal = F)
BEST <- BESTmcmc(data$S_A_50, data$A_50, parallel = F)
plot(BEST)
# t = -1.5661, df = 4, p-value = 0.1924

