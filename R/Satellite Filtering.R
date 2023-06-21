#### SATELLITE FILTERING ####
# Previous Script: Calculate COAs

# Filter satellite data and get summary stats

library(dplyr)
library(lubridate)
library(sf)
library(tidyverse)
library(ggplot2)


## IMPORT DATA ##

# Each individual has it's own file containing the Argos transmission data 

sat_raw_A <- read.csv("sat_raw_A.csv", header = T)
sat_raw_A$date <- as.POSIXct(sat_raw_A$date, format = "%Y/%m/%d %H:%M:%S", tz = "America/New_York") # convert datetime 
sat_A <- sat_raw_A %>%
  filter(date > "2017-05-03 14:16:00") # filter out first 24 hours
sat_A <- sat_A[!duplicated(sat_A$date), ] # remove duplicates

# Repeat above for Turtle B 
sat_raw_B <- read.csv("sat_raw_B.csv", header = T)
sat_raw_B$date <- as.POSIXct(sat_raw_B$date, format = "%Y/%m/%d %H:%M:%S", tz = "America/New_York")  
sat_B <- sat_raw_B %>%
  filter(date > "2017-05-04 15:56:00")
sat_B <- sat_B[!duplicated(sat_B$date), ]

# Repeat above for Turtle C 
sat_raw_C <- read.csv("sat_raw_C.csv", header = T)
sat_raw_C$date <- as.POSIXct(sat_raw_C$date, format = "%Y/%m/%d %H:%M:%S", tz = "America/New_York")  
sat_C <- sat_raw_C %>%
  filter(date > "2017-05-03 14:13:00")
sat_C <- sat_C[!duplicated(sat_C$date), ]

# Repeat above for Turtle D 
sat_raw_D <- read.csv("sat_raw_D.csv", header = T)
sat_raw_D$date <- as.POSIXct(sat_raw_D$date, format = "%Y/%m/%d %H:%M:%S", tz = "America/New_York")  
sat_D <- sat_raw_D %>%
  filter(date > "2017-05-04 15:58:00")
sat_D <- sat_D[!duplicated(sat_D$date), ]

# Repeat above for Turtle E 
sat_raw_E <- read.csv("sat_raw_E.csv", header = T)
sat_raw_E$date <- as.POSIXct(sat_raw_E$date, format = "%Y/%m/%d %H:%M:%S", tz = "America/New_York")  
sat_E <- sat_raw_E %>%
  filter(date > "2017-05-04 16:05:00")
sat_E <- sat_E[!duplicated(sat_E$date), ]

# Repeat above for Turtle G 
sat_raw_G <- read.csv("sat_raw_G.csv", header = T)
sat_raw_G$date <- as.POSIXct(sat_raw_G$date, format = "%Y/%m/%d %H:%M:%S", tz = "America/New_York")  
sat_G <- sat_raw_G %>%
  filter(date > "2017-05-03 14:14:00")
sat_G <- sat_G[!duplicated(sat_G$date), ]

# Repeat above for Turtle H 
sat_raw_H <- read.csv("sat_raw_H.csv", header = T)
sat_raw_H$date <- as.POSIXct(sat_raw_H$date, format = "%Y/%m/%d %H:%M:%S", tz = "America/New_York")  
sat_H <- sat_raw_H %>%
  filter(date > "2017-05-04 16:01:00")
sat_H <- sat_H[!duplicated(sat_H$date), ]

# Repeat above for Turtle I 
sat_raw_I <- read.csv("sat_raw_I.csv", header = T)
sat_raw_I$date <- as.POSIXct(sat_raw_I$date, format = "%Y/%m/%d %H:%M:%S", tz = "America/New_York")  
sat_I <- sat_raw_I %>%
  filter(date > "2017-05-04 15:55:00")
sat_I <- sat_I[!duplicated(sat_I$date), ]


sat_all <- rbind(sat_A, sat_B, sat_C, sat_D, sat_E, sat_G, sat_H, sat_I) # Combine all turtle satellite filtered data 
sat_all2 <- rbind(sat_raw_A, sat_raw_B, sat_raw_C, sat_raw_D, sat_raw_E, sat_raw_G, sat_raw_H, sat_raw_I) #originally, 6595 total raw transmissions

sat_all <- sat_all %>% # Select desired columns 
  dplyr::select(id, date, lc, lat, long, smaj, smin, eor)
sat_all <- sat_all %>%
  filter(lc != "Z") # remove LC Z 
sat_all <- sat_all[!is.na(sat_all$lat),] # remove any NA locations
sat_all <- sat_all[!is.na(sat_all$long),] # remove any NA locations
sat_all$id <- as.character(sat_all$id) # ensure proper format
sat_all$smaj <- as.numeric(sat_all$smaj)
sat_all$smin <- as.numeric(sat_all$smin)
sat_all$eor <- as.numeric(sat_all$eor)

# Plot latitude over time to visually inspect the data 
ggplot() +
  geom_path(data = sat_all, aes(date, lat, color = id)) +
  theme_bw() +
  facet_wrap(~ id, scales = "free")

# Turtle H (169274) has a point way far south after tracking started that is obviously erroneous - we will filter that point out (lat = 21.3079)
sat_all <- sat_all %>%
  filter(lat != 21.3079)
write.csv(sat_all, "sat_all.csv")

sat_all <- sf::st_as_sf(sat_all, coords=c("long","lat"), crs = 4326) # set up coordinate system
sat_all$lon<-st_coordinates(sat_all)[,1] 
sat_all$lat<-st_coordinates(sat_all)[,2] 
sat_all <- st_transform(sat_all, 3395)


## SUMMARY STATS ##

# number of transmissions and last detection dates
satsum <- sat_all %>%
  group_by(id) %>%
  dplyr::summarize(n = n(), 
                   LastDet = max(date))

# number of LC 3 and 2 transmissions for each individual
loc.class <- sat_all %>%
  filter(id == "169274") %>% # change out ID
  group_by(lc) %>%
  dplyr::summarize(n = n())


## FILTER TRANSMISSION DATA FOR MATCHING TEMPORAL DURATION ##

# As with the acoustic data, some of the individuals were tracked longer with satellite telemetry and therefore the satellite data needs to be subset to match the acoustic temporal period. 4 turtles' (E, G, H, I) satellite tracks will need to be subset to match the temporal duration of their acoustic tracks. Additionally, in order to compare evenly with acoustic UDs, all satellite tracks will have the May gap time filtered out. 

# Turtle E 
sat_E_sub <- sat_E %>%
  filter(date <= "2017-06-17 11:16:00") # date when acoustic tracking ends
# Turtle G 
sat_G_sub <- sat_G %>%
  filter(date <= "2017-07-13 04:06:00") # date when acoustic tracking ends
# Turtle H 
sat_H_sub <- sat_H %>%
  filter(date <= "2017-05-14 13:00:00") %>% # date when acoustic tracking ends
  filter(lat != 21.3079)
# Turtle I 
sat_I_sub <- sat_I %>%
  filter(date <= "2017-07-26 22:49:00") # date when acoustic tracking ends


sat_all_sub <- rbind(sat_A, sat_B, sat_C, sat_D, sat_E_sub, sat_G_sub, sat_H_sub, sat_I_sub) # dataframe with all turtles' matching temporal duration satellite transmissions 
sat_all_sub <- sat_all_sub %>%
  dplyr::select(id, date, lc, lat, long, smaj, smin, eor)
# remove May time gap, LC Z, and NAs
sat_all_sub <- sat_all_sub %>%
  filter(date <= "2017-05-04 08:21:00" | date >= "2017-05-06 17:37:00") %>% # filter out transmissions that occurred when acoustic receivers were offline at beginning of May so that this dataframe has the same temporal duration as acoustic data
  filter(lc != "Z")
sat_all_sub <- sat_all_sub[!is.na(sat_all_sub$lat),]
sat_all_sub <- sat_all_sub[!is.na(sat_all_sub$long),]
sat_all_sub$lon <- sat_all_sub$long
sat_all_sub$id <- as.character(sat_all_sub$id)
sat_all_sub$smaj <- as.numeric(sat_all_sub$smaj)
sat_all_sub$smin <- as.numeric(sat_all_sub$smin)
sat_all_sub$eor <- as.numeric(sat_all_sub$eor)

write.csv(sat_all_sub, "sat_all_sub.csv")

# Visualize #
ggplot(data = Study_Area) +
  geom_sf(fill = "grey", col = "grey") +
  geom_point(data = sat_all, aes(lon, lat, color = id), alpha = 0.5) +
  theme_bw() +
  theme(text = element_text(size = 15)) +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5)) +
  theme(axis.ticks.y = element_line(size = 0.5)) +
  theme(axis.ticks.x = element_line(size = 0.5)) +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  facet_wrap(~ id) + 
  coord_sf(xlim = c(-79.5, -79), ylim = c(25.5, 26)) # points for some turtles are out of frame 

# Next Script: Continuous Time SSM_ODs







