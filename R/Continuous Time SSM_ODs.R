#### CONTINUOUS TIME SSM_ODS####
# Previous Script: Satellite Filtering 

# A continuous-time state-space model will be fit to the satellite telemetry data to filter the Argos geolocation data and improve location estimates. The {foieGras} package will be used for this, and since we are not interested in estimating behavioral states, but rather just correcting location estimates based on the provided errors, we will use the "fitted" locations returned by {foieGras}. The time step will be set to NA so that only locations at the time of transmissions will be estimated (i.e. locations will not be interpolated at regular time steps).

library(dplyr)
library(lubridate)
library(sf)
library(tidyverse)
library(foieGras) #**NOTE: package name has changed to aniMotum, however foieGras still functions


## PREPARE DATA ##
sat_all <- st_drop_geometry(sat_all) # drop the geometry from the sat_all data set
col_order <- c("id", "date", "lc", "lon", "lat", "smaj", "smin", "eor") # the columns need to be in a particular order for foieGras to run 
sat_all <- sat_all[, col_order] 


## RUN MODEL ##
fG <- foieGras::fit_ssm(sat_all, 
                        vmax = 2, # speed filter (m/s)
                        min.dt = 60, # min timestep b/w transmissions (s)
                        ang = c(15, 25), # turning angle filter (deg)
                        distlim = c(1500,3000), # distance filter (m)
                        model = "crw", # model, either random walk or correlated random walk
                        time.step = NA, # timestep - NA = not regularized
                        map = list(psi = factor(NA))) 

fG_fit <- foieGras::osar(fG) # assess model fit 

fG_data <- foieGras::grab(fG, what = "f", as_sf = FALSE)  # create a dataframe of the fitted value results
  # as_sf = FALSE gives you a dataframe lat/long and world mercator x/y


## FILTER OUT POINTS OVER LAND ##

fG_data <- sf::st_as_sf(fG_data, coords = c("lon","lat"), crs = 4326) # add CRS
fG_data$lon<-st_coordinates(fG_data)[,1] # get coordinates
fG_data$lat<-st_coordinates(fG_data)[,2] # get coordinates
fG_data <- st_transform(fG_data, 3395) # transform to world mercator
Study_Area <- st_read(dsn = "bimini shapefile/bimini_shape.shp", quiet = TRUE) # shapefile of study area
Study_Area <- st_transform(Study_Area, 3395) # transform to common CRS
land <- lengths(st_intersects(fG_data, Study_Area)) > 0 # identify which points intersect w/ land
fG_noland <- fG_data[!land,] # remove these overlapping points


## PREPARE DATA & RUN MODEL FOR MATCHING TEMPORAL DURATION DATA ## 

col_order <- c("id", "date", "lc",
               "lon", "lat", "smaj", "smin", "eor")
sat_all_sub <- sat_all_sub[, col_order] # get columns in the correct order

fG_sub <- foieGras::fit_ssm(sat_all_sub, 
                            vmax = 2, # speed filter (m/s)
                            min.dt = 60, # min timestep b/w transmissions (s)
                            ang = c(15, 25), # turning angle filter (deg)
                            distlim = c(1500,3000), # distance filter (m)
                            model = "crw", # model, either rw or crw
                            time.step = NA, # timestep - NA = not regularized
                            map = list(psi = factor(NA))) 

fG_sub_data <- foieGras::grab(fG_sub, what = "f", as_sf = FALSE)

# remove points over land
fG_sub_data <- sf::st_as_sf(fG_sub_data, coords=c("lon","lat"), crs = 4326) 
fG_sub_data$lon<-st_coordinates(fG_sub_data)[,1] 
fG_sub_data$lat<-st_coordinates(fG_sub_data)[,2] 
fG_sub_data <- st_transform(fG_sub_data, 3395)
land_sub <- lengths(st_intersects(fG_sub_data, Study_Area)) > 0 
fG_sub_noland<- fG_sub_data[!land_sub,] 


## PREP FOR NEXT STEP ## 

# When calculating dBBMM in future steps, the {move} package requires that locations be in a projected format (i.e. not in lat/long). We want to use ESPG:3395 (+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs). The "x", "y", "x.se", and "y.se" columns of the fG dataframes are already in world mercator CRS, but have units of km instead of the desired m. We are going to convert them here. The dBBMM also requires a location error, for which we will use the smaller of the two standard errors 

fG_noland$x <-  fG_noland$x*1000 
fG_noland$y <- fG_noland$y*1000
fG_noland$x.se <- fG_noland$x.se*1000
fG_noland$y.se <- fG_noland$y.se*1000
fG_noland$Location.Err <- pmin(fG_noland$x.se, fG_noland$y.se)
write.csv(fG_noland, "fG_noland.csv")

fG_sub_noland$x <-  fG_sub_noland$x*1000 
fG_sub_noland$y <- fG_sub_noland$y*1000
fG_sub_noland$x.se <- fG_sub_noland$x.se*1000
fG_sub_noland$y.se <- fG_sub_noland$y.se*1000
fG_sub_noland$Location.Err <- pmin(fG_sub_noland$x.se, fG_sub_noland$y.se)
write.csv(fG_sub_noland, "fG_sub_noland.csv")


# Next Script: Individual dBBMMs_ODs

