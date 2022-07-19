#### DETECTION REASSIGNMENT #### 
# Previous Script: Acoustic Filtering

# Since acoustic detections are logged as the lat/long of the receiver, not  the actual location of the individual, we want to reassign the detection locations to be within the 50% detection probability range of the receiver weighted by detection probability (i.e. more detections closer to the receiver). This is more representative of the actual turtle locations, plus it induces noise so that if a turtle was only detected on one receiver, all lat/long are not the same 
# In order to do this, Dr. Josh Cullen created a function based on the detection range test data provided by Maurits P.M. van Zinnicq Bergmann from the Bimini Shark Lab.The code below is a product of JC, MvZB, and EH.

library(dplyr)
library(lubridate)
library(sf)
library(tidyverse)
library(ggplot2)


## IMPORT RANGE TEST DATA ##

df_new <- read.table("sentinel detections with loc.txt", sep = ",", dec = ".", header = T)
df1_new<- df_new %>% 
  filter(location %in% c('south longline a east', 'south west turtle')) %>%  # only keep test locations
  rename(datetime = Date.and.time.UTC) %>%  # replace name with something shorter
  mutate(datetime = as_datetime(datetime, tz="US/Eastern")) %>%  # convert to POSIXct class
  mutate(date = as_date(datetime),
         time = strftime(datetime, format = "%H:%M:%S"), .before = sentinel.id) %>%
  arrange(location, datetime)

# make a dataframe referencing the sentinel tag numbers with the test locations and their distances from the test receivers
site_ref<- data.frame(sentinel.id = c(65230,65231,65233,65234,
                                      65229,65232,65250,65251),
                      dist = c(750, 0, 250, 500),
                      location = rep(c('south longline a east', 'south west turtle'), each = 4),
                      habitat = rep(c("bank","reef"), each = 4))

# join the data into a new dataframe
df1_new<- left_join(df1_new, site_ref, by = c('sentinel.id','location'))

# Summarize the number of detections per sentinel.id per datetime
site.f_new<- df1_new %>%
  group_by(habitat, dist, date) %>%
  tally() %>%
  ungroup()

# some dates are missing: need to reintroduce those missing days:
start <- min(site.f_new$date)
end <- max(site.f_new$date)
site.f_new2<- site.f_new %>%
  arrange(habitat, date) %>%
  filter(date > (start + 1) & date < (end - 1)) %>%  # remove 1st and last 2 days of obs
  mutate(n = case_when(n > 173 ~ 173,
                       TRUE ~ as.double(n))) %>%
  mutate(non.det = 173 - n)  # number of non-detects per day


## RUN LOGISTIC REGRESSION PER HABITAT ## 

# Sandy Bank Habitat
bank.dat<- site.f_new2 %>%
  filter(habitat == 'bank')
mod.bank<- glm(cbind(n, non.det) ~ dist, data = bank.dat, family = binomial)
summary(mod.bank)
  # Coefficients:
  #               Estimate Std. Error z value Pr(>|z|)
  # (Intercept)  2.125e+00  6.288e-03   337.9   <2e-16 ***
  #   dist        -6.059e-03  1.743e-05  -347.7   <2e-16 ***

# plot to visualize 
new.bank.dat<- data.frame(dist = seq(0, 750))
new.bank.dat$preds<- plogis(predict(mod.bank, newdata = new.bank.dat))
plot(bank.dat$dist, bank.dat$n / 173)
lines(preds ~ dist, data = new.bank.dat)

# Reef Habitat 
reef.dat<- site.f_new2 %>%
  filter(habitat == 'reef')
mod.reef<- glm(cbind(n, non.det) ~ dist, data = reef.dat, family = binomial)
summary(mod.reef)
  # Coefficients:
  #                 Estimate Std. Error z value Pr(>|z|)
  # (Intercept)  2.478e+00  9.375e-03   264.3   <2e-16 ***
  #   dist        -1.353e-02  4.116e-05  -328.7   <2e-16 ***

# plot to visualize
new.reef.dat<- data.frame(dist = seq(0, 750))
new.reef.dat$preds<- plogis(predict(mod.reef, newdata = new.reef.dat))
plot(reef.dat$dist, reef.dat$n / 173)
lines(preds ~ dist, data = new.reef.dat)


## CREATE FUNCTION TO RANDOMLY DISTRIBUTE DETECTIONS ## 

## Sandy Bank Habitat 
r.bank<- function(nobs, xcoord, ycoord) {
  x1<- 0:750  # define input vector (of distance from receiver) to calc probabilities
  # calculate probabilities over distance based on logistic regression results
  probs<- exp(2.124770354 - 0.006058997*x1) / (1 + exp(2.124770354 - 0.006058997*x1))
  # generate random sample of distances from receiver based on detection probability
  dist<- sample(x = x1, size = nobs, replace = TRUE, prob = probs)
  # generate random direction
  angle<- runif(n = nobs, min = 0, max = 360)
  # calculate new coordinates; relies on {geosphere} package
  new.coords<- geosphere::destPoint(p = cbind(xcoord, ycoord),
                                    b = angle,
                                    d = dist) %>%
    data.frame()
  return(new.coords)
}

## Reef Habitat
r.reef<- function(nobs, xcoord, ycoord) {
  x1<- 0:750  # define input vector (of distance from receiver) to calc probabilities
  # calculate probabilities over distance based on logistic regression results
  probs<- exp(2.47823255 - 0.01352845*x1) / (1 + exp(2.47823255 - 0.01352845*x1))
  # generate random sample of distances from receiver based on detection probability
  dist<- sample(x = x1, size = nobs, replace = TRUE, prob = probs)
  # generate random direction
  angle<- runif(n = nobs, min = 0, max = 360)
  # calculate new coordinates; relies on {geosphere} package
  new.coords<- geosphere::destPoint(p = cbind(xcoord, ycoord),
                                    b = angle,
                                    d = dist) %>%
    data.frame()
  return(new.coords)
}


## APPLY FUNCTIONS TO ACOUSTIC DETECTION DATA ##

# Any points that are placed over land to be re-run. Any that are still placed over land will be reassigned to the receiver location

bank.det <- filtdet9 %>%
  filter(habitat == "bank") # create dataframe of detections on sandy bank receivers 
reef.det <- filtdet9 %>%
  filter(habitat == "reef") # create dataframe of detections on reef receivers 

# run functions created above to reassign detection locations 
new.reef.det <- r.reef(nobs = 1308, xcoord = reef.det$deploy_long, ycoord = reef.det$deploy_lat)
new.bank.det <- r.bank(nobs = 18964, xcoord = bank.det$deploy_long, ycoord = bank.det$deploy_lat)

# the above functions return just columns with lat/long, so they need to be merged with the rest of the detection data
new.reef.det <- cbind(reef.det, new.reef.det)
new.bank.det <- cbind(bank.det, new.bank.det)

# these sf dataframes currently have the geometry associated with deploy_lat/deploy_long (i.e. receiver locations), but we need to reset the geometry to the newly assigned coordinates
new.reef.det <- new.reef.det %>%
  st_drop_geometry() %>%
  st_as_sf(., coords=c("lon","lat"), crs = 4326)
new.reef.det$lon<-st_coordinates(new.reef.det)[,1] 
new.reef.det$lat<-st_coordinates(new.reef.det)[,2] 
new.reef.det <- st_transform(new.reef.det, 3395) # transform the CRS projected World Mercator, units = m 

new.bank.det <- new.bank.det %>%
  st_drop_geometry() %>%
  st_as_sf(., coords=c("lon","lat"), crs = 4326)
new.bank.det$lon<-st_coordinates(new.bank.det)[,1]
new.bank.det$lat<-st_coordinates(new.bank.det)[,2]
new.bank.det <- st_transform(new.bank.det, 3395)

# some of these new scattered detections will inevitably be put on land because of the receivers' proximity to land. Any new locations over land will be reassigned again (from original receiver location). The remaining points still falling over land will be placed back on the receiver location.

Study_Area <- st_read(dsn = "bimini shapefile/bimini_shape.shp", quiet = TRUE) # shapefile of study area
Study_Area <- st_transform(Study_Area, 3395) # transform to common CRS

## REEF DATA
# identify areas over land 
land.reef <- lengths(st_intersects(new.reef.det, Study_Area)) > 0  
land.reef.det <- new.reef.det[land.reef,] # 7 points were relocated over land 
sea.reef.det <- new.reef.det[!land.reef,] # 1301 reef points that were relocated over water and are good to go and move forward with! 

# drop the lat/long on the land.reef.det dataframe and rerun the r.reef function to see if these points are reassigned over land 
land.reef.det <- land.reef.det %>%
  dplyr::select(-c(lat, lon)) %>%
  st_drop_geometry() # drop geometry because it is based off of the lat/long that we are trying to redo 

# rerun function 
new.reef.det2 <- r.reef(nobs = 7, xcoord = land.reef.det$deploy_long, ycoord = land.reef.det$deploy_lat)
new.reef.det2 <- cbind(land.reef.det, new.reef.det2) # merge new lat/long with rest of detection data
new.reef.det2 <- sf::st_as_sf(new.reef.det2, coords=c("lon","lat"), crs = 4326) # assign CRS and transform 
new.reef.det2$lon<-st_coordinates(new.reef.det2)[,1] 
new.reef.det2$lat<-st_coordinates(new.reef.det2)[,2]  
new.reef.det2 <- st_transform(new.reef.det2, 3395)

land.reef2 <- lengths(st_intersects(new.reef.det2, Study_Area)) > 0  # assess if locations are over land 
land.reef.det2 <- new.reef.det2[land.reef2,] #0 - wonderful - none of the locations were placed over land!! 

# combine above with the points that did not need to be run again/were already placed over land 
new.reef.det3 <- rbind(sea.reef.det, new.reef.det2)

## BANK DATA 
# identify areas over land 
land.bank <- lengths(st_intersects(new.bank.det, Study_Area)) > 0  
land.bank.det <- new.bank.det[land.bank,] # 2762 points were relocated over land
sea.bank.det <- new.bank.det[!land.bank,] # 16202 reef points that were relocated over water and are good to go and move forward with! 

# drop the lat/long on the land.reef.det dataframe and rerun the r.bank function to see if these points are reassigned over land 
land.bank.det <- land.bank.det %>%
  dplyr::select(-c(lat, lon)) %>%
  st_drop_geometry() 

# rerun function 
new.bank.det2 <- r.bank(nobs = 2762, xcoord = land.bank.det$deploy_long, ycoord = land.bank.det$deploy_lat)
new.bank.det2 <- cbind(land.bank.det, new.bank.det2) # merge new lat/long with rest of detection data
new.bank.det2 <- sf::st_as_sf(new.bank.det2, coords=c("lon","lat"), crs = 4326)
new.bank.det2$lon<-st_coordinates(new.bank.det2)[,1] 
new.bank.det2$lat<-st_coordinates(new.bank.det2)[,2] 
new.bank.det2 <- st_transform(new.bank.det2, 3395)

land.bank2 <- lengths(st_intersects(new.bank.det2, Study_Area)) > 0 # assess if locations are over land
land.bank.det2 <- new.bank.det2[land.bank2,] # 420 points relocated over land
sea.bank.det2 <- new.bank.det2[!land.bank2,]

# now, these points that were placed over land will be replaced with the receiver location 
land.bank.det2 <- land.bank.det2 %>%
  dplyr::select(-c(lat, lon)) %>%
  st_drop_geometry() 
land.bank.det2$lon <- land.bank.det2$deploy_long # make the long column equal to the deploy_long column
land.bank.det2$lat <- land.bank.det2$deploy_lat # make the lat column equal to the deploy_lat column
land.bank.det2 <- sf::st_as_sf(land.bank.det2, coords=c("lon","lat"), crs = 4326) # assign geometry 
land.bank.det2$lon<-st_coordinates(land.bank.det2)[,1]
land.bank.det2$lat<-st_coordinates(land.bank.det2)[,2]
land.bank.det2 <- st_transform(land.bank.det2, 3395)

# combine with the points that did not need to be run again/were already placed over land 
new.bank.det3 <- rbind(sea.bank.det, sea.bank.det2, land.bank.det2)

# combine reef and bank data 
new.det.data <- rbind(new.bank.det3, new.reef.det3)
write.csv(new.det.data, "new.det.data.csv")

# check that it worked
Study_Area <- st_transform(Study_Area, 4326)
ggplot(data = Study_Area) +
  geom_sf(fill = "grey", col = "grey") +
  geom_point(data = new.det.data, aes(lon, lat), color = "red", alpha = 0.5) +
  geom_point(data = filtdet9, aes(deploy_long, deploy_lat), size = 2, color = "black") +
  scale_x_continuous(breaks = c(-79.30, -79.20)) +
  scale_y_continuous(breaks = c(25.70, 25.78)) +
  theme_bw() +
  theme(text = element_text(size = 15)) +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5)) + 
  theme(axis.ticks.y = element_line(size = 0.5)) + 
  theme(axis.ticks.x = element_line(size = 0.5)) +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())


## APPLY FUNCTIONS TO THE SUBSET ACOUSTIC DETECTION DATA ##

bank.det_sub <- filtdet9_sub %>%
  filter(habitat == "bank")
reef.det_sub <- filtdet9_sub %>%
  filter(habitat == "reef")

# run functions
new.reef.det_sub <- r.reef(nobs = 975, xcoord = reef.det_sub$deploy_long, ycoord = reef.det_sub$deploy_lat)
new.bank.det_sub <- r.bank(nobs = 15007, xcoord = bank.det_sub$deploy_long, ycoord = bank.det_sub$deploy_lat)

# merge new lat/long with the rest of the detection data
new.reef.det_sub <- cbind(reef.det_sub, new.reef.det_sub)
new.bank.det_sub <- cbind(bank.det_sub, new.bank.det_sub)

new.reef.det_sub <- st_drop_geometry(new.reef.det_sub) # drop current geometry
new.reef.det_sub <- st_as_sf(new.reef.det_sub, coords=c("lon","lat"), crs = 4326)
new.reef.det_sub$lon<-st_coordinates(new.reef.det_sub)[,1] 
new.reef.det_sub$lat<-st_coordinates(new.reef.det_sub)[,2] 
new.reef.det_sub <- st_transform(new.reef.det_sub, 3395)

new.bank.det_sub <- st_drop_geometry(new.bank.det_sub)
new.bank.det_sub <- st_as_sf(new.bank.det_sub, coords=c("lon","lat"), crs = 4326) 
new.bank.det_sub$lon<-st_coordinates(new.bank.det_sub)[,1] 
new.bank.det_sub$lat<-st_coordinates(new.bank.det_sub)[,2] 
new.bank.det_sub <- st_transform(new.bank.det_sub, 3395)

## REEF DATA
land.reef_sub <- lengths(st_intersects(new.reef.det_sub, Study_Area)) > 0 # identify areas over land 
land.reef.det_sub <- new.reef.det_sub[land.reef_sub,] # 4 points were relocated over land 
sea.reef.det_sub <- new.reef.det_sub[!land.reef_sub,] # 971 reef points that were relocated over water and are good to go and move forward with! 

# drop the lat/long on the land.reef.det_sub dataframe and rerun the r.reef function to see if these points can go back over land 
land.reef.det_sub <- land.reef.det_sub %>%
  dplyr::select(-c(lat, lon)) %>%
  st_drop_geometry() 
new.reef.det2_sub <- r.reef(nobs = 4, xcoord = land.reef.det_sub$deploy_long, ycoord = land.reef.det_sub$deploy_lat)
new.reef.det2_sub <- cbind(land.reef.det_sub, new.reef.det2_sub) # merge
new.reef.det2_sub <- sf::st_as_sf(new.reef.det2_sub, coords=c("lon","lat"), crs = 4326)
new.reef.det2_sub$lon<-st_coordinates(new.reef.det2_sub)[,1] 
new.reef.det2_sub$lat<-st_coordinates(new.reef.det2_sub)[,2] 
new.reef.det2_sub <- st_transform(new.reef.det2_sub, 3395)

land.reef2_sub <- lengths(st_intersects(new.reef.det2_sub, Study_Area)) > 0 # identify areas over land
land.reef.det2_sub <- new.reef.det2_sub[land.reef2_sub,] # 0 points relocated over land
sea.reef.det2_sub <- new.reef.det2_sub[!land.reef2_sub,]

# combine with the points that did not need to be run again/were already placed over land 
new.reef.det3_sub <- rbind(sea.reef.det_sub, new.reef.det2_sub)

## BANK DATA
new.bank.det_sub <- st_drop_geometry(new.bank.det_sub)
new.bank.det_sub <- sf::st_as_sf(new.bank.det_sub, coords=c("lon","lat"), crs = 4326) 
new.bank.det_sub$lon<-st_coordinates(new.bank.det_sub)[,1] 
new.bank.det_sub$lat<-st_coordinates(new.bank.det_sub)[,2]
new.bank.det_sub <- st_transform(new.bank.det_sub, 3395)
land.bank_sub <- lengths(st_intersects(new.bank.det_sub, Study_Area)) > 0  # identify areas over land 
land.bank.det_sub <- new.bank.det_sub[land.bank_sub,] # 2070 points were relocated over land
sea.bank.det_sub <- new.bank.det_sub[!land.bank_sub,] # 12977 reef points that were relocated over water and are good to go and move forward with! 

# drop the lat/long on the land.reef.det dataframe and rerun the r.bank function to see if these points can go back over land 
land.bank.det_sub <- land.bank.det_sub %>%
  dplyr::select(-c(lat, lon)) %>%
  st_drop_geometry() 
new.bank.det2_sub <- r.bank(nobs = 2070, xcoord = land.bank.det_sub$deploy_long, ycoord = land.bank.det_sub$deploy_lat)
new.bank.det2_sub <- cbind(land.bank.det_sub, new.bank.det2_sub)

new.bank.det2_sub <- sf::st_as_sf(new.bank.det2_sub, coords=c("lon","lat"), crs = 4326)
new.bank.det2_sub$lon<-st_coordinates(new.bank.det2_sub)[,1] 
new.bank.det2_sub$lat<-st_coordinates(new.bank.det2_sub)[,2] 
new.bank.det2_sub <- st_transform(new.bank.det2_sub, 3395)

land.bank2_sub <- lengths(st_intersects(new.bank.det2_sub, Study_Area)) > 0 # identify areas over land
land.bank.det2_sub <- new.bank.det2_sub[land.bank2_sub,] # 389 points relocated over land
sea.bank.det2_sub <- new.bank.det2_sub[!land.bank2_sub,]

# replace points that were still placed over land with the receiver location 
land.bank.det2_sub <- land.bank.det2_sub %>%
  dplyr::select(-c(lat, lon)) %>%
  st_drop_geometry() 
land.bank.det2_sub$lon <- land.bank.det2_sub$deploy_long
land.bank.det2_sub$lat <- land.bank.det2_sub$deploy_lat
land.bank.det2_sub <- sf::st_as_sf(land.bank.det2_sub, coords=c("lon","lat"), crs = 4326)
land.bank.det2_sub$lon<-st_coordinates(land.bank.det2_sub)[,1]
land.bank.det2_sub$lat<-st_coordinates(land.bank.det2_sub)[,2]
land.bank.det2_sub <- st_transform(land.bank.det2_sub, 3395)

# combine with the points that did not need to be run again/were already placed over land 
new.bank.det3_sub <- rbind(sea.bank.det_sub, sea.bank.det2_sub, land.bank.det2_sub)

# now combine reef and bank data 
new.det.data_sub <- rbind(new.bank.det3_sub, new.reef.det3_sub)
write.csv(new.det.data_sub, "new.det.data_sub.csv")


#Next Script: Calculate COAs

