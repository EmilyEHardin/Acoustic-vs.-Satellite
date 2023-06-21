#### DETECTION REASSIGNMENT #### 
# Previous Script: Acoustic Filtering

# Since acoustic detections are logged as the lat/long of the receiver, not  the actual location of the individual, we want to reassign the detection locations to be within the 50% detection probability range of the receiver weighted by detection probability (i.e. more detections closer to the receiver). This is more representative of the actual turtle locations, plus it induces noise so that if a turtle was only detected on one receiver, all lat/long are not the same 
# In order to do this, Dr. Josh Cullen created a function based on the detection range test data provided by Maurits P.M. van Zinnicq Bergmann from the Bimini Shark Lab.The code below is a product of JC, MvZB, and EH.

library(dplyr)
library(lubridate)
library(sf)
library(tidyverse)
library(ggplot2)

## LOAD BIMINI SHAPEFILE ##

Study_Area <- st_read(dsn = "bimini shapefile/bimini_shape.shp", quiet = TRUE) # shapefile of study area

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
plot(bank.dat$dist, bank.dat$n / 173, main = "Bank Detection Probability",
     xlab = "Distance from receiver (m)", ylab = "Proportion of detections (per day)")
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
plot(reef.dat$dist, reef.dat$n / 173, main = "Reef Detection Probability",
     xlab = "Distance from receiver (m)", ylab = "Proportion of detections (per day)")
lines(preds ~ dist, data = new.reef.dat)


## CREATE FUNCTION TO RANDOMLY DISTRIBUTE DETECTIONS ## 

## Sandy Bank Habitat 
r.bank<- function(data, coords, land_shp) {
  nobs <- nrow(data)  # number of points to randomly generate
  
  x1<- 0:750  # define input vector (of distance from receiver) to calc probabilities
  # calculate probabilities over distance based on logistic regression results
  probs<- exp(2.124770354 - 0.006058997*x1) / (1 + exp(2.124770354 - 0.006058997*x1))
  # generate random sample of distances from receiver based on detection probability
  dist<- sample(x = x1, size = nobs, replace = TRUE, prob = probs)
  # generate random direction
  angle<- runif(n = nobs, min = 0, max = 360)
  # calculate new coordinates; relies on {geosphere} package
  new.coords<- geosphere::destPoint(p = data[,coords],
                                    b = angle,
                                    d = dist) %>%
    data.frame() %>%
    mutate(dist_m = dist)
  
  # determine whether any random points overlap land; if so, move back to water
  overlap_ind <- st_intersects(st_as_sf(new.coords, coords = c('lon','lat'), crs = 4326), land_shp) %>%
    as.numeric()
  ind <- which(!is.na(overlap_ind))
  
  
  while (length(ind) > 0) {
    dist1<- sample(x = x1, size = length(ind), replace = TRUE, prob = probs)
    angle1<- runif(n = length(ind), min = 0, max = 360)
    land.coords<- geosphere::destPoint(p = data[ind,coords],
                                       b = angle1,
                                       d = dist1) %>%
      data.frame() %>%
      mutate(dist_m = dist1)
    
    new.coords[ind,] <- land.coords  #add newly generated points
    
    # re-evaluate whether any points still overlap land
    overlap_ind <- st_intersects(st_as_sf(new.coords, coords = c('lon','lat'), crs = 4326), land_shp) %>%
      as.numeric()
    ind <- which(!is.na(overlap_ind))
  }
  
  # Merge random locs w/ original dataset
  names(new.coords)[1:2] <- c('lon1','lat1')
  data <- cbind(data, new.coords)
  
  return(data)
}

## Reef Habitat
r.reef<- function(data, coords, land_shp) {
  nobs <- nrow(data)  # number of points to randomly generate
  
  x1<- 0:750  # define input vector (of distance from receiver) to calc probabilities
  # calculate probabilities over distance based on logistic regression results
  probs<- exp(2.47823255 - 0.01352845*x1) / (1 + exp(2.47823255 - 0.01352845*x1))
  # generate random sample of distances from receiver based on detection probability
  dist<- sample(x = x1, size = nobs, replace = TRUE, prob = probs)
  # generate random direction
  angle<- runif(n = nobs, min = 0, max = 360)
  # calculate new coordinates; relies on {geosphere} package
  new.coords<- geosphere::destPoint(p = data[,coords],
                                    b = angle,
                                    d = dist) %>%
    data.frame() %>%
    mutate(dist_m = dist)
  
  # determine whether any random points overlap land; if so, move back to water
  overlap_ind <- st_intersects(st_as_sf(new.coords, coords = c('lon','lat'), crs = 4326), land_shp) %>%
    as.numeric()
  ind <- which(!is.na(overlap_ind))
  
  
  while (length(ind) > 0) {
    dist1<- sample(x = x1, size = length(ind), replace = TRUE, prob = probs)
    angle1<- runif(n = length(ind), min = 0, max = 360)
    land.coords<- geosphere::destPoint(p = data[ind,coords],
                                       b = angle1,
                                       d = dist1) %>%
      data.frame() %>%
      mutate(dist_m = dist1)
    
    new.coords[ind,] <- land.coords  #add newly generated points
    
    # re-evaluate whether any points still overlap land
    overlap_ind <- st_intersects(st_as_sf(new.coords, coords = c('lon','lat'), crs = 4326), land_shp) %>%
      as.numeric()
    ind <- which(!is.na(overlap_ind))
  }
  
  # Merge random locs w/ original dataset
  names(new.coords)[1:2] <- c('lon1','lat1')
  data <- cbind(data, new.coords)
  
  return(data)
}

## APPLY FUNCTIONS TO ACOUSTIC DETECTION DATA ##

filtdet9 <- as.data.frame(filtdet9) # convert from sf object to dataframe

bank.det <- filtdet9 %>%
  filter(habitat == "bank") # create dataframe of detections on sandy bank receivers 
reef.det <- filtdet9 %>%
  filter(habitat == "reef") # create dataframe of detections on reef receivers 

# run functions created above to reassign detection locations 
set.seed(2022)
new.reef.det <- r.reef(data = reef.det, coords = c('deploy_long','deploy_lat'), land_shp = Study_Area)
new.bank.det <- r.bank(data = bank.det, coords = c('deploy_long','deploy_lat'), land_shp = Study_Area)

# these sf dataframes currently have the geometry associated with deploy_lat/deploy_long (i.e. receiver locations), but we need to reset the geometry to the newly assigned coordinates
new.reef.det <- new.reef.det %>%
  dplyr::select(-geometry) %>%
  st_as_sf(., coords=c("lon1","lat1"), crs = 4326, remove = FALSE) %>%
  st_transform(3395) # transform the CRS projected World Mercator, units = m

new.bank.det <- new.bank.det %>%
  dplyr::select(-geometry) %>%
  st_as_sf(., coords=c("lon1","lat1"), crs = 4326, remove = FALSE) %>%
  st_transform(3395) # transform the CRS projected World Mercator, units = m

# combine reef and bank data
new.det.data <- rbind(new.bank.det, new.reef.det) %>%
  st_drop_geometry()
write.csv(new.det.data, "new.det.data.csv", row.names = FALSE)

# visualize reassigned detection locations 
ggplot(data = Study_Area) +
  geom_sf(fill = "grey", col = "grey") +
  geom_point(data = new.det.data, aes(lon1, lat1), color = "red", alpha = 0.5) +
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


# Interactively investigate results (reminder: some receiver stations may have had two or more different actual receivers (and therefore different receiver #s) at different points in time)
library(bayesmove)
library(datamods)

new.det.data %>%
  dplyr::rename(id = receiver_sn, date = detection_timestamp, x = lon1, y = lat1) %>%
  mutate(date = as_datetime(date)) %>%
  bayesmove::shiny_tracks(epsg = 4326)

plotly::ggplotly(
  ggplot(data = Study_Area) +
    geom_sf(fill = "grey", col = "grey") +
    geom_point(data = new.det.data, aes(lon1, lat1, color = factor(receiver_sn)), alpha = 0.5) +
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
          panel.grid.minor = element_blank()))


## APPLY FUNCTIONS TO THE SUBSET ACOUSTIC DETECTION DATA ##

filtdet9_sub <- as.data.frame(filtdet9_sub) # convert from sf object to dataframe

bank.det_sub <- filtdet9_sub %>%
  filter(habitat == "bank") # create dataframe of detections on sandy bank receivers
reef.det_sub <- filtdet9_sub %>%
  filter(habitat == "reef") # create dataframe of detections on reef receivers

# run functions created above to reassign detection locations
set.seed(2022)
new.reef.det_sub <- r.reef(data = reef.det_sub, coords = c('deploy_long','deploy_lat'), land_shp = Study_Area)
new.bank.det_sub <- r.bank(data = bank.det_sub, coords = c('deploy_long','deploy_lat'), land_shp = Study_Area)

# these sf dataframes currently have the geometry associated with deploy_lat/deploy_long (i.e. receiver locations), but we need to reset the geometry to the newly assigned coordinates
new.reef.det_sub <- new.reef.det_sub %>%
  dplyr::select(-geometry) %>%
  st_as_sf(., coords=c("lon1","lat1"), crs = 4326, remove = FALSE) %>%
  st_transform(3395) # transform the CRS projected World Mercator, units = m

new.bank.det_sub <- new.bank.det_sub %>%
  dplyr::select(-geometry) %>%
  st_as_sf(., coords=c("lon1","lat1"), crs = 4326, remove = FALSE) %>%
  st_transform(3395) # transform the CRS projected World Mercator, units = m

# combine reef and bank data
new.det.data_sub <- rbind(new.bank.det_sub, new.reef.det_sub) %>%
  st_drop_geometry()
write.csv(new.det.data_sub, "new.det.data_sub.csv", row.names = FALSE)

# visualize reassigned detection locations 
ggplot(data = Study_Area) +
  geom_sf(fill = "grey", col = "grey") +
  geom_point(data = new.det.data_sub, aes(lon1, lat1), color = "red", alpha = 0.5) +
  geom_point(data = filtdet9_sub, aes(deploy_long, deploy_lat), size = 2, color = "black") +
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


#Next Script: Calculate COAs

