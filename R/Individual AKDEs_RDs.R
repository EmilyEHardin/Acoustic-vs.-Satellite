### INDIVIDUAL AKDES_RDS ###
# Previous script: Supplementary Study_ODs

setwd("/Volumes/Macintosh HD/Users/EmilyHardin/Desktop/Research/Acous.vs.Sat/Data & Code")
library(dplyr)
library(lubridate)
library(raster)
library(sf)
library(sp)
library(tidyverse)
library(flapper)
library(ctmm)
library(ggspatial)
library(rgdal)


# Previous scripts: Acoustic Filtering, Detection Reassignment, Calculate COAs, Satellite Filtering

## IMPORT FILTERED SATELLITE DATA ## 
sat_all <- read.csv("sat_all.csv")
sat_all <- sat_all %>% # rename columns to match what is needed for {ctmm} package
  rename(tag.local.identifier = id, 
         timestamp = date, 
         Argos.location.class = lc, 
         location.lat = lat, 
         location.long = long, 
         Argos.semi.major = smaj, 
         Argos.semi.minor = smin, 
         Argos.orientation = eor)
sat_all <- sat_all %>% # only columns that are needed (remove LCs to avoid confusion)
  dplyr::select(-c(Argos.location.class))
sat_all$timestamp<-as.POSIXct(sat_all$timestamp, format = "%m/%d/%Y %H:%M:%S", tz = "America/New_York") # convert datetime to proper format


# change to telemetry object, set projection to Mercator 3395 
Satellite <- as.telemetry(sat_all, 
                          projection = CRS("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")) 
# Minimum sampling interval of 15 seconds in 169267
# Minimum sampling interval of 3 seconds in 169268
# Minimum sampling interval of 1.67 minutes in 169269
# Minimum sampling interval of 21 seconds in 169270
# Minimum sampling interval of 1 minutes in 169271
# Minimum sampling interval of 19 seconds in 169272
# Minimum sampling interval of 15 seconds in 169273
# Minimum sampling interval of 26 seconds in 169274


# IDENTIFY AND REMOVE OUTLIERS #
# remove any locations that violate 2 m/s travel speed

# Turtle A 
outAs <- outlie(Satellite[[1]], plot = T)
plot(outAs,units=FALSE)
badAs <- outAs$speed > 2.0 
Satellite[[1]] <- Satellite[[1]][!badAs,] # Removed 96 points

# Turtle B
outBs <- outlie(Satellite[[5]], plot = T)
plot(outBs,units=FALSE)
badBs <- outBs$speed > 2.0 
Satellite[[5]] <- Satellite[[5]][!badBs,]

# Turtle C
outCs <- outlie(Satellite[[3]], plot = T)
plot(outCs,units=FALSE)
badCs <- outCs$speed > 2.0 
Satellite[[3]] <- Satellite[[3]][!badCs,]

# Turtle D
outDs <- outlie(Satellite[[6]], plot = T)
plot(outDs,units=FALSE)
badDs <- outDs$speed > 2.0 
Satellite[[6]] <- Satellite[[6]][!badDs,]

# Turtle E
outEs <- outlie(Satellite[[7]], plot = T)
plot(outEs,units=FALSE)
badEs <- outEs$speed > 2.0 
Satellite[[7]] <- Satellite[[7]][!badEs,]

# Turtle G
outGs <- outlie(Satellite[[2]], plot = T)
plot(outGs,units=FALSE)
badGs <- outGs$speed > 2.0 
Satellite[[2]] <- Satellite[[2]][!badGs,]

# Turtle H
outHs <- outlie(Satellite[[8]], plot = T)
plot(outHs,units=FALSE)
badHs <- outHs$speed > 2.0 
Satellite[[8]] <- Satellite[[8]][!badHs,]

# Turtle I
outIs <- outlie(Satellite[[4]], plot = T)
plot(outIs,units=FALSE)
badIs <- outIs$speed > 2.0
Satellite[[4]] <- Satellite[[4]][!badIs,]


plot(Satellite, col = rainbow(length(Satellite))) # plot all to visualize
plot(Satellite[[1]]) # plot single individual to visualize 

# separate individuals
As <- Satellite[[1]]  # 169267
Bs <- Satellite[[5]]  # 169271
Cs <- Satellite[[3]]  # 169269
Ds <- Satellite[[6]]  # 169272
Es <- Satellite[[7]]  # 169273
Gs <- Satellite[[2]]  # 169268
Hs <- Satellite[[8]]  # 169274
Is <- Satellite[[4]]  # 169270


## IMPORT SATELLITE SUBSET DATA ##
sat_sub <- read.csv("sat_all_sub.csv")
sat_sub <- sat_sub %>% # rename columns to match what is needed for {ctmm} package
  rename(tag.local.identifier = id, 
         timestamp = date, 
         Argos.location.class = lc, 
         location.lat = lat, 
         location.long = long, 
         Argos.semi.major = smaj, 
         Argos.semi.minor = smin, 
         Argos.orientation = eor)
sat_sub <- sat_sub %>% # only columns that are needed (remove LCs to avoid confusion)
  dplyr::select(-c(X, Argos.location.class))
sat_sub$timestamp <- as.POSIXct(sat_sub$timestamp, format = "%m/%d/%Y %H:%M:%S", tz = "America/New_York") # convert datetime to proper format

# Make into telemetry object 
Satellite_sub <- as.telemetry(sat_sub,
                              projection = CRS("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")) 
# Minimum sampling interval of 15 seconds in 169267
# Minimum sampling interval of 3 seconds in 169268
# Minimum sampling interval of 1.67 minutes in 169269
# Minimum sampling interval of 33 seconds in 169270
# Minimum sampling interval of 1 minutes in 169271
# Minimum sampling interval of 19 seconds in 169272
# Minimum sampling interval of 15 seconds in 169273
# Minimum sampling interval of 26 seconds in 169274

# Identify and remove speed outliers
# Turtle A 
outAss <- outlie(Satellite_sub[[1]], plot = T)
plot(outAss,units=FALSE)
badAss <- outAss$speed > 2.0 
Satellite_sub[[1]] <- Satellite_sub[[1]][!badAss,] 

# Turtle B
outBss <- outlie(Satellite_sub[[5]], plot = T)
plot(outBss,units=FALSE)
badBss <- outBss$speed > 2.0 
Satellite_sub[[5]] <- Satellite_sub[[5]][!badBss,]

# Turtle C
outCss <- outlie(Satellite_sub[[3]], plot = T)
plot(outCss,units=FALSE)
badCss <- outCss$speed > 2.0 
Satellite_sub[[3]] <- Satellite_sub[[3]][!badCss,]

# Turtle D
outDss <- outlie(Satellite_sub[[6]], plot = T)
plot(outDss,units=FALSE)
badDss <- outDss$speed > 2.0 
Satellite_sub[[6]] <- Satellite_sub[[6]][!badDss,]

# Turtle E
outEss <- outlie(Satellite_sub[[7]], plot = T)
plot(outEss,units=FALSE)
badEss <- outEss$speed > 2.0 
Satellite_sub[[7]] <- Satellite_sub[[7]][!badEss,]

# Turtle G
outGss <- outlie(Satellite_sub[[2]], plot = T)
plot(outGss,units=FALSE)
badGss <- outGss$speed > 2.0 
Satellite_sub[[2]] <- Satellite_sub[[2]][!badGss,]

# Turtle H
outHss <- outlie(Satellite_sub[[8]], plot = T)
plot(outHss,units=FALSE)
badHss <- outHss$speed > 2.0 
Satellite_sub[[8]] <- Satellite_sub[[8]][!badHss,]

# Turtle I
outIss <- outlie(Satellite_sub[[4]], plot = T)
plot(outIss,units=FALSE)
badIss <- outIss$speed > 2.0 
Satellite_sub[[4]] <- Satellite_sub[[4]][!badIss,]


plot(Satellite_sub, col = rainbow(length(Satellite_sub))) # plot all to visualize
plot(Satellite_sub[[1]]) # plot turtle A to visualize 

# separate individuals
Ass <- Satellite_sub[[1]]  # 169267
Bss <- Satellite_sub[[5]]  # 169271
Css <- Satellite_sub[[3]]  # 169269
Dss <- Satellite_sub[[6]]  # 169272
Ess <- Satellite_sub[[7]]  # 169273
Gss <- Satellite_sub[[2]]  # 169268
Hss <- Satellite_sub[[8]]  # 169274
Iss <- Satellite_sub[[4]]  # 169270


## IMPORT ACOUSTIC DATA ##
COA_all <- read.csv("COAall_loc.err.csv", header = T)
COA_all <- COA_all %>% # rename columns to match what is needed for {ctmm} package
  rename(tag.local.identifier = Tag.ID, 
         timestamp = TimeStep.coa, 
         location.lat = Latitude.coa, 
         location.long = Longitude.coa, 
         eobs.horizontal.accuracy.estimate = Err) # set calculated error equal to e-obs
COA_all <- COA_all %>% # retain only columns that are needed
  dplyr::select(c(tag.local.identifier, timestamp, location.lat, location.long, eobs.horizontal.accuracy.estimate))
COA_all$timestamp<-as.POSIXct(COA_all$timestamp, format = "%m/%d/%Y %H:%M:%S", tz = "America/New_York") # convert datetime to proper format

# make telemetry object
Acoustic <- as.telemetry(COA_all, projection = CRS("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")) 
# Minimum sampling interval of 30 minutes in 64475
# Minimum sampling interval of 30 minutes in 64477
# Minimum sampling interval of 30 minutes in 64479
# Minimum sampling interval of 30 minutes in 64480
# Minimum sampling interval of 30 minutes in 64481
# Minimum sampling interval of 30 minutes in 64482
# Minimum sampling interval of 30 minutes in 64483
# Minimum sampling interval of 30 minutes in 64484

# separate individuals 
Aa <- Acoustic[[1]] 
Ca <- Acoustic[[2]] 
Da <- Acoustic[[3]]  
Ea <- Acoustic[[4]]  
Fa <- Acoustic[[5]]
Ga <- Acoustic[[6]]  
Ha <- Acoustic[[7]]  
Ia <- Acoustic[[8]] 


## IMPORT ACOUSTIC SUBSET DATA ## 
COA_sub <- read.csv("COAall_sub_loc.err.csv", header = T)
COA_sub <- COA_sub %>% # rename columns to match what is needed for {ctmm} package
  rename(tag.local.identifier = Tag.ID, 
         timestamp = TimeStep.coa, 
         location.lat = Latitude.coa, 
         location.long = Longitude.coa, 
         eobs.horizontal.accuracy.estimate = Err) # set calculated error equal to e-obs
COA_sub <- COA_sub %>% # retain only columns that are needed
  dplyr::select(c(tag.local.identifier, timestamp, location.lat, location.long, eobs.horizontal.accuracy.estimate))
COA_sub$timestamp <- as.POSIXct(COA_sub$timestamp, format = "%m/%d/%Y %H:%M:%S", tz = "America/New_York") # convert datetime to proper format

Acoustic_sub <- as.telemetry(COA_sub, 
                             projection = CRS("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")) 
# Minimum sampling interval of 30 minutes in 64475
# Minimum sampling interval of 30 minutes in 64477
# Minimum sampling interval of 30 minutes in 64479

# separate individuals 
Aas <- Acoustic_sub[[1]] 
Cas <- Acoustic_sub[[2]] 
Das <- Acoustic_sub[[3]]  


## IMPORT LAND POLYGON ##
# {ctmm} can accept impassable barriers as an argument when calculating ADKEs that masks the portion of the AKDE that falls within the specified barrier

# import land shapefile as a SpatialPolygonDataFrame
land <- readOGR(dsn = "bimini shapefile/bimini_shape.shp")
class(land)
land <- spTransform(land, CRS("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")) # transform to common 3395 CRS

# because we want to calculate the AKDE only over water, not land, we need to invert our shapefile - set the extent to something that should reasonably cover the entire study area and all AKDEs
searaster <- raster(
  xmn = -8915977,
  xmx = -8754342, 
  ymn = 2884244, 
  ymx = 3011888,
  crs = CRS("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"),
  resolution = 50)
sea <- invert_poly(land, boundaries = raster::extent(searaster))


## VARIOGRAMS ##
# plotting variograms can help to see whether space use reaches an asymptope and stabilizes 
# ctmm.guess facilitates a starting point/guess for the ctmm.fit function
# interactive = TRUE plots the variogram, shows a line for the best fit guess, and gives you sliders to move around for the parameters, shows the variogram with the model
# interactive = FALSE will identify a first guess at the best parameters for you

# Satellite Data #
ctmm.guess(As, interactive = TRUE) # some variation but overall seems to stabilize for a while but then increases at the end of the track
ctmm.guess(Bs, interactive = TRUE) # very stable but then increases at end 
ctmm.guess(Cs, interactive = TRUE) # 
ctmm.guess(Ds, interactive = TRUE) # looks good but increases at end
ctmm.guess(Es, interactive = TRUE) # lots of variation - a lot of movement at later dates, may need to segment tracks
ctmm.guess(Gs, interactive = TRUE) # lot of movement at end of track
ctmm.guess(Hs, interactive = TRUE) # shorter track, increases at end, variation
ctmm.guess(Is, interactive = TRUE) # o

# Satellite Subset Data #
ctmm.guess(Ass, interactive = TRUE) # 
ctmm.guess(Bss, interactive = TRUE) # 
ctmm.guess(Css, interactive = TRUE) # 
ctmm.guess(Dss, interactive = TRUE) # 
ctmm.guess(Ess, interactive = TRUE) # 
ctmm.guess(Gss, interactive = TRUE) # 
ctmm.guess(Hss, interactive = TRUE) # short
ctmm.guess(Iss, interactive = TRUE) # 

# Acoustic Data #
ctmm.guess(Aa, interactive = TRUE) # large jump at end when zoomed all the way out 
ctmm.guess(Ca, interactive = TRUE) # seems to stablize for about 3 months, then decreases and increases again
ctmm.guess(Da, interactive = TRUE) # very stable, variance has some high peaks at end 
ctmm.guess(Ea, interactive = TRUE) # completely disjointed, probably cannot do analysis due to short duration
ctmm.guess(Fa, interactive = TRUE)
ctmm.guess(Ga, interactive = TRUE)
ctmm.guess(Ha, interactive = TRUE) # completely disjointed, probably cannot do analysis due to short duration
ctmm.guess(Ia, interactive = TRUE) # 

# Acoustic Subset Data # 
ctmm.guess(Aas, interactive = TRUE) 
ctmm.guess(Cas, interactive = TRUE) 
ctmm.guess(Das, interactive = TRUE) 

# From the variograms, it appears that Turtles E and H do not have enough acoustic data for home range stabilization and therefore we will not estimate AKDEs for them


## ESTIMATE AKDEs ##

# calculate the AKDE - full explanation for Turtle A below, simplified function below for all turtles
akde_As <- akde(As,                  # the data 
                CTMM = fit_As,       # the movement model 
                res = 100,           # resolution relative to bandwidth
                grid = searaster,    # raster grid to calculate AKDE within
                weights = T,         # weighted AKDE
                SP = sea,            # inverted land raster
                SP.in = T)           # estimate within sea raster

summary(akde_As, units = TRUE, level.UD = 0.50) # 32.14524 (low 95% CI) 42.2221 (EST) 53.65182 (high 95% CI) km^2
summary(akde_As, units = TRUE, level.UD = 0.95) # 255.9982 336.2482 427.2721


## TURTLE A ## 
set.seed(2022)
# Satellite # 
guess_As <- ctmm.guess(As, interactive = F)
summary(guess_As) # OUF anistropic
fit_As <- ctmm.select(As, guess_As, IC = "AICc")
summary(fit_As) # OUF anistropic
akde_As <- akde(As, CTMM = fit_As, res = 100, grid = searaster, weights = T, SP = sea, SP.in = T)   
summary(akde_As, units = TRUE, level.UD = 0.50) # 32.14524 42.2221 53.65182 km c
summary(akde_As, units = TRUE, level.UD = 0.95) # 255.9982 336.2482 427.2721 km c

# Satellite Subset # 
guess_Ass <- ctmm.guess(Ass, interactive = F)
summary(guess_Ass) # OUF
fit_Ass <- ctmm.select(Ass, guess_Ass, IC = "AICc")
summary(fit_Ass) # 
akde_Ass <- akde(Ass, CTMM = fit_Ass, res = 100, grid = searaster, weights = T, SP = sea, SP.in = T)  
summary(akde_Ass, units = TRUE, level.UD = 0.50) # 32.14524 42.2221 53.65182 c
summary(akde_Ass, units = TRUE, level.UD = 0.95) # 255.9982 336.2482 427.2721 c

# Acoustic # 
guess_Aa <- ctmm.guess(Aa, interactive = F)
summary(guess_Aa) # OUF
fit_Aa <- ctmm.select(Aa, guess_Aa, IC = "AICc")
summary(fit_Aa) # 
akde_Aa <- akde(Aa, CTMM = fit_Aa, res = 100, grid = searaster, weights = T, SP = sea, SP.in = T)   
summary(akde_Aa, units = TRUE, level.UD = 0.50) # 12.34296 29.5398 54.11059 c
summary(akde_Aa, units = TRUE, level.UD = 0.95) # 76.65745 183.4604 336.0603 c

# Acoustic Subset #
guess_Aas <- ctmm.guess(Aas, interactive = F)
summary(guess_Aas) # OUF a
fit_Aas <- ctmm.select(Aas, guess_Aas, IC = "AICc")
summary(fit_Aas) # OU a
akde_Aas <- akde(Aas, CTMM = fit_Aas, res = 100, grid = searaster, weights = T, SP = sea, SP.in = T)
summary(akde_Aas, units = TRUE, level.UD = 0.50) # 33.78468 40.19571 47.15545 ***hectares c
summary(akde_Aas, units = TRUE, level.UD = 0.95) # 2.606575 3.101202 3.638164 c


## TURTLE B ## 

# Satellite # 
set.seed(2022)
guess_Bs <- ctmm.guess(Bs, interactive = F)
summary(guess_Bs) # OUF
fit_Bs <- ctmm.select(Bs, guess_Bs, IC = "AICc")
summary(fit_Bs) # 
akde_Bs <- akde(Bs, CTMM = fit_Bs, res = 100, grid = searaster, weights = T, SP = sea, SP.in = T)  
# Default grid size of 3.9375 minutes chosen for bandwidth(...,fast=TRUE).
summary(akde_Bs, units = TRUE, level.UD = 0.50) # 1.032431 1.114843 1.200375 c
summary(akde_Bs, units = TRUE, level.UD = 0.95) # 22.27702 24.05525 25.9008 c

# Satellite Subset # 
guess_Bss <- ctmm.guess(Bss, interactive = F)
summary(guess_Bss) # OUF
fit_Bss <- ctmm.select(Bss, guess_Bss, IC = "AICc")
summary(fit_Bss) # 
akde_Bss <- akde(Bss, CTMM = fit_Bss, res = 100, grid = searaster, weights = T, SP = sea, SP.in = T)
# Default grid size of 3.96875 minutes chosen for bandwidth(...,fast=TRUE).
summary(akde_Bss, units = TRUE, level.UD = 0.50) # 1.050046 1.134318 1.221797
summary(akde_Bss, units = TRUE, level.UD = 0.95) # 22.43665 24.23732 26.1065


## TURTLE C ## 

# Satellite # 
guess_Cs <- ctmm.guess(Cs, interactive = F)
summary(guess_Cs) #  OUF
fit_Cs <- ctmm.select(Cs, guess_Cs, IC = "AICc")
summary(fit_Cs) # 
akde_Cs <- akde(Cs, CTMM = fit_Cs, res = 100, grid = searaster, weights = T, SP = sea, SP.in = T)     
summary(akde_Cs, units = TRUE, level.UD = 0.50) # 19.84051 33.84105 51.5184 c
summary(akde_Cs, units = TRUE, level.UD = 0.95) # 138.6342 236.462 359.9812 c

# Satellite Subset # 
guess_Css <- ctmm.guess(Css, interactive = F)
summary(guess_Css) # OUF
fit_Css <- ctmm.select(Css, guess_Css, IC = "AICc")
summary(fit_Css) # 
akde_Css <- akde(Css, CTMM = fit_Css, res = 100, grid = searaster, weights = T, SP = sea, SP.in = T) 
summary(akde_Css, units = TRUE, level.UD = 0.50) # 21.66126 36.51576 55.18577 c
summary(akde_Css, units = TRUE, level.UD = 0.95) # 150.223 253.2404 382.7188 c

# Acoustic # 
guess_Ca <- ctmm.guess(Ca, interactive = F)
summary(guess_Ca) # OUF
fit_Ca <- ctmm.select(Ca, guess_Ca, IC = "AICc")
summary(fit_Ca) # 
akde_Ca <- akde(Ca, CTMM = fit_Ca, res = 100, grid = searaster, weights = T, SP = sea, SP.in = T)   
# Default grid size of 30 minutes chosen for bandwidth(...,fast=TRUE).
summary(akde_Ca, units = TRUE, level.UD = 0.50) # 9.747807 10.74875 11.79791 ***hectares c
summary(akde_Ca, units = TRUE, level.UD = 0.95) # 70.23486 77.44685 85.00624 ***hectares c

# Acoustic Subset #
guess_Cas <- ctmm.guess(Cas, interactive = F)
summary(guess_Cas) # OUF
fit_Cas <- ctmm.select(Cas, guess_Cas, IC = "AICc")
summary(fit_Cas) # 
akde_Cas <- akde(Cas, CTMM = fit_Cas, res = 100, grid = searaster, weights = T, SP = sea, SP.in = T)  
summary(akde_Cas, units = TRUE, level.UD = 0.50) # 8.762323 10.26632 11.88769 ***hectares c
summary(akde_Cas, units = TRUE, level.UD = 0.95) # 53.69573 62.91228 72.84806 ***hectares c


## TURTLE D ## 

# Satellite # 
guess_Ds <- ctmm.guess(Ds, interactive = F)
summary(guess_Ds) # OUF
fit_Ds <- ctmm.select(Ds, guess_Ds, IC = "AICc")
summary(fit_Ds) # 
akde_Ds <- akde(Ds, CTMM = fit_Ds, res = 100, grid = searaster, weights = T, SP = sea, SP.in = T)     
summary(akde_Ds, units = TRUE, level.UD = 0.50) # 4.383402 5.018646 5.696247 c
summary(akde_Ds, units = TRUE, level.UD = 0.95) # 44.06914 50.45565 57.26801 c

# Satellite Subset # 
guess_Dss <- ctmm.guess(Dss, interactive = F)
summary(guess_Dss) # OUF
fit_Dss <- ctmm.select(Dss, guess_Dss, IC = "AICc")
summary(fit_Dss) # 
akde_Dss <- akde(Dss, CTMM = fit_Dss, res = 100, grid = searaster, weights = T, SP = sea, SP.in = T)  
summary(akde_Dss, units = TRUE, level.UD = 0.50) # 4.350365 4.988116 5.668851 c
summary(akde_Dss, units = TRUE, level.UD = 0.95) # 44.86773 51.44521 58.46601 c

# Acoustic # 
guess_Da <- ctmm.guess(Da, interactive = F)
summary(guess_Da) # OUF
fit_Da <- ctmm.select(Da, guess_Da, IC = "AICc")
summary(fit_Da) # 
akde_Da <- akde(Da, CTMM = fit_Da, res = 100, grid = searaster, weights = T, SP = sea, SP.in = T)   
summary(akde_Da, units = TRUE, level.UD = 0.50) # 5.541887 6.024715 6.52742 ***hectares c
summary(akde_Da, units = TRUE, level.UD = 0.95) # 51.56354 56.05592 60.73326 ***hectares c

# Acoustic Subset #
guess_Das <- ctmm.guess(Das, interactive = F)
summary(guess_Das) # OUF
fit_Das <- ctmm.select(Das, guess_Das, IC = "AICc")
summary(fit_Das) # 
akde_Das <- akde(Das, CTMM = fit_Das, res = 100, grid = searaster, weights = T, SP = sea, SP.in = T)  
summary(akde_Das, units = TRUE, level.UD = 0.50) # 5.979832 6.535519 7.115541 ***hectares c
summary(akde_Das, units = TRUE, level.UD = 0.95) # 47.75397 52.1916 56.82356 ***hectares c


## TURTLE E ##

# Satellite # 
guess_Es <- ctmm.guess(Es, interactive = F)
summary(guess_Es) #  OUF
fit_Es <- ctmm.select(Es, guess_Es, IC = "AICc")
summary(fit_Es) # 
akde_Es <- akde(Es, CTMM = fit_Es, res = 100, grid = searaster, weights = T, SP = sea, SP.in = T)   
summary(akde_Es, units = TRUE, level.UD = 0.50) # 117.8844 138.9972 161.8234 c
summary(akde_Es, units = TRUE, level.UD = 0.95) # 1646.669 1941.583 2260.431 c

# Satellite Subset # 
guess_Ess <- ctmm.guess(Ess, interactive = F)
summary(guess_Ess) # OUF
fit_Ess <- ctmm.select(Ess, guess_Ess, IC = "AICc")
summary(fit_Ess) # 
akde_Ess <- akde(Ess, CTMM = fit_Ess, res = 100, grid = searaster, weights = T, SP = sea, SP.in = T)
summary(akde_Ess, units = TRUE, level.UD = 0.50) # 121.2712 165.3414 216.1364 c 
summary(akde_Ess, units = TRUE, level.UD = 0.95) # 1145.151 1561.301 2040.953 c

# Acoustic 
guess_Ea <- ctmm.guess(Ea, interactive = F)
summary(guess_Ea) #  OUF a
fit_Ea <- ctmm.select(Ea, guess_Ea, IC = "AICc")
summary(fit_Ea) # 
akde_Ea <- akde(Ea, CTMM = fit_Ea, res = 100, grid = searaster, weights = T, SP = sea, SP.in = T)   
summary(akde_Ea, units = TRUE, level.UD = 0.50) # 
summary(akde_Ea, units = TRUE, level.UD = 0.95) # 


## TURTLE F ## 

# Acoustic # 
guess_Fa <- ctmm.guess(Fa, interactive = F)
summary(guess_Fa) # OUF
fit_Fa <- ctmm.select(Fa, guess_Fa, IC = "AICc")
summary(fit_Fa) # 
akde_Fa <- akde(Fa, CTMM = fit_Fa, res = 100, grid = searaster, weights = T, SP = sea, SP.in = T)     
summary(akde_Fa, units = TRUE, level.UD = 0.50) # 7.929984 8.917498 9.962122 ***hectares c
summary(akde_Fa, units = TRUE, level.UD = 0.95) # 53.79313 60.49194 67.57816 ***hectares c


## TURTLE G ## 

# Satellite # 
guess_Gs <- ctmm.guess(Gs, interactive = F)
summary(guess_Gs) #  OUF
fit_Gs <- ctmm.select(Gs, guess_Gs, IC = "AICc")
summary(fit_Gs) # 
akde_Gs <- akde(Gs, CTMM = fit_Gs, res = 100, grid = searaster, weights = T, SP = sea, SP.in = T)     
summary(akde_Gs, units = TRUE, level.UD = 0.50) # 1.159572 1.239036 1.321095 c 
summary(akde_Gs, units = TRUE, level.UD = 0.95) # 21.92501 23.42749 24.97907 c

# Satellite Subset # 
guess_Gss <- ctmm.guess(Gss, interactive = F)
summary(guess_Gss) # OUF
fit_Gss <- ctmm.select(Gss, guess_Gss, IC = "AICc")
summary(fit_Gss) # 
akde_Gss <- akde(Gss, CTMM = fit_Gss, res = 100, grid = searaster, weights = T, SP = sea, SP.in = T)  
summary(akde_Gss, units = TRUE, level.UD = 0.50) # 1.309831 1.494392 1.690942 c
summary(akde_Gss, units = TRUE, level.UD = 0.95) # 17.54411 20.01616 22.64878 c

# Acoustic # 
guess_Ga <- ctmm.guess(Ga, interactive = F)
summary(guess_Ga) # OUF
fit_Ga <- ctmm.select(Ga, guess_Ga, IC = "AICc")
summary(fit_Ga) # 
akde_Ga <- akde(Ga, CTMM = fit_Ga, res = 100, grid = searaster, weights = T, SP = sea, SP.in = T)     
summary(akde_Ga, units = TRUE, level.UD = 0.50) # 5.926026 6.321025 6.728589 ***hectares c
summary(akde_Ga, units = TRUE, level.UD = 0.95) # 46.77827 49.89628 53.11346 ***hectares c


## TURTLE H ## 

# Satellite # 
guess_Hs <- ctmm.guess(Hs, interactive = F)
summary(guess_Hs) #  OUF
fit_Hs <- ctmm.select(Hs, guess_Hs, IC = "AICc")
summary(fit_Hs) # OU
akde_Hs <- akde(Hs, CTMM = fit_Hs, res = 100, grid = searaster, weights = T, SP = sea, SP.in = T)    
summary(akde_Hs, units = TRUE, level.UD = 0.50) # 36.87465 59.72057 87.98521  c
summary(akde_Hs, units = TRUE, level.UD = 0.95) # 283.9372 459.8523 677.4918 c

# Satellite Subset # 
guess_Hss <- ctmm.guess(Hss, interactive = F)
summary(guess_Hss) # OUF
fit_Hss <- ctmm.select(Hss, guess_Hss, IC = "AICc")
summary(fit_Hss) # IID
akde_Hss <- akde(Hss, CTMM = fit_Hss, res = 100, grid = searaster, weights = T, SP = sea, SP.in = T) 
summary(akde_Hss, units = TRUE, level.UD = 0.50) # 3.5973 5.699816 8.278403 c
summary(akde_Hss, units = TRUE, level.UD = 0.95) # 22.20954 35.19036 51.11041 c


## TURTLE I ## 

# Satellite # 
guess_Is <- ctmm.guess(Is, interactive = F)
summary(guess_Is) # OUF
fit_Is <- ctmm.select(Is, guess_Is, IC = "AICc")
summary(fit_Is) # OUF a
akde_Is <- akde(Is, CTMM = fit_Is, res = 100, grid = searaster, weights = T, SP = sea, SP.in = T) 
# Default grid size of 6.03771929824561 minutes chosen for bandwidth(...,fast=TRUE).
summary(akde_Is, units = TRUE, level.UD = 0.50) # 41.32131 51.17874 62.07481 c 
summary(akde_Is, units = TRUE, level.UD = 0.95) # 280.5689 347.5001 421.4837 c

# Satellite Subset # 
guess_Iss <- ctmm.guess(Iss, interactive = F)
summary(guess_Iss) # OUF
fit_Iss <- ctmm.select(Iss, guess_Iss, IC = "AICc")
summary(fit_Iss) # IID
akde_Iss <- akde(Iss, CTMM = fit_Iss, res = 100, grid = searaster, weights = T, SP = sea, SP.in = T)  
summary(akde_Iss, units = TRUE, level.UD = 0.50) # 69.24962 96.81941 128.937 c
summary(akde_Iss, units = TRUE, level.UD = 0.95) # 626.684 876.1806 1166.833 c

# Acoustic # 
guess_Ia <- ctmm.guess(Ia, interactive = F)
summary(guess_Ia) # OUF
fit_Ia <- ctmm.select(Ia, guess_Ia, IC = "AICc")
summary(fit_Ia) # OUF
akde_Ia <- akde(Ia, CTMM = fit_Ia, res = 100, grid = searaster, weights = T, SP = sea, SP.in = T)    
summary(akde_Ia, units = TRUE, level.UD = 0.50) # 11.44052 58.25881 142.4341
summary(akde_Ia, units = TRUE, level.UD = 0.95) # 46.77414 238.1889 582.3363

# review selected models for each individual 
summary(fit_As) 
summary(fit_Ass)
summary(fit_Aa) 
summary(fit_Aas)
summary(fit_Bs) 
summary(fit_Bss) 
summary(fit_Cs) 
summary(fit_Css) 
summary(fit_Ca) 
summary(fit_Cas) 
summary(fit_Ds) 
summary(fit_Dss) 
summary(fit_Da) 
summary(fit_Das)
summary(fit_Es) 
summary(fit_Ess) 
summary(fit_Fa) 
summary(fit_Gs) 
summary(fit_Gss) 
summary(fit_Ga)
summary(fit_Hs) 
summary(fit_Hss) 
summary(fit_Is)
summary(fit_Iss)
summary(fit_Ia)

# not used in further anaylses, but included below is how to reconstruct the satellite tracks based on the model fit per individual 
pred_As <- predict(object = As, CTMM = fit_As, t = As[["timestamp"]]) 
# calculate SEs from the x and y COVs
pred_As$X.SE <- sqrt(pred_As[["COV.x.x"]])
pred_As$Y.SE <- sqrt(pred_As[["COV.y.y"]])
# make dataframe
pred_As <- data.frame(pred_As)
# calculate smallest location SE for each location 
pred_As$Location.Err <- pmin(pred_As$X.SE, pred_As$Y.SE)
# add back in timestamps 
pred_As$timestamp <- As[["timestamp"]]
# add in ID (so all can be merged in future steps for supplementary study)
pred_As$tag.local.identifier <- rep("X169267", 248)
pred_As <- pred_As %>%
  dplyr::select(c(tag.local.identifier, x, y, timestamp))

pred_Ass <- predict(object = Ass, CTMM = fit_Ass, t = Ass[["timestamp"]]) 
pred_Ass <- data.frame(pred_Ass)
pred_Ass$timestamp <- Ass[["timestamp"]]
pred_Ass$tag.local.identifier <- rep("X169267", 248)
pred_Ass <- pred_Ass %>%
  dplyr::select(c(tag.local.identifier, x, y, timestamp))

pred_Bs <- predict(object = Bs, CTMM = fit_Bs, t = Bs[["timestamp"]]) 
pred_Bs <- data.frame(pred_Bs)
pred_Bs$timestamp <- Bs[["timestamp"]]
pred_Bs$tag.local.identifier <- rep("X169271", 1270)
pred_Bs <- pred_Bs %>%
  dplyr::select(c(tag.local.identifier, x, y, timestamp))

pred_Bss <- predict(object = Bss, CTMM = fit_Bss, t = Bss[["timestamp"]]) 
pred_Bss <- data.frame(pred_Bss)
pred_Bss$timestamp <- Bss[["timestamp"]]
pred_Bss$tag.local.identifier <- rep("X169271", 1254)
pred_Bss <- pred_Bss %>%
  dplyr::select(c(tag.local.identifier, x, y, timestamp))

pred_Cs <- predict(object = Cs, CTMM = fit_Cs, t = Cs[["timestamp"]]) 
pred_Cs <- data.frame(pred_Cs)
pred_Cs$timestamp <- Cs[["timestamp"]]
pred_Cs$tag.local.identifier <- rep("X169269", 66)
pred_Cs <- pred_Cs %>%
  dplyr::select(c(tag.local.identifier, x, y, timestamp))

pred_Css <- predict(object = Css, CTMM = fit_Css, t = Css[["timestamp"]]) 
pred_Css <- data.frame(pred_Css)
pred_Css$timestamp <- Css[["timestamp"]]
pred_Css$tag.local.identifier <- rep("X169269", 59)
pred_Css <- pred_Css %>%
  dplyr::select(c(tag.local.identifier, x, y, timestamp))

pred_Ds <- predict(object = Ds, CTMM = fit_Ds, t = Ds[["timestamp"]]) 
pred_Ds <- data.frame(pred_Ds)
pred_Ds$timestamp <- Ds[["timestamp"]]
pred_Ds$tag.local.identifier <- rep("X169272", 472)
pred_Ds <- pred_Ds %>%
  dplyr::select(c(tag.local.identifier, x, y, timestamp))

pred_Dss <- predict(object = Dss, CTMM = fit_Dss, t = Dss[["timestamp"]]) 
pred_Dss <- data.frame(pred_Dss)
pred_Dss$timestamp <- Dss[["timestamp"]]
pred_Dss$tag.local.identifier <- rep("X169272", 459)
pred_Dss <- pred_Dss %>%
  dplyr::select(c(tag.local.identifier, x, y, timestamp))

pred_Es <- predict(object = Es, CTMM = fit_Es, t = Es[["timestamp"]]) 
pred_Es <- data.frame(pred_Es)
pred_Es$timestamp <- Es[["timestamp"]]
pred_Es$tag.local.identifier <- rep("X169273", 363)
pred_Es <- pred_Es %>%
  dplyr::select(c(tag.local.identifier, x, y, timestamp))

pred_Ess <- predict(object = Ess, CTMM = fit_Ess, t = Ess[["timestamp"]]) 
pred_Ess <- data.frame(pred_Ess)
pred_Ess$timestamp <- Ess[["timestamp"]]
pred_Ess$tag.local.identifier <- rep("X169273", 98)
pred_Ess <- pred_Ess %>%
  dplyr::select(c(tag.local.identifier, x, y, timestamp))

pred_Gs <- predict(object = Gs, CTMM = fit_Gs, t = Gs[["timestamp"]]) 
pred_Gs <- data.frame(pred_Gs)
pred_Gs$timestamp <- Gs[["timestamp"]]
pred_Gs$tag.local.identifier <- rep("X169268", 905)
pred_Gs <- pred_Gs %>%
  dplyr::select(c(tag.local.identifier, x, y, timestamp))

pred_Gss <- predict(object = Gss, CTMM = fit_Gss, t = Gss[["timestamp"]]) 
pred_Gss <- data.frame(pred_Gss)
pred_Gss$timestamp <- Gss[["timestamp"]]
pred_Gss$tag.local.identifier <- rep("X169268", 413)
pred_Gss <- pred_Gss %>%
  dplyr::select(c(tag.local.identifier, x, y, timestamp))

pred_Hs <- predict(object = Hs, CTMM = fit_Hs, t = Hs[["timestamp"]]) 
pred_Hs <- data.frame(pred_Hs)
pred_Hs$timestamp <- Hs[["timestamp"]]
pred_Hs$tag.local.identifier <- rep("X169274", 94)
pred_Hs <- pred_Hs %>%
  dplyr::select(c(tag.local.identifier, x, y, timestamp))

pred_Hss <- predict(object = Hss, CTMM = fit_Hss, t = Hss[["timestamp"]]) 
pred_Hss <- data.frame(pred_Hss)
pred_Hss$timestamp <- Hss[["timestamp"]]
pred_Hss$tag.local.identifier <- rep("X169274", 61)
pred_Hss <- pred_Hss %>%
  dplyr::select(c(tag.local.identifier, x, y, timestamp))

pred_Is <- predict(object = Is, CTMM = fit_Is, t = Is[["timestamp"]]) 
pred_Is <- data.frame(pred_Is)
pred_Is$timestamp <- Is[["timestamp"]]
pred_Is$tag.local.identifier <- rep("X169270", 937) 
pred_Is <- pred_Is %>%
  dplyr::select(c(tag.local.identifier, x, y, timestamp))

pred_Iss <- predict(object = Iss, CTMM = fit_Iss, t = Iss[["timestamp"]]) 
pred_Iss <- data.frame(pred_Iss)
pred_Iss$timestamp <- Iss[["timestamp"]]
pred_Iss$tag.local.identifier <- rep("X169270", 308)
pred_Iss <- pred_Iss %>%
  dplyr::select(c(tag.local.identifier, x, y, timestamp))


sat_pred <- rbind(pred_As, pred_Bs, pred_Cs, pred_Ds, pred_Es, pred_Gs, pred_Hs, pred_Is)
write.csv(sat_pred, "sat_ctmm.csv")

sat_pred_sub <- rbind(pred_Ass, pred_Bss, pred_Css, pred_Dss, pred_Ess, pred_Gss, pred_Hss, pred_Iss)
write.csv(sat_pred_sub, "sat_ctmm_sub.csv")

# Next Script: Range Distributions 