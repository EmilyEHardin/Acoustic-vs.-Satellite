#### INDIVIDUAL dBBMMs ####
# Previous Script: Continuous Time SSM

# dBBMMs will be applied to each individual with the {move package} in order to get utilization distributions for the full extent of both the acoustic and satellite data, as well as the matching temporal duration for both acoustic and satellite data

library(doParallel)
library(dplyr)
library(lubridate)
library(move)
library(raster)
library(sf)
library(sp)
library(tidyverse)


## PREPARE DATA ##

# Satellite Data
sat <- read.csv("fG_noland.csv", header = TRUE)
sat$timestamp<-as.POSIXct(sat$date, format = "%m/%d/%Y %H:%M:%S", tz = "America/New_York")
sat <- sat %>%
  mutate(id = make.names(id)) %>%
  dplyr::select(id, timestamp, x, y, Location.Err) %>%
  filter(Location.Err < 3000) # filter out any transmissions with location errors > 3km 

# Satellite Subset
sat_sub <- read.csv("fG_sub_noland.csv", header = TRUE)
sat_sub$timestamp<-as.POSIXct(sat_sub$date, format = "%m/%d/%Y %H:%M:%S", tz = "America/New_York")
sat_sub <- sat_sub %>%
  mutate(id = make.names(id)) %>%
  dplyr::select(id, timestamp, x, y, Location.Err)  %>%
  filter(Location.Err < 3000)

# Acoustic Data 
COAall_loc <- read.csv("COAall_loc.err.csv", header = TRUE)
COAall_loc$timestamp<-as.POSIXct(COAall_loc$TimeStep.coa, format = "%m/%d/%Y %H:%M:%S", tz = "America/New_York")
COAall_loc <- COAall_loc %>%
  mutate(Tag.ID = make.names(Tag.ID)) %>%
  dplyr::select(Tag.ID, timestamp, Latitude.coa, Longitude.coa, Location.Err)

# Acoustic Subset
COAall_sub_loc <- read.csv("COAall_sub_loc.err.csv", header = TRUE)
COAall_sub_loc$timestamp<-as.POSIXct(COAall_sub_loc$TimeStep.coa, format = "%m/%d/%Y %H:%M:%S", tz = "America/New_York")
COAall_sub_loc <- COAall_sub_loc %>%
  mutate(Tag.ID = make.names(Tag.ID)) %>%
  dplyr::select(Tag.ID, timestamp, Latitude.coa, Longitude.coa, Location.Err)

# Check and remove turtles that don't have enough relocations 
  # For satellite data, the window will be set to 13, so need over 13 relocations 
    # average time step for satellite data is ~2 hrs, so 13 locations = ~24 hours
  # For acoustic data, the window will be set to 49, so need over 49 relocations 
    # time step for acoustic COAs is 30 minutes, so 49 locations = ~24 hours
checkS <- sat %>%
  group_by(id) %>%
  summarise(relocations = length(timestamp))
checkS # all good 

#satellite subsetted
checkS_sub <- sat_sub %>%
  group_by(id) %>%
  summarise(relocations = length(timestamp))
checkS_sub # all good 

# acoustic 
checkA <- COAall_loc %>%
  group_by(Tag.ID) %>%
  summarise(relocations = length(timestamp))
checkA 
# Turtle H - 64483 (169274) does not have enough relocations (13) to be used with a window of 49
COAall_loc <- COAall_loc %>%
  filter(Tag.ID != "X64483")
# Turtle E - 64480 does not have enough relocation (36) to be used with a window of 49
COAall_loc <- COAall_loc %>%
  filter(Tag.ID != "X64480")

# acoustic subsetted
checkAs <- COAall_sub_loc %>%
  group_by(Tag.ID) %>%
  summarise(relocations = length(timestamp))
checkAs #all good


## MAKE {move} OBJECTS AND SUBSET INDIVIDUALS ##

satellite <- move(x = sat$x, 
                  y = sat$y, 
                  time= sat$timestamp, 
                  proj = CRS("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"), 
                  data = sat, 
                  animal = sat$id, 
                  Location.Error = sat$Location.Err)

# satellite subsetted
satellite_sub <- move(x = sat_sub$x, 
                      y = sat_sub$y, 
                      time= sat_sub$timestamp, 
                      proj = CRS("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"), 
                      data = sat_sub, 
                      animal = sat_sub$id, 
                      Location.Error = sat_sub$Location.Err)

# acoustic 
acoustic <- move(x = COAall_loc$Longitude.coa, 
                 y = COAall_loc$Latitude.coa, 
                 time= COAall_loc$timestamp, 
                 proj = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"), 
                 data = COAall_loc, 
                 animal = COAall_loc$Tag.ID, 
                 Location.Error = COAall_loc$Location.Err)

# acoustic subsetted
acoustic_sub <- move(x = COAall_sub_loc$Longitude.coa, 
                     y = COAall_sub_loc$Latitude.coa, 
                     time= COAall_sub_loc$timestamp, 
                     proj = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"), 
                     data = COAall_sub_loc, 
                     animal = COAall_sub_loc$Tag.ID, 
                     Location.Error = COAall_sub_loc$Location.Err)

# Satellite data are already projected in World Mercator, but acoustic data still needs to be transformed 
acoustic <- spTransform(acoustic, CRSobj = "+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
acoustic_sub <- spTransform(acoustic_sub, CRSobj = "+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")

# Naming convention: Turtle ID + acoustic (a) or satellite data (s) + subset data (s) or full data ( )
As <- satellite[["X169267"]]
Gs <- satellite[["X169268"]]
Cs <- satellite[["X169269"]]
Is <- satellite[["X169270"]]
Bs <- satellite[["X169271"]]
Ds <- satellite[["X169272"]]
Es <- satellite[["X169273"]]
Hs <- satellite[["X169274"]]

Ass <- satellite_sub[["X169267"]]
Css <- satellite_sub[["X169269"]]
Bss <- satellite_sub[["X169271"]]
Dss <- satellite_sub[["X169272"]]
Gss <- satellite_sub[["X169268"]]
Iss <- satellite_sub[["X169270"]]
Ess <- satellite_sub[["X169273"]]
Hss <- satellite_sub[["X169274"]]

Aa <- acoustic[["X64475"]]
Ca <- acoustic[["X64477"]]
Da <- acoustic[["X64479"]]
Fa <- acoustic[["X64481"]]
Ga <- acoustic[["X64482"]]
Ia <- acoustic[["X64484"]]

Aas <- acoustic_sub[["X64475"]]
Cas <- acoustic_sub[["X64477"]]
Das <- acoustic_sub[["X64479"]]


## dBBMMs FOR EACH INDIVIDUAL ## 

# TURTLE A

# identify the extent of the rasters for all of the turtle's {move} objects
# from all rasters, identify the maxs and mins in the x and y directions 
# add/substract 20,000 to each to create a new, common raster for the turtle 
extent(As)
extent(Aa)
extent(Aas)
raster.A <- raster(
  xmn = -8855676, 
  xmx = -8794026, 
  ymn = 2917158, 
  ymx = 2967797, 
  crs = CRS("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"),
  resolution = 50)

registerDoParallel(detectCores() - 2) # identify the number of core processors on your computer, subtract 2
getDoParWorkers() # allows you to run code in parellel on the amount of core processors you have (minus 2)

# for tracks that have gaps in data > 24 hrs, we want the dBBMM to ignore the variance associated with those gaps 
# satellite variance
V.As <- move::brownian.motion.variance.dyn(As, 
                                           location.error=As@data[["Location.Err"]], 
                                           window.size=13, 
                                           margin=3)
V.As@interest[timeLag(As,"hours") > 24] <- FALSE
# satellite subset variance
V.Ass <- move::brownian.motion.variance.dyn(Ass, 
                                            location.error=Ass@data[["Location.Err"]], 
                                            window.size=13, 
                                            margin=3)
V.Ass@interest[timeLag(Ass,"hours") > 24] <- FALSE
# acoustic variance 
V.Aa <- move::brownian.motion.variance.dyn(Aa, 
                                           location.error=Aa@data[["Location.Err"]], 
                                           window.size=49, 
                                           margin=15)
V.Aa@interest[timeLag(Aa,"hours") > 24] <- FALSE
# acoustic subset variance 
V.Aas <- move::brownian.motion.variance.dyn(Aas, 
                                            location.error=Aas@data[["Location.Err"]], 
                                            window.size=49, 
                                            margin=15)
V.Aas@interest[timeLag(Aas,"hours") > 24] <- FALSE

# Run the dBBMM with the objects created above so that the variance in gaps is ignored
# satellite dBBMM
dBB.As <- brownian.bridge.dyn(V.As, 
                              ext = 0.5, 
                              raster = raster.A, 
                              location.error = As@data[["Location.Err"]],
                              window = 13, 
                              margin = 3)
# satellite subset dBBMM
dBB.Ass <- brownian.bridge.dyn(V.Ass, 
                               ext = 0.5, 
                               raster = raster.A, 
                               location.error = Ass@data[["Location.Err"]],
                               window = 13, 
                               margin = 3)
# acoustic dBBMM
dBB.Aa <- brownian.bridge.dyn(V.Aa, 
                              ext = 0.5, 
                              raster = raster.A, 
                              location.error = Aa@data[["Location.Err"]],
                              window = 49, 
                              margin = 15)
# acoustic subset dBBMM
dBB.Aas <- brownian.bridge.dyn(V.Aas, 
                               ext = 0.5, 
                               raster = raster.A, 
                               location.error = Aas@data[["Location.Err"]],
                               window = 49, 
                               margin = 15)

# save dBBMM objects 
save(x=dBB.Aa, file="Aa")
save(x=dBB.As, file="As")
save(x=dBB.Ass, file="Ass")
save(x=dBB.Aas, file="Aas")

# TURTLE B - only satellite data
extent(Bs)
raster.B <- raster(
  xmn = -8848214, 
  xmx = -8798377, 
  ymn = 2926943, 
  ymx = 2976908, 
  crs = CRS("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"),
  resolution = 50)

registerDoParallel(detectCores() - 2)
getDoParWorkers()

# variance 
V.Bs <- move::brownian.motion.variance.dyn(Bs, 
                                           location.error=Bs@data[["Location.Err"]], 
                                           window.size=13, 
                                           margin=3)
V.Bs@interest[timeLag(Bs,"hours") > 24] <- FALSE

V.Bss <- move::brownian.motion.variance.dyn(Bss, 
                                            location.error=Bss@data[["Location.Err"]], 
                                            window.size=13, 
                                            margin=3)
V.Bss@interest[timeLag(Bss,"hours") > 24] <- FALSE

# dBBMMs
dBB.Bs <- brownian.bridge.dyn(V.Bs, 
                              ext = 0.5, 
                              raster = raster.B, 
                              location.error = Bs@data[["Location.Err"]],
                              window = 13, 
                              margin = 3)
dBB.Bss <- brownian.bridge.dyn(V.Bss, 
                               ext = 0.5, 
                               raster = raster.B, 
                               location.error = Bss@data[["Location.Err"]],
                               window = 13, 
                               margin = 3)
save(x=dBB.Bs, file="Bs")
save(x=dBB.Bss, file="Bss")

# TURTLE C 
extent(Cs)
extent(Ca)
extent(Cas)
raster.C <- raster(
  xmn = -8848948, 
  xmx = -8803260, 
  ymn = 2921266, 
  ymx = 2963801, 
  crs = CRS("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"),
  resolution = 50)

registerDoParallel(detectCores() - 2) 
getDoParWorkers()

# variance 
V.Cs <- move::brownian.motion.variance.dyn(Cs, 
                                           location.error = Cs@data[["Location.Err"]], 
                                           window.size=13, 
                                           margin=3)
V.Cs@interest[timeLag(Cs,"hours") > 24] <- FALSE

V.Css <- move::brownian.motion.variance.dyn(Css, 
                                            location.error = Css@data[["Location.Err"]], 
                                            window.size=13, 
                                            margin=3)
V.Css@interest[timeLag(Css,"hours") > 24] <- FALSE

V.Ca <- move::brownian.motion.variance.dyn(Ca, 
                                           location.error = Ca@data[["Location.Err"]], 
                                           window.size=49, 
                                           margin=15)
V.Ca@interest[timeLag(Ca,"hours") > 24] <- FALSE

V.Cas <- move::brownian.motion.variance.dyn(Cas, 
                                            location.error = Cas@data[["Location.Err"]], 
                                            window.size=49, 
                                            margin=15)
V.Cas@interest[timeLag(Cas,"hours") > 24] <- FALSE

# dBBMMs
dBB.Cs <- brownian.bridge.dyn(V.Cs, 
                              ext = 0.5, 
                              raster = raster.C, 
                              location.error = Cs@data[["Location.Err"]],
                              window = 13, 
                              margin = 3)
dBB.Css <- brownian.bridge.dyn(V.Css, 
                               ext = 0.5, 
                               raster = raster.C, 
                               location.error = Css@data[["Location.Err"]],
                               window = 13, 
                               margin = 3)
dBB.Ca <- brownian.bridge.dyn(V.Ca, 
                              ext = 0.5, 
                              raster = raster.C, 
                              location.error = Ca@data[["Location.Err"]],
                              window = 49, 
                              margin = 15)
dBB.Cas <- brownian.bridge.dyn(V.Cas, 
                               ext = 0.5, 
                               raster = raster.C, 
                               location.error = Cas@data[["Location.Err"]],
                               window = 49, 
                               margin = 15)
save(x=dBB.Ca, file="Ca")
save(x=dBB.Cs, file="Cs")
save(x=dBB.Css, file="Css")
save(x=dBB.Cas, file="Cas")

# TURTLE D
extent(Ds)
extent(Da)
extent(Das)
raster.D <- raster(
  xmn = -8848433, 
  xmx = -8799326, 
  ymn = 2923510, 
  ymx = 2969144, 
  crs = CRS("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"),
  resolution = 50)

registerDoParallel(detectCores() - 2) 
getDoParWorkers()

# variance
V.Ds <- move::brownian.motion.variance.dyn(Ds, 
                                           location.error = Ds@data[["Location.Err"]], 
                                           window.size=13, 
                                           margin=3)
V.Ds@interest[timeLag(Ds,"hours") > 24] <- FALSE

V.Dss <- move::brownian.motion.variance.dyn(Dss, 
                                            location.error = Dss@data[["Location.Err"]], 
                                            window.size=13, 
                                            margin=3)
V.Dss@interest[timeLag(Dss,"hours") > 24] <- FALSE

V.Da <- move::brownian.motion.variance.dyn(Da, 
                                           location.error = Da@data[["Location.Err"]], 
                                           window.size=49, 
                                           margin=15)
V.Da@interest[timeLag(Da,"hours") > 24] <- FALSE

V.Das <- move::brownian.motion.variance.dyn(Das, 
                                            location.error = Das@data[["Location.Err"]], 
                                            window.size=49, 
                                            margin=15)
V.Das@interest[timeLag(Das,"hours") > 24] <- FALSE

# dBBMMs
dBB.Ds <- brownian.bridge.dyn(V.Ds, 
                              ext = 0.5, 
                              raster = raster.D, 
                              location.error = Ds@data[["Location.Err"]],
                              window = 13, 
                              margin = 3)
dBB.Dss <- brownian.bridge.dyn(V.Dss, 
                               ext = 0.5, 
                               raster = raster.D, 
                               location.error = Dss@data[["Location.Err"]],
                               window = 13, 
                               margin = 3)
dBB.Da <- brownian.bridge.dyn(V.Da, 
                              ext = 0.5, 
                              raster = raster.D, 
                              location.error = Da@data[["Location.Err"]],
                              window = 49, 
                              margin = 15)
dBB.Das <- brownian.bridge.dyn(V.Das, 
                               ext = 0.5, 
                               raster = raster.D, 
                               location.error = Das@data[["Location.Err"]],
                               window = 49, 
                               margin = 15)
save(x=dBB.Da, file="Da")
save(x=dBB.Ds, file="Ds")
save(x=dBB.Dss, file="Dss")
save(x=dBB.Das, file="Das")

# TURTLE E - not enough COAs to run dBBMM on acoustic data

extent(Es)
extent(Ess)
raster.E <- raster(
  xmn = -8915977, #added 40,000 instead of 20,000
  xmx = -8753983, 
  ymn = 2884244, 
  ymx = 3011888, 
  crs = CRS("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"),
  resolution = 50)

# variance
V.Es <- move::brownian.motion.variance.dyn(Es, 
                                           location.error = Es@data[["Location.Err"]], 
                                           window.size=13, 
                                           margin=3)
V.Es@interest[timeLag(Es,"hours") > 24] <- FALSE

V.Ess <- move::brownian.motion.variance.dyn(Ess, 
                                            location.error = Ess@data[["Location.Err"]], 
                                            window.size=13, 
                                            margin=3)
V.Ess@interest[timeLag(Ess,"hours") > 24] <- FALSE

registerDoParallel(detectCores() - 2) 
getDoParWorkers()

# dBBMMs
dBB.Es <- brownian.bridge.dyn(V.Es, 
                              ext = 0.5, 
                              raster = raster.E, 
                              location.error = Es@data[["Location.Err"]],
                              window = 13, 
                              margin = 3)
dBB.Ess <- brownian.bridge.dyn(V.Ess, 
                               ext = 0.5, 
                               raster = raster.E, 
                               location.error = Ess@data[["Location.Err"]],
                               window = 13, 
                               margin = 3)
save(x=dBB.Es, file="Es")
save(x=dBB.Ess, file="Ess")

# TURTLE F - only acoustic data 

extent(Fa)
raster.F <- raster(
  xmn = -8845745, 
  xmx = -8803237, 
  ymn = 2921496, 
  ymx = 2962924, 
  crs = CRS("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"),
  resolution = 50)

# variance
V.Fa <- move::brownian.motion.variance.dyn(Fa, 
                                           location.error = Fa@data[["Location.Err"]], 
                                           window.size=49, 
                                           margin=15)
V.Fa@interest[timeLag(Fa,"hours") > 24] <- FALSE

registerDoParallel(detectCores() - 2) 
getDoParWorkers()

# dBBMMs
dBB.Fa <- brownian.bridge.dyn(V.Fa, 
                              ext = 0.5, 
                              raster = raster.F, 
                              location.error = Fa@data[["Location.Err"]],
                              window = 49, 
                              margin = 15)
save(x=dBB.Fa, file="Fa")

# TURTLE G

extent(Gs)
extent(Gss)
extent(Ga)
raster.G <- raster(
  xmn = -8848608, 
  xmx = -8798927, 
  ymn = 2918522, 
  ymx = 2965853, 
  crs = CRS("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"),
  resolution = 50)

# variance
V.Gs <- move::brownian.motion.variance.dyn(Gs, 
                                           location.error = Gs@data[["Location.Err"]], 
                                           window.size=13, 
                                           margin=3)
V.Gs@interest[timeLag(Gs,"hours") > 24] <- FALSE

V.Gss <- move::brownian.motion.variance.dyn(Gss, 
                                            location.error = Gss@data[["Location.Err"]], 
                                            window.size=13, 
                                            margin=3)
V.Gss@interest[timeLag(Gss,"hours") > 24] <- FALSE

V.Ga <- move::brownian.motion.variance.dyn(Ga, 
                                           location.error = Ga@data[["Location.Err"]], 
                                           window.size=49, 
                                           margin=15)
V.Ga@interest[timeLag(Ga,"hours") > 24] <- FALSE

registerDoParallel(detectCores() - 2) 
getDoParWorkers()

# dBBMMs
dBB.Gs <- brownian.bridge.dyn(V.Gs, 
                              ext = 0.5, 
                              raster = raster.G, 
                              location.error = Gs@data[["Location.Err"]],
                              window = 13, 
                              margin = 3)
dBB.Gss <- brownian.bridge.dyn(V.Gss, 
                               ext = 0.5, 
                               raster = raster.G, 
                               location.error = Gss@data[["Location.Err"]],
                               window = 13, 
                               margin = 3)
dBB.Ga <- brownian.bridge.dyn(V.Ga, 
                              ext = 0.5, 
                              raster = raster.G, 
                              location.error = Ga@data[["Location.Err"]],
                              window = 49, 
                              margin = 15)
save(x=dBB.Ga, file="Ga")
save(x=dBB.Gs, file="Gs")
save(x=dBB.Gss, file="Gss")

# TURTLE H - not enough COAs to run dBBMM on acoustic data

extent(Hs)
extent(Hss)
raster.H <- raster(
  xmn = -8849174, 
  xmx = -8801643, 
  ymn = 2921337, 
  ymx = 2972214, 
  crs = CRS("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"),
  resolution = 50)

# variance
V.Hs <- move::brownian.motion.variance.dyn(Hs, 
                                           location.error = Hs@data[["Location.Err"]], 
                                           window.size=13, 
                                           margin=3)
V.Hs@interest[timeLag(Hs,"hours") > 24] <- FALSE

V.Hss <- move::brownian.motion.variance.dyn(Hss, 
                                            location.error = Hss@data[["Location.Err"]], 
                                            window.size=13, 
                                            margin=3)
V.Hss@interest[timeLag(Hss,"hours") > 24] <- FALSE

registerDoParallel(detectCores() - 2) 
getDoParWorkers()

# dBBMMs
dBB.Hs <- brownian.bridge.dyn(V.Hs, 
                              ext = 0.5, 
                              raster = raster.H, 
                              location.error = Hs@data[["Location.Err"]],
                              window = 13, 
                              margin = 3)
dBB.Hss <- brownian.bridge.dyn(V.Hss, 
                               ext = 0.5, 
                               raster = raster.H, 
                               location.error = Hss@data[["Location.Err"]],
                               window = 13, 
                               margin = 3)
save(x=dBB.Hs, file="Hs")
save(x=dBB.Hss, file="Hss")

# TURTLE I 
extent(Is)
extent(Iss)
extent(Ia)
raster.I <- raster(
  xmn = -8849621, 
  xmx = -8795207, 
  ymn = 2893294, 
  ymx = 2972769, 
  crs = CRS("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"),
  resolution = 50)

# variance
V.Is <- move::brownian.motion.variance.dyn(Is, 
                                           location.error = Is@data[["Location.Err"]], 
                                           window.size=13, 
                                           margin=3)
V.Is@interest[timeLag(Is,"hours") > 24] <- FALSE

V.Iss <- move::brownian.motion.variance.dyn(Iss, 
                                            location.error = Iss@data[["Location.Err"]], 
                                            window.size=13, 
                                            margin=3)
V.Iss@interest[timeLag(Iss,"hours") > 24] <- FALSE

V.Ia <- move::brownian.motion.variance.dyn(Ia, 
                                           location.error = Ia@data[["Location.Err"]], 
                                           window.size=49, 
                                           margin=15)
V.Ia@interest[timeLag(Ia,"hours") > 24] <- FALSE

registerDoParallel(detectCores() - 2) 
getDoParWorkers()

# dBBMMs
dBB.Is <- brownian.bridge.dyn(V.Is, 
                              ext = 0.5, 
                              raster = raster.I, 
                              location.error = Is@data[["Location.Err"]],
                              window = 13, 
                              margin = 3)
dBB.Iss <- brownian.bridge.dyn(V.Iss, 
                               ext = 0.5, 
                               raster = raster.I, 
                               location.error = Iss@data[["Location.Err"]],
                               window = 13, 
                               margin = 3)
dBB.Ia <- brownian.bridge.dyn(V.Ia, 
                              ext = 0.5, 
                              raster = raster.I, 
                              location.error = Ia@data[["Location.Err"]],
                              window = 49, 
                              margin = 15)
save(x=dBB.Ia, file="Ia")
save(x=dBB.Is, file="Is")
save(x=dBB.Iss, file="Iss")


# Next Script: Utilization Distributions 


