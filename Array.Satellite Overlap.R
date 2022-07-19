#### ARRAY.SATELLITE OVERLAP ####
# Previous Script: Overlap Indices

# Here we will estimate the coverage of the passive acoustic array and compare how much of the turtles' full satellite 95% UDs overlap with the array "listening range." We will look at both 50% detection probability ranges (standard definition) as well the maximum 1% probability. Additionally, the minimum convex polygon of the receivers will be calculated to assess what proportion of the satellite UDs falls within the bounds of the array. 
# This script uses the UDs calculated for each turtle in "Utilization Distributions" script. Please use that script to load/calculate UDs if not already done so

library(dplyr)
library(ggplot2)
library(sf)
library(sp)
library(tidyverse)
library(ggspatial)


## PREPARE DATA ## 

# This script uses the UDs calculated for each turtle in "Utilization Distributions" script. Please use that script to load/calculate UDs if not already done so

# if not already done, import receiver locations and study area 
recdata2 <- read.csv("rec_final.csv", header = TRUE)
recdata2 <- st_as_sf(recdata2, coords = c("lon", "lat"), crs = 4326)
recdata2 <- st_transform(recdata2, 3395)
recdata2$lon<-st_coordinates(recdata2)[,1]
recdata2$lat<-st_coordinates(recdata2)[,2]
Study_Area <- st_read(dsn = "bimini shapefile/bimini_shape.shp", quiet = TRUE)
land <- Study_Area[Study_Area$AltMode == "0", ] 
land <- st_union(land) 
land <- st_transform(land, 3395)

# split receivers into reef and sand receivers
reef <- recdata2 %>% filter(habitat == "reef")
sand <- recdata2 %>% filter(habitat == "bank") 


## 50% DETECTION RANGE OF ARRAY ##

reef1 <- st_buffer(reef, 185) # buffer each reef point by 185 m (based on logistic regression in "Detection Reassignment")
sand1 <- st_buffer(sand, 350) # buffer each bank point by 350 m 
reef1 <- st_union(reef1) # unite layers 
sand1 <- st_union(sand1) # unite layers 
reef1 <- st_make_valid(reef1)
sand1 <- st_make_valid(sand1)
array.50 <- st_union(reef1, sand1) # join back together
array.50 <- st_difference(array.50, land) # remove overlap between detection range and land 
st_area(array.50) # 13091822 [m^2] // 13.09 km2


## 1% DETECTION RANGE OF ARRAY ##

reef2 <- st_buffer(reef, 520) # buffer each reef point by 520 m (based on logistic regression in "Detection Reassignment")
sand2 <- st_buffer(sand, 1100) # buffer each bank point by 1100 m 
reef2 <- st_union(reef2) 
sand2 <- st_union(sand2) 
reef2 <- st_make_valid(reef2)
sand2 <- st_make_valid(sand2)
array.1 <- st_union(reef2, sand2)
array.1 <- st_difference(array.1, land)
st_area(array.1) # 95094844 [m^2] // 95.09 km2


## MCP OF ARRAY ##

# this code taken in part/modified from https://www.r-bloggers.com/2016/04/home-range-estimation-mcp/ contributed by Mitchell Gritts

# organize data
rec <- recdata2
rec <- st_drop_geometry(rec)
id <- "id"
rec[ , id] <- "a"
rec$x <- rec$lon
rec$y <- rec$lat
rec$x <- as.numeric(rec$x)
rec$y <- as.numeric(rec$y)
rec <- rec %>%
  dplyr::select(-c(station, habitat, lat, lon))

centroid <- apply(rec[, 2:3], 2, mean) # calculate centroid of all receiver locations 

plot(rec$x, rec$y, asp = 1) # visualize 
points(centroid[1], centroid[2], pch = 19, col = 'red')

d <- sqrt(((rec[, 2] - centroid[1])^2) + ((rec[, 3] - centroid[2])^2))
indx <- 1:length(d)
pct <- indx[d <= quantile(d, 1)]
mcp.pts <- rec[pct, ]

brdr <- chull(mcp.pts[, 2], mcp.pts[, 3]) # convex hull algorithm to estimate MCP
rec.brdr <- mcp.pts[brdr, ] # extract points that are in the border
rec.brdr <- rbind(rec.brdr[nrow(rec.brdr), ], rec.brdr) # repeat first point as last point to close the border

plot(rec$x, rec$y, asp = 1) # visualize
points(centroid[1], centroid[2], pch = 19, col = 'red')
lines(rec.brdr[, 2], rec.brdr[, 3], col = 'green')

rec.border <- st_as_sf(rec.brdr, coords = c("x", "y"), crs = 3395) # convert border to sf object
rec.border <- st_combine(rec.border)
rec.border <- st_cast(rec.border, "POLYGON") # create polygon 
rec.border <- st_make_valid(rec.border)
plot(rec.border)
st_area(rec.border) # 698.678436 including land 

rec.mcp <- st_difference(rec.border, land) # remove overlap with land 
st_area(rec.mcp) # 674.828670 without land 
plot(rec.mcp)


## CALCULATE PROPORTION OF SATELLITE UDs WITHIN 50% DETECTION RANGE ##

# Turtle A
A.95 <- st_difference(UD.As.95, array.50) # polygon of 95% satellite UD falling outside of 50% array
(st_area(A.95)/st_area(UD.As.95))*100 # 95.3% of A's 95% satellite UD is outside of detection range 

A.50 <- st_difference(UD.As.50, array.50) 
(st_area(A.50)/st_area(UD.As.50))*100 # 93.0% of satellite UD is outside of array 

# Turtle B
B.95 <- st_difference(UD.Bs.95, array.50)
(st_area(B.95)/st_area(UD.Bs.95))*100 # 92.55864

B.50 <- st_difference(UD.Bs.50, array.50) 
(st_area(B.50)/st_area(UD.Bs.50))*100 # 100 

# Turtle C
C.95 <- st_difference(UD.Cs.95, array.50)
(st_area(C.95)/st_area(UD.Cs.95))*100 # 67.40401

C.50 <- st_difference(UD.Cs.50, array.50) 
(st_area(C.50)/st_area(UD.Cs.50))*100 # 82.64992

# Turtle D 
D.95 <- st_difference(UD.Ds.95, array.50)
(st_area(D.95)/st_area(UD.Ds.95))*100 # 90.12783

D.50 <- st_difference(UD.Ds.50, array.50) 
(st_area(D.50)/st_area(UD.Ds.50))*100 # 82.52657

# Turtle E
E.95 <- st_difference(UD.Es.95, array.50)
plot(E.95)
(st_area(E.95)/st_area(UD.Es.95))*100 # 98.79599

E.50 <- st_difference(UD.Es.50, array.50) 
(st_area(E.50)/st_area(UD.Es.50))*100 # 92.08413

# Turtle G 
G.95 <- st_difference(UD.Gs.95, array.50)
(st_area(G.95)/st_area(UD.Gs.95))*100 # 91.61552

G.50 <- st_difference(UD.Gs.50, array.50) 
(st_area(G.50)/st_area(UD.Gs.50))*100 # 72.42645

# Turtle H 
H.95 <- st_difference(UD.Hs.95, array.50)
(st_area(H.95)/st_area(UD.Hs.95))*100 # 93.35526

H.50 <- st_difference(UD.Hs.50, array.50) 
(st_area(H.50)/st_area(UD.Hs.50))*100 # 88.76691

# Turtle I 
I.95 <- st_difference(UD.Is.95, array.50)
(st_area(I.95)/st_area(UD.Is.95))*100 # 94.74824

I.50 <- st_difference(UD.Is.50, array.50) 
(st_area(I.50)/st_area(UD.Is.50))*100 # 93.6686


## CALCULATE PROPORTION OF SATELLITE UDs WITHIN 1% DETECTION RANGE ##

# Turtle A 
A.95 <- st_difference(UD.As.95, array.1)
(st_area(A.95)/st_area(UD.As.95))*100 # 65.30513

A.50 <- st_difference(UD.As.50, array.1) 
(st_area(A.50)/st_area(UD.As.50))*100 # 49.50813 

# Turtle B 
B.95 <- st_difference(UD.Bs.95, array.1)
(st_area(B.95)/st_area(UD.Bs.95))*100 # 33.23017

B.50 <- st_difference(UD.Bs.50, array.1) 
(st_area(B.50)/st_area(UD.Bs.50))*100 # 57.4581

# Turtle C 
C.95 <- st_difference(UD.Cs.95, array.1)
(st_area(C.95)/st_area(UD.Cs.95))*100 # 0%

C.50 <- st_difference(UD.Cs.50, array.1) 
(st_area(C.50)/st_area(UD.Cs.50))*100 # 0%

# Turtle D
D.95 <- st_difference(UD.Ds.95, array.1)
(st_area(D.95)/st_area(UD.Ds.95))*100 # 36.78026

D.50 <- st_difference(UD.Ds.50, array.1) 
D.50 <- st_make_valid(D.50)
(st_area(D.50)/st_area(UD.Ds.50))*100 # 0

# Turtle E 
E.95 <- st_difference(UD.Es.95, array.1)
(st_area(E.95)/st_area(UD.Es.95))*100 # 91.40474

E.50 <- st_difference(UD.Es.50, array.1) 
(st_area(E.50)/st_area(UD.Es.50))*100 # 39.22377 

# Turtle G 
G.95 <- st_difference(UD.Gs.95, array.1)
(st_area(G.95)/st_area(UD.Gs.95))*100 # 40.14652

G.50 <- st_difference(UD.Gs.50, array.1) 
(st_area(G.50)/st_area(UD.Gs.50))*100 # 0

## Turtle H
H.95 <- st_difference(UD.Hs.95, array.1)
(st_area(H.95)/st_area(UD.Hs.95))*100 # 49.2749

H.50 <- st_difference(UD.Hs.50, array.1) 
(st_area(H.50)/st_area(UD.Hs.50))*100 # 36.3074

# Turtle I
I.95 <- st_difference(UD.Is.95, array.1)
(st_area(I.95)/st_area(UD.Is.95))*100 # 59.14723

I.50 <- st_difference(UD.Is.50, array.1) 
(st_area(I.50)/st_area(UD.Is.50))*100 # 41.2538


## CALCULATE PROPORTION OF SATELLITE UDs WITHIN ARRAY MCP ##

# Turtle A 
a.95 <- st_difference(UD.As.95, rec.mcp)
(st_area(a))/(st_area(UD.As.95)) # 0.08482267 = 8.5% 
a.50 <- st_difference(UD.As.50, rec.mcp)
(st_area(a.50))/(st_area(UD.As.50)) # 0 

# Turtle B 
b.95 <- st_difference(UD.Bs.95, rec.mcp)
(st_area(b))/(st_area(UD.Bs.95)) # 0 
b.50 <- st_difference(UD.Bs.50, rec.mcp)
(st_area(b.50))/(st_area(UD.Bs.50)) # 0 

# Turtle C 
c.95 <- st_difference(UD.Cs.95, rec.mcp)
(st_area(c))/(st_area(UD.Cs.95)) # 0
c.50 <- st_difference(UD.Cs.50, rec.mcp)
(st_area(c.50))/(st_area(UD.Cs.50)) # 0

# Turtle D 
d.95 <- st_difference(UD.Ds.95, rec.mcp)
(st_area(d))/(st_area(UD.Ds.95)) # 0 
d.50 <- st_difference(UD.Ds.50, rec.mcp)
(st_area(d.50))/(st_area(UD.Ds.50)) # 0

# Turtle E 
e.95 <- st_difference(UD.Es.95, rec.mcp)
(st_area(e))/(st_area(UD.Es.95)) # 0.6510252 = 65.1 %
e.50 <- st_difference(UD.Es.50, rec.mcp)
(st_area(e.50))/(st_area(UD.Es.50)) # 0

# Turtle G 
g.95 <- st_difference(UD.Gs.95, rec.mcp)
(st_area(g))/(st_area(UD.Gs.95)) # 0
g.50 <- st_difference(UD.Gs.50, rec.mcp) 
(st_area(g.50))/(st_area(UD.Gs.50)) # 0

# Turtle H 
h.95 <- st_difference(UD.Hs.95, rec.mcp)
(st_area(h))/(st_area(UD.Hs.95)) # 0.1186101 = 11.9 %
h.50 <- st_difference(UD.Hs.50, rec.mcp)
(st_area(h.50))/(st_area(UD.Hs.50)) # 0

# Turtle I 
i.95 <- st_difference(UD.Is.95, rec.mcp)
(st_area(i))/(st_area(UD.Is.95)) # 0.130216 = 13.0 %
i.50 <- st_difference(UD.Is.50, rec.mcp)
(st_area(i.50))/(st_area(UD.Is.50)) # 0.001078226 = 0.11%


## MAPS OF OVERLAP BETWEEN ARRAY AND 95% SATELLITE UDs ## 
# Maps joined in outside software

## 50% detection range map 
array.50_plot <- ggplot() + 
  geom_sf(data = UD.As.95, col = "deepskyblue1", lwd = 0.25, alpha = 0.3, aes(fill = "deepskyblue1")) +
  geom_sf(data = UD.Bs.95, col = "deepskyblue1", lwd = 0.25,  alpha = 0.3, aes(fill = "deepskyblue1")) +
  geom_sf(data = UD.Cs.95, col = "deepskyblue1", lwd = 0.25,  alpha = 0.3, aes(fill = "deepskyblue1")) +
  geom_sf(data = UD.Ds.95, col = "deepskyblue1", lwd = 0.25,  alpha = 0.3, aes(fill = "deepskyblue1")) +
  geom_sf(data = UD.Es.95, col = "deepskyblue1", lwd = 0.25,  alpha = 0.3, aes(fill = "deepskyblue1")) +
  geom_sf(data = UD.Gs.95, col = "deepskyblue1", lwd = 0.25,  alpha = 0.3, aes(fill = "deepskyblue1")) +
  geom_sf(data = UD.Hs.95, col = "deepskyblue1", lwd = 0.25,  alpha = 0.3, aes(fill = "deepskyblue1")) +
  geom_sf(data = UD.Is.95, col = "deepskyblue1", lwd = 0.25, alpha = 0.3, aes(fill = "deepskyblue1", col = "deepskyblue1"), show.legend = "polygon") +
  geom_sf(data = land, fill = "grey", col = "grey") +
  geom_sf(data = array.50, aes(fill = "grey87", col = "grey28"), col = "grey28", alpha = 0.7, lwd = 0.25, show.legend = "polygon") +
  coord_sf(xlim = c(-8849899.51, -8799805.74), ylim = c(2912763.26, 2968042.41)) +
  scale_x_continuous(breaks = c(-79.2, -79.4)) +
  scale_y_continuous(breaks = c(25.6, 25.8)) +
  annotation_scale(style = "ticks", location = "br", text_cex = 1.6, line_width = 1.75, height = unit(0.60, "cm"), text_pad = unit(0.35, "cm"), pad_x = unit(0.55, "cm"), pad_y = unit(0.55, "cm")) + 
  theme_bw() +
  theme(text = element_text(size = 30)) +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5)) + 
  theme(axis.ticks.y = element_line(size = 0.5)) + 
  theme(axis.ticks.x = element_line(size = 0.5)) +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  annotate("text", x = -8849899.51, y = 2968042.41, label = "a)", size = 13, hjust = 0, vjust = 1) + 
  scale_fill_manual(values = c("deepskyblue1" = "deepskyblue1", "grey87" = "grey87"), labels = c(deepskyblue1 = "Satellite 95% UDs", grey87 = "Receiver Array Detection Range"), name = NULL) + 
  theme(legend.position = "bottom")

ggsave(filename = "array.50_plot.tiff", plot = array.50_plot, device = "tiff",path = "/Users/EmilyHardin/Desktop/Research/Acous.vs.Sat", width = 240, height = 240, units = c("mm"), dpi = 600)

# 1% detection range map 
array.1_plot <- ggplot() + 
  geom_sf(data = UD.As.95, col = "deepskyblue1", lwd = 0.25, alpha = 0.3, aes(fill = "deepskyblue1")) +
  geom_sf(data = UD.Bs.95, col = "deepskyblue1", lwd = 0.25,  alpha = 0.3, aes(fill = "deepskyblue1")) +
  geom_sf(data = UD.Cs.95, col = "deepskyblue1", lwd = 0.25,  alpha = 0.3, aes(fill = "deepskyblue1")) +
  geom_sf(data = UD.Ds.95, col = "deepskyblue1", lwd = 0.25,  alpha = 0.3, aes(fill = "deepskyblue1")) +
  geom_sf(data = UD.Es.95, col = "deepskyblue1", lwd = 0.25,  alpha = 0.3, aes(fill = "deepskyblue1")) +
  geom_sf(data = UD.Gs.95, col = "deepskyblue1", lwd = 0.25,  alpha = 0.3, aes(fill = "deepskyblue1")) +
  geom_sf(data = UD.Hs.95, col = "deepskyblue1", lwd = 0.25,  alpha = 0.3, aes(fill = "deepskyblue1")) +
  geom_sf(data = UD.Is.95, col = "deepskyblue1", lwd = 0.25, alpha = 0.3, aes(fill = "deepskyblue1", col = "deepskyblue1"), show.legend = "polygon") +
  geom_sf(data = land, fill = "grey", col = "grey") +
  geom_sf(data = array.1, aes(fill = "grey87", col = "grey28"), col = "grey28", alpha = 0.7, lwd = 0.25, show.legend = "polygon") +
  coord_sf(xlim = c(-8849899.51, -8799805.74), ylim = c(2912763.26, 2968042.41)) +
  scale_x_continuous(breaks = c(-79.2, -79.4)) +
  scale_y_continuous(breaks = c(25.6, 25.8)) +
  annotation_scale(style = "ticks", location = "br", text_cex = 1.6, line_width = 1.75, height = unit(0.60, "cm"), text_pad = unit(0.35, "cm"), pad_x = unit(0.55, "cm"), pad_y = unit(0.55, "cm")) + 
  theme_bw() +
  theme(text = element_text(size = 30)) +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5)) + 
  theme(axis.ticks.y = element_line(size = 0.5)) + 
  theme(axis.ticks.x = element_line(size = 0.5)) +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  annotate("text", x = -8849899.51, y = 2968042.41, label = "b)", size = 13, hjust = 0, vjust = 1) + 
  scale_fill_manual(values = c("deepskyblue1" = "deepskyblue1", "grey87" = "grey87"), labels = c(deepskyblue1 = "Satellite 95% UDs", grey87 = "Receiver Array Detection Range"), name = NULL) + 
  theme(legend.position = "bottom")

ggsave(filename = "array.1_plot.tiff", plot = array.1_plot, device = "tiff",path = "/Users/EmilyHardin/Desktop/Research/Acous.vs.Sat", width = 240, height = 240, units = c("mm"), dpi = 600)

# MCP map
mcp.plot <- ggplot() + 
  geom_sf(data = UD.As.95, col = "deepskyblue1", lwd = 0.25, alpha = 0.3, aes(fill = "deepskyblue1")) +
  geom_sf(data = UD.Bs.95, col = "deepskyblue1", lwd = 0.25,  alpha = 0.3, aes(fill = "deepskyblue1")) +
  geom_sf(data = UD.Cs.95, col = "deepskyblue1", lwd = 0.25,  alpha = 0.3, aes(fill = "deepskyblue1")) +
  geom_sf(data = UD.Ds.95, col = "deepskyblue1", lwd = 0.25,  alpha = 0.3, aes(fill = "deepskyblue1")) +
  geom_sf(data = UD.Es.95, col = "deepskyblue1", lwd = 0.25,  alpha = 0.3, aes(fill = "deepskyblue1")) +
  geom_sf(data = UD.Gs.95, col = "deepskyblue1", lwd = 0.25,  alpha = 0.3, aes(fill = "deepskyblue1")) +
  geom_sf(data = UD.Hs.95, col = "deepskyblue1", lwd = 0.25,  alpha = 0.3, aes(fill = "deepskyblue1")) +
  geom_sf(data = UD.Is.95, col = "deepskyblue1", lwd = 0.25, alpha = 0.3, aes(fill = "deepskyblue1", col = "deepskyblue1"), show.legend = "polygon") +
  geom_sf(data = land, fill = "grey", col = "grey") +
  geom_sf(data = rec.border, aes(col = "black"), col = "black", fill = "grey100", lwd = 0.3, alpha = 0, show.legend = "polygon") +
  geom_point(data = recdata2, aes(x = lon, y = lat), col = "grey28", size = 0.8) +
  coord_sf(xlim = c(-8854630.60, -8791456.79), ylim = c(2912763.26, 2983435.41)) +
  scale_x_continuous(breaks = c(-79.1, -79.3, -79.5)) +
  scale_y_continuous(breaks = c(25.6, 25.8, 26.0)) +
  annotation_scale(style = "ticks", location = "br", text_cex = 1.6, line_width = 1.75, height = unit(0.60, "cm"), text_pad = unit(0.35, "cm"), pad_x = unit(0.55, "cm"), pad_y = unit(0.55, "cm")) +
  theme_bw() +
  theme(text = element_text(size = 30)) +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5)) + 
  theme(axis.ticks.y = element_line(size = 0.5)) + 
  theme(axis.ticks.x = element_line(size = 0.5)) +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  annotate("text", x = -8854630.60, y = 2983435.41, label = "c)", size = 13, hjust = 0, vjust = 1) + 
  scale_fill_manual(values = c("deepskyblue1" = "deepskyblue1", "grey100" = "grey100"), labels = c(deepskyblue1 = "Satellite 95% UDs", grey100 = "Array MCP"), name = NULL) + 
  theme(legend.position = "bottom")

ggsave(filename = "mcp.plot.tiff", plot = mcp.plot, device = "tiff", path = "/Users/EmilyHardin/Desktop/Research/Acous.vs.Sat", width = 240, height = 240, units = c("mm"), dpi = 600)


## MAPS OF OVERLAP BETWEEN ARRAY AND 95% SATELLITE UDs ## 
# Maps joined in outside software

## 50% detection range map 
array.50_plot.50 <- ggplot() + ## Need to figure out legend = blue border around both polygons
  geom_sf(data = UD.As.50, col = "deepskyblue1", lwd = 0.25, alpha = 0.3, aes(fill = "deepskyblue1")) +
  geom_sf(data = UD.Bs.50, col = "deepskyblue1", lwd = 0.25,  alpha = 0.3, aes(fill = "deepskyblue1")) +
  geom_sf(data = UD.Cs.50, col = "deepskyblue1", lwd = 0.25,  alpha = 0.3, aes(fill = "deepskyblue1")) +
  geom_sf(data = UD.Ds.50, col = "deepskyblue1", lwd = 0.25,  alpha = 0.3, aes(fill = "deepskyblue1")) +
  geom_sf(data = UD.Es.50, col = "deepskyblue1", lwd = 0.25,  alpha = 0.3, aes(fill = "deepskyblue1")) +
  geom_sf(data = UD.Gs.50, col = "deepskyblue1", lwd = 0.25,  alpha = 0.3, aes(fill = "deepskyblue1")) +
  geom_sf(data = UD.Hs.50, col = "deepskyblue1", lwd = 0.25,  alpha = 0.3, aes(fill = "deepskyblue1")) +
  geom_sf(data = UD.Is.50, col = "deepskyblue1", lwd = 0.25, alpha = 0.3, aes(fill = "deepskyblue1", col = "deepskyblue1"), show.legend = "polygon") +
  geom_sf(data = land, fill = "grey", col = "grey") +
  geom_sf(data = array.50, aes(fill = "grey87", col = "grey28"), col = "grey28", alpha = 0.7, lwd = 0.25, show.legend = "polygon") +
  coord_sf(xlim = c(-8833201.59, -8804815.12), ylim = c(2921961.84, 2961889.83)) +
  scale_x_continuous(breaks = c(-79.1, -79.3)) +
  scale_y_continuous(breaks = c(25.6, 25.8)) +
  annotation_scale(style = "ticks", location = "br", text_cex = 1.6, line_width = 1.75, height = unit(0.60, "cm"), text_pad = unit(0.35, "cm"), pad_x = unit(0.55, "cm"), pad_y = unit(0.55, "cm")) + 
  theme_bw() +
  theme(text = element_text(size = 30)) +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5)) + 
  theme(axis.ticks.y = element_line(size = 0.5)) + 
  theme(axis.ticks.x = element_line(size = 0.5)) +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  annotate("text", x = -8833201.59, y = 2961889.83, label = "a)", size = 13, hjust = 0, vjust = 1) + 
  scale_fill_manual(values = c("deepskyblue1" = "deepskyblue1", "grey87" = "grey87"), labels = c(deepskyblue1 = "Satellite 50% UDs", grey87 = "Receiver Array Detection Range"), name = NULL) + 
  theme(legend.position = "bottom")

ggsave(filename = "array.50_plot.50.tiff", plot = array.50_plot.50, device = "tiff",path = "/Users/EmilyHardin/Desktop/Research/Acous.vs.Sat", width = 240, height = 240, units = c("mm"), dpi = 600)

# 1% detection range map 
array.1_plot.50 <- ggplot() + 
  geom_sf(data = UD.As.50, col = "deepskyblue1", lwd = 0.25, alpha = 0.3, aes(fill = "deepskyblue1")) +
  geom_sf(data = UD.Bs.50, col = "deepskyblue1", lwd = 0.25,  alpha = 0.3, aes(fill = "deepskyblue1")) +
  geom_sf(data = UD.Cs.50, col = "deepskyblue1", lwd = 0.25,  alpha = 0.3, aes(fill = "deepskyblue1")) +
  geom_sf(data = UD.Ds.50, col = "deepskyblue1", lwd = 0.25,  alpha = 0.3, aes(fill = "deepskyblue1")) +
  geom_sf(data = UD.Es.50, col = "deepskyblue1", lwd = 0.25,  alpha = 0.3, aes(fill = "deepskyblue1")) +
  geom_sf(data = UD.Gs.50, col = "deepskyblue1", lwd = 0.25,  alpha = 0.3, aes(fill = "deepskyblue1")) +
  geom_sf(data = UD.Hs.50, col = "deepskyblue1", lwd = 0.25,  alpha = 0.3, aes(fill = "deepskyblue1")) +
  geom_sf(data = UD.Is.50, col = "deepskyblue1", lwd = 0.25, alpha = 0.3, aes(fill = "deepskyblue1", col = "deepskyblue1"), show.legend = "polygon") +
  geom_sf(data = land, fill = "grey", col = "grey") +
  geom_sf(data = array.1, aes(fill = "grey87", col = "grey28"), col = "grey28", alpha = 0.7, lwd = 0.25, show.legend = "polygon") +
  coord_sf(xlim = c(-8833201.59, -8804815.12), ylim = c(2921961.84, 2961889.83)) +
  scale_x_continuous(breaks = c(-79.1, -79.3)) +
  scale_y_continuous(breaks = c(25.6, 25.8)) +
  annotation_scale(style = "ticks", location = "br", text_cex = 1.6, line_width = 1.75, height = unit(0.60, "cm"), text_pad = unit(0.35, "cm"), pad_x = unit(0.55, "cm"), pad_y = unit(0.55, "cm")) + 
  theme_bw() +
  theme(text = element_text(size = 30)) +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5)) + 
  theme(axis.ticks.y = element_line(size = 0.5)) + 
  theme(axis.ticks.x = element_line(size = 0.5)) +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  annotate("text", x = -8833201.59, y = 2961889.83, label = "b)", size = 13, hjust = 0, vjust = 1) + 
  scale_fill_manual(values = c("deepskyblue1" = "deepskyblue1", "grey87" = "grey87"), labels = c(deepskyblue1 = "Satellite 50% UDs", grey87 = "Receiver Array Detection Range"), name = NULL) + 
  theme(legend.position = "bottom")

ggsave(filename = "array.1_plot.50.tiff", plot = array.1_plot.50, device = "tiff",path = "/Users/EmilyHardin/Desktop/Research/Acous.vs.Sat", width = 240, height = 240, units = c("mm"), dpi = 600)

# MCP map
mcp.plot.50 <- ggplot() + 
  geom_sf(data = UD.As.50, col = "deepskyblue1", lwd = 0.25, alpha = 0.3, aes(fill = "deepskyblue1")) +
  geom_sf(data = UD.Bs.50, col = "deepskyblue1", lwd = 0.25,  alpha = 0.3, aes(fill = "deepskyblue1")) +
  geom_sf(data = UD.Cs.50, col = "deepskyblue1", lwd = 0.25,  alpha = 0.3, aes(fill = "deepskyblue1")) +
  geom_sf(data = UD.Ds.50, col = "deepskyblue1", lwd = 0.25,  alpha = 0.3, aes(fill = "deepskyblue1")) +
  geom_sf(data = UD.Es.50, col = "deepskyblue1", lwd = 0.25,  alpha = 0.3, aes(fill = "deepskyblue1")) +
  geom_sf(data = UD.Gs.50, col = "deepskyblue1", lwd = 0.25,  alpha = 0.3, aes(fill = "deepskyblue1")) +
  geom_sf(data = UD.Hs.50, col = "deepskyblue1", lwd = 0.25,  alpha = 0.3, aes(fill = "deepskyblue1")) +
  geom_sf(data = UD.Is.50, col = "deepskyblue1", lwd = 0.25, alpha = 0.3, aes(fill = "deepskyblue1", col = "deepskyblue1"), show.legend = "polygon") +
  geom_sf(data = land, fill = "grey", col = "grey") +
  geom_sf(data = rec.border, aes(col = "black"), col = "black", fill = "grey100", lwd = 0.3, alpha = 0, show.legend = "polygon") +
  geom_point(data = recdata2, aes(x = lon, y = lat), col = "grey28", size = 0.8) +
  coord_sf(xlim = c(-8838767.57, -8794239.77)) +
  scale_x_continuous(breaks = c(-79.1, -79.3, -79.5)) +
  scale_y_continuous(breaks = c(25.6, 25.8, 26.0)) +
  annotation_scale(style = "ticks", location = "br", text_cex = 1.6, line_width = 1.75, height = unit(0.60, "cm"), text_pad = unit(0.35, "cm"), pad_x = unit(0.55, "cm"), pad_y = unit(0.55, "cm")) +
  theme_bw() +
  theme(text = element_text(size = 30)) +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5)) + 
  theme(axis.ticks.y = element_line(size = 0.5)) + 
  theme(axis.ticks.x = element_line(size = 0.5)) +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  annotate("text", x = -8838767.57, y = 2986515.99, label = "c)", size = 13, hjust = 0, vjust = 1) + 
  scale_fill_manual(values = c("deepskyblue1" = "deepskyblue1", "grey100" = "grey100"), labels = c(deepskyblue1 = "Satellite 50% UDs", grey100 = "Array MCP"), name = NULL) + 
  theme(legend.position = "bottom")

ggsave(filename = "mcp.plot.50.tiff", plot = mcp.plot.50, device = "tiff", path = "/Users/EmilyHardin/Desktop/Research/Acous.vs.Sat", width = 240, height = 240, units = c("mm"), dpi = 600)




