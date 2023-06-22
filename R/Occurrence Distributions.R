#### Occurrene Distributions ####
# Previous Script: Individual dBBMMs_ODs

# Here we will extract the 95% and 50% contours for the occurrence distributions. We will calculate their areas and map them
# Overall Steps:
  # 1. Import the dBBMM objects for the individual (most turtles have 3)
  # 2. use raster2contour to convert from dBBMM to spatial lines 
  # 3. convert to sf linestring object
  # 4. cast to a polygon or multipolygon depending on the turtle
  # 5. delete overlap with land
  # 6. Plot with ggplot
  # 7. Individual maps are combined with outside software for publication; the first map for Turtle A includes a legend used for all maps (not included in code for other maps)

library(dplyr)
library(ggplot2)
library(move)
library(raster)
library(sf)
library(sp)
library(tidyverse)
library(ggspatial)


## PREP FOR PLOTS ## 

# receiver information 
recdata2 <- read.csv("rec_final.csv", header = TRUE)
recdata2 <- st_as_sf(recdata2, coords = c("lon", "lat"), crs = 4326)
recdata2 <- st_transform(recdata2, 3395)
recdata2$lon<-st_coordinates(recdata2)[,1]
recdata2$lat<-st_coordinates(recdata2)[,2]
# Load the study area and get into the proper format for future steps
Study_Area <- st_read(dsn = "bimini shapefile/bimini_shape.shp", quiet = TRUE)
land <- Study_Area[Study_Area$AltMode == "0", ] # extract just one layer from the collection 
land <- st_union(land) # make it a polygon layer
land <- st_transform(land, 3395)


## TURTLE A ## 

load(file="Aa")
# Acoustic 95% 
UD.Aa.95 <- raster2contour(dBB.Aa, levels = 0.95) 
UD.Aa.95 <- st_as_sf(UD.Aa.95, coords = c("long", "lat"), crs = 3395)
UD.Aa.95 <- st_cast(UD.Aa.95, "POLYGON")
UD.Aa.95 <- st_make_valid(UD.Aa.95)
UD.Aa.95 <- st_difference(UD.Aa.95, land)
st_area(UD.Aa.95) 
# Acoustic 50% 
UD.Aa.50 <- raster2contour(dBB.Aa, levels = 0.50) 
UD.Aa.50 <- st_as_sf(UD.Aa.50, coords = c("long", "lat"), crs = 3395)
UD.Aa.50 <- st_cast(UD.Aa.50, "POLYGON") 
UD.Aa.50 <- st_make_valid(UD.Aa.50)
UD.Aa.50 <- st_difference(UD.Aa.50, land)
st_area(UD.Aa.50) 

load(file = "As")
# Satellite 95% 
UD.As.95 <- raster2contour(dBB.As, levels = 0.95) 
UD.As.95 <- st_as_sf(UD.As.95, coords = c("long", "lat"), crs = 3395)
UD.As.95 <- st_cast(UD.As.95, "POLYGON") 
UD.As.95 <- st_make_valid(UD.As.95)
UD.As.95 <- st_difference(UD.As.95, land)
st_area(UD.As.95) 
# Satellite 50% 
UD.As.50 <- raster2contour(dBB.As, levels = 0.50) 
UD.As.50 <- st_as_sf(UD.As.50, coords = c("long", "lat"), crs = 3395)
UD.As.50 <- st_cast(UD.As.50, "MULTIPOLYGON") 
UD.As.50 <- st_make_valid(UD.As.50)
UD.As.50 <- st_difference(UD.As.50, land)
plot(UD.As.50)
st_area(UD.As.50) 

load(file = "Ass")
# Satellite Subset 95% 
UD.Ass.95 <- raster2contour(dBB.Ass, levels = 0.95) 
UD.Ass.95 <- st_as_sf(UD.Ass.95, coords = c("long", "lat"), crs = 3395)
UD.Ass.95 <- st_cast(UD.Ass.95, "POLYGON") 
UD.Ass.95 <- st_make_valid(UD.Ass.95)
UD.Ass.95 <- st_difference(UD.Ass.95, land)
st_area(UD.Ass.95)
# Satellite Subset 50% 
UD.Ass.50 <- raster2contour(dBB.Ass, levels = 0.50) 
UD.Ass.50 <- st_as_sf(UD.Ass.50, coords = c("long", "lat"), crs = 3395)
UD.Ass.50 <- st_cast(UD.Ass.50, "POLYGON") 
UD.Ass.50 <- st_make_valid(UD.Ass.50)
UD.Ass.50 <- st_difference(UD.Ass.50, land)
plot(UD.Ass.50)
st_area(UD.Ass.50)

load(file = "Aas")
# Acoustic Subset 95% 
UD.Aas.95 <- raster2contour(dBB.Aas, levels = 0.95) 
UD.Aas.95 <- st_as_sf(UD.Aas.95, coords = c("long", "lat"), crs = 3395) 
UD.Aas.95 <- st_cast(UD.Aas.95, "POLYGON") 
UD.Aas.95 <- st_make_valid(UD.Aas.95)
UD.Aas.95 <- st_difference(UD.Aas.95, land)
st_area(UD.Aas.95) 
# Acoustic Subset 50% 
UD.Aas.50 <- raster2contour(dBB.Aas, levels = 0.50) 
UD.Aas.50 <- st_as_sf(UD.Aas.50, coords = c("long", "lat"), crs = 3395)
UD.Aas.50 <- st_cast(UD.Aas.50, "POLYGON")
UD.Aas.50 <- st_make_valid(UD.Aas.50)
UD.Aas.50 <- st_difference(UD.Aas.50, land)
st_area(UD.Aas.50) 

# Matching Temporal Duration UD plots
A_equal <- ggplot() +
  geom_sf(data = land, fill = "grey", col = "grey") +
  geom_sf(data = UD.Ass.95, aes(fill = "deepskyblue1"), col = "deepskyblue1", lwd = 0.25, alpha = 0.3, show.legend = "polygon") +
  geom_sf(data = UD.Ass.50, aes(fill = "blue2"), col = "blue2", lwd = 0.25, alpha = 0.5, show.legend = "polygon") +
  geom_sf(data = UD.Aas.95, aes(fill = "violet"), col = "violet", lwd = 0.25, alpha = 0.7, show.legend = "polygon") +
  geom_sf(data = UD.Aas.50, aes(fill = "violetred"), col = "violetred", lwd = 0.25, alpha = 0.5, show.legend = "polygon") +
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
  scale_fill_identity(name = "Occurrence Distributions", labels = c(deepskyblue1 = "Satellite 95%", blue2 = "Satellite 50%", violet = "Acoustic 95%", violetred = "Acoustic 50%"), limits = c("violet", "violetred", "deepskyblue1", "blue2"), guide = guide_legend(override.aes = list(linetype = c(rep("blank", 4)), shape = c(rep(NA, 4))))) + 
  scale_color_manual(values = c("grey28" = "grey28"), labels = c(grey28 = "Acoustic\nReceivers"), name = NULL,guide = guide_legend(override.aes = list(linetype = "blank", shape = 20, fill = NA, size = 3.5))) + 
  theme(legend.position = "right", 
        legend.text = element_text(size = 14), 
        legend.title = element_text(size = 14))
ggsave(filename = "A_equal.tiff", plot = A_equal, device = "tiff",path = "/Users/EmilyHardin/Desktop/Research/Acous.vs.Sat", width = 120, height = 120, units = c("mm"), dpi = 600)

# Full Temporal Duration UD plots
A_full <- ggplot() +
  geom_sf(data = land, fill = "grey", col = "grey") +
  geom_sf(data = UD.As.95, fill = "deepskyblue1", col = "deepskyblue1", lwd = 0.4, alpha = 0.3) +
  geom_sf(data = UD.As.50, fill = "blue2", col = "blue2", lwd = 0.4, alpha = 0.5) +
  geom_sf(data = UD.Aa.95, fill = "violet", col = "violet", lwd = 0.4, alpha = 0.7) +
  geom_sf(data = UD.Aa.50, fill = "violetred", col = "violetred", lwd = 0.4, alpha = 0.5) +
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
ggsave(filename = "A_full.tiff", plot = A_full, device = "tiff",path = "/Users/EmilyHardin/Desktop/Research/Acous.vs.Sat", width = 120, height = 120, units = c("mm"), dpi = 600)

ggplot() +
  geom_sf(data = land, fill = "grey", col = "grey") +
  geom_sf(data = UD.Ass.95, fill = "deepskyblue1", col = "deepskyblue1", lwd = 0.4, alpha = 0.3) +
  geom_sf(data = UD.Ass.50, fill = "blue2", col = "blue2", lwd = 0.4, alpha = 0.5) +
  geom_sf(data = UD.Aas.95, fill = "violet", col = "violet", lwd = 0.4, alpha = 0.7) +
  geom_sf(data = UD.Aas.50, fill = "violetred", col = "violetred", lwd = 0.4, alpha = 0.5) +
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
ggsave(filename = "A_equal.tiff", plot = A_equal, device = "tiff",path = "/Users/EmilyHardin/Desktop/Research/Acous.vs.Sat", width = 120, height = 120, units = c("mm"), dpi = 600)


## TURTLE B ## 

load(file = "Bs")
# Satellite 95% 
UD.Bs.95 <- raster2contour(dBB.Bs, levels = 0.95) 
UD.Bs.95 <- st_as_sf(UD.Bs.95, coords = c("long", "lat"), crs = 3395)
UD.Bs.95 <- st_cast(UD.Bs.95, "POLYGON") 
UD.Bs.95 <- st_make_valid(UD.Bs.95)
UD.Bs.95 <- st_difference(UD.Bs.95, land)
st_area(UD.Bs.95) 
# Satellite 50% 
UD.Bs.50 <- raster2contour(dBB.Bs, levels = 0.50) 
UD.Bs.50 <- st_as_sf(UD.Bs.50, coords = c("long", "lat"), crs = 3395)
UD.Bs.50 <- st_cast(UD.Bs.50, "POLYGON") 
UD.Bs.50 <- st_make_valid(UD.Bs.50)
UD.Bs.50 <- st_difference(UD.Bs.50, land)
st_area(UD.Bs.50) 

# Full Temporal Duration UD plot
B_full <- ggplot() +
  geom_sf(data = land, fill = "grey", col = "grey") +
  geom_sf(data = UD.Bs.95, fill = "deepskyblue1", col = "deepskyblue1", lwd = 0.4, alpha = 0.3) +
  geom_sf(data = UD.Bs.50, fill = "blue2", col = "blue2", lwd = 0.4, alpha = 0.5) +
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
  annotate("text", x = -8833201.59, y = 2955739.88, label = "Turtle B", size = 6.5, hjust = 0, vjust = 1)
ggsave(filename = "B_full.tiff", plot = B_full, device = "tiff",path = "/Users/EmilyHardin/Desktop/Research/Acous.vs.Sat", width = 120, height = 120, units = c("mm"), dpi = 600)


## TURTLE C ## 

load(file="Ca")
# Acoustic 95% 
UD.Ca.95 <- raster2contour(dBB.Ca, levels = 0.95) 
UD.Ca.95 <- st_as_sf(UD.Ca.95, coords = c("long", "lat"), crs = 3395)
UD.Ca.95 <- st_cast(UD.Ca.95, "POLYGON") 
UD.Ca.95 <- st_make_valid(UD.Ca.95)
UD.Ca.95 <- st_difference(UD.Ca.95, land)
st_area(UD.Ca.95)
# Acoustic 50% 
UD.Ca.50 <- raster2contour(dBB.Ca, levels = 0.50) 
UD.Ca.50 <- st_as_sf(UD.Ca.50, coords = c("long", "lat"), crs = 3395)
UD.Ca.50 <- st_cast(UD.Ca.50, "POLYGON") 
UD.Ca.50 <- st_make_valid(UD.Ca.50)
UD.Ca.50 <- st_difference(UD.Ca.50, land)
st_area(UD.Ca.50)

load(file = "Cs")
# Satellite 95% 
UD.Cs.95 <- raster2contour(dBB.Cs, levels = 0.95) 
UD.Cs.95 <- st_as_sf(UD.Cs.95, coords = c("long", "lat"), crs = 3395) 
UD.Cs.95 <- st_cast(UD.Cs.95, "POLYGON") 
UD.Cs.95 <- st_make_valid(UD.Cs.95)
UD.Cs.95 <- st_difference(UD.Cs.95, land)
st_area(UD.Cs.95) 
# Satellite 50% 
UD.Cs.50 <- raster2contour(dBB.Cs, levels = 0.50) 
UD.Cs.50 <- st_as_sf(UD.Cs.50, coords = c("long", "lat"), crs = 3395)
UD.Cs.50 <- st_cast(UD.Cs.50, "POLYGON") 
UD.Cs.50 <- st_make_valid(UD.Cs.50)
UD.Cs.50 <- st_difference(UD.Cs.50, land)
st_area(UD.Cs.50) #

load(file = "Css")
# Satellite Subset 95% 
UD.Css.95 <- raster2contour(dBB.Css, levels = 0.95) 
UD.Css.95 <- st_as_sf(UD.Css.95, coords = c("long", "lat"), crs = 3395) 
UD.Css.95 <- st_cast(UD.Css.95, "POLYGON") 
UD.Css.95 <- st_make_valid(UD.Css.95)
UD.Css.95 <- st_difference(UD.Css.95, land)
st_area(UD.Css.95) # 
# Satellite Subset 50% 
UD.Css.50 <- raster2contour(dBB.Css, levels = 0.50) 
UD.Css.50 <- st_as_sf(UD.Css.50, coords = c("long", "lat"), crs = 3395)
UD.Css.50 <- st_cast(UD.Css.50, "POLYGON") 
UD.Css.50 <- st_make_valid(UD.Css.50)
UD.Css.50 <- st_difference(UD.Css.50, land)
st_area(UD.Css.50) # 

load(file = "Cas")
# Acoustic Subset 95% 
UD.Cas.95 <- raster2contour(dBB.Cas, levels = 0.95) 
UD.Cas.95 <- st_as_sf(UD.Cas.95, coords = c("long", "lat"), crs = 3395) 
UD.Cas.95 <- st_cast(UD.Cas.95, "POLYGON") 
UD.Cas.95 <- st_make_valid(UD.Cas.95)
UD.Cas.95 <- st_difference(UD.Cas.95, land)
st_area(UD.Cas.95) # 
# Acoustic Subset 50% 
UD.Cas.50 <- raster2contour(dBB.Cas, levels = 0.50) 
UD.Cas.50 <- st_as_sf(UD.Cas.50, coords = c("long", "lat"), crs = 3395)
UD.Cas.50 <- st_cast(UD.Cas.50, "POLYGON") 
UD.Cas.50 <- st_make_valid(UD.Cas.50)
UD.Cas.50 <- st_difference(UD.Cas.50, land)
st_area(UD.Cas.50) # 

# Matching Temporal Duration UD plot
C_equal<- ggplot() +
  geom_sf(data = land, fill = "grey", col = "grey") +
  geom_sf(data = UD.Cas.95, fill = "violet", col = "violet", lwd = 0.4, alpha = 0.7) +
  geom_sf(data = UD.Css.95, fill = "deepskyblue1", col = "deepskyblue1", lwd = 0.4, alpha = 0.3) +
  geom_sf(data = UD.Cas.50, fill = "violetred", col = "violetred", lwd = 0.4, alpha = 0.5) +
  geom_sf(data = UD.Css.50, fill = "blue2", col = "blue2", lwd = 0.4, alpha = 0.5) +
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
ggsave(filename = "C_equal.tiff", plot = C_equal, device = "tiff",path = "/Users/EmilyHardin/Desktop/Research/Acous.vs.Sat", width = 120, height = 120, units = c("mm"), dpi = 600)

# Full Temporal Duration UD plot
C_full <- ggplot() +
  geom_sf(data = land, fill = "grey", col = "grey") +
  geom_sf(data = UD.Ca.95, fill = "violet", col = "violet", lwd = 0.4, alpha = 0.7) +
  geom_sf(data = UD.Cs.95, fill = "deepskyblue1", col = "deepskyblue1", lwd = 0.4, alpha = 0.3) +
  geom_sf(data = UD.Ca.50, fill = "violetred", col = "violetred", lwd = 0.4, alpha = 0.5) +
  geom_sf(data = UD.Cs.50, fill = "blue2", col = "blue2", lwd = 0.4, alpha = 0.5) +
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
ggsave(filename = "C_full.tiff", plot = C_full, device = "tiff",path = "/Users/EmilyHardin/Desktop/Research/Acous.vs.Sata", width = 120, height = 120, units = c("mm"), dpi = 600)


## TURTLE D ## 

load(file="Da")
# Acoustic 95% 
UD.Da.95 <- raster2contour(dBB.Da, levels = 0.95) 
UD.Da.95 <- st_as_sf(UD.Da.95, coords = c("long", "lat"), crs = 3395) 
UD.Da.95 <- st_cast(UD.Da.95, "POLYGON") 
UD.Da.95 <- st_make_valid(UD.Da.95)
UD.Da.95 <- st_difference(UD.Da.95, land)
st_area(UD.Da.95) 
# Acoustic 50% 
UD.Da.50 <- raster2contour(dBB.Da, levels = 0.50) 
UD.Da.50 <- st_as_sf(UD.Da.50, coords = c("long", "lat"), crs = 3395)
UD.Da.50 <- st_cast(UD.Da.50, "POLYGON") 
UD.Da.50 <- st_make_valid(UD.Da.50)
UD.Da.50 <- st_difference(UD.Da.50, land)
st_area(UD.Da.50) 

load(file = "Ds")
# Satellite 95% 
UD.Ds.95 <- raster2contour(dBB.Ds, levels = 0.95) 
UD.Ds.95 <- st_as_sf(UD.Ds.95, coords = c("long", "lat"), crs = 3395) 
UD.Ds.95 <- st_cast(UD.Ds.95, "POLYGON") 
UD.Ds.95 <- st_make_valid(UD.Ds.95)
UD.Ds.95 <- st_difference(UD.Ds.95, land)
st_area(UD.Ds.95) 
# Satellite 50% 
UD.Ds.50 <- raster2contour(dBB.Ds, levels = 0.50) 
UD.Ds.50 <- st_as_sf(UD.Ds.50, coords = c("long", "lat"), crs = 3395)
UD.Ds.50 <- st_cast(UD.Ds.50, "POLYGON") 
UD.Ds.50 <- st_make_valid(UD.Ds.50)
UD.Ds.50 <- st_difference(UD.Ds.50, land)
st_area(UD.Ds.50) 

load(file = "Dss")
# Satellite Subset 95% 
UD.Dss.95 <- raster2contour(dBB.Dss, levels = 0.95) 
UD.Dss.95 <- st_as_sf(UD.Dss.95, coords = c("long", "lat"), crs = 3395) 
UD.Dss.95 <- st_cast(UD.Dss.95, "POLYGON") 
UD.Dss.95 <- st_make_valid(UD.Dss.95)
UD.Dss.95 <- st_difference(UD.Dss.95, land)
st_area(UD.Dss.95) 
# Satellite Subset 50% 
UD.Dss.50 <- raster2contour(dBB.Dss, levels = 0.50) 
UD.Dss.50 <- st_as_sf(UD.Dss.50, coords = c("long", "lat"), crs = 3395)
UD.Dss.50 <- st_cast(UD.Dss.50, "POLYGON") 
UD.Dss.50 <- st_make_valid(UD.Dss.50)
UD.Dss.50 <- st_difference(UD.Dss.50, land)
st_area(UD.Dss.50) 

load(file = "Das")
# Acoustic Subset 95% 
UD.Das.95 <- raster2contour(dBB.Das, levels = 0.95) 
UD.Das.95 <- st_as_sf(UD.Das.95, coords = c("long", "lat"), crs = 3395) 
UD.Das.95 <- st_cast(UD.Das.95, "POLYGON")
UD.Das.95 <- st_make_valid(UD.Das.95)
UD.Das.95 <- st_difference(UD.Das.95, land)
st_area(UD.Das.95) 
# Acoustic Subset 50% 
UD.Das.50 <- raster2contour(dBB.Das, levels = 0.50) 
UD.Das.50 <- st_as_sf(UD.Das.50, coords = c("long", "lat"), crs = 3395)
UD.Das.50 <- st_cast(UD.Das.50, "POLYGON") 
UD.Das.50 <- st_make_valid(UD.Das.50)
UD.Das.50 <- st_difference(UD.Das.50, land)
st_area(UD.Das.50) 

# Matching Temporal Duration UD plot
D_equal <- ggplot() +
  geom_sf(data = land, fill = "grey", col = "grey") +
  geom_sf(data = UD.Dss.95, fill = "deepskyblue1", col = "deepskyblue1", lwd = 0.4, alpha = 0.3) +
  geom_sf(data = UD.Dss.50, fill = "blue2", col = "blue2", lwd = 0.4, alpha = 0.5) +
  geom_sf(data = UD.Das.95, fill = "violet", col = "violet", lwd = 0.4, alpha = 0.7) +
  geom_sf(data = UD.Das.50, fill = "violetred", col = "violetred", lwd = 0.4, alpha = 0.5) +
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
ggsave(filename = "D_equal.tiff", plot = D_equal, device = "tiff",path = "/Users/EmilyHardin/Desktop/Research/Acous.vs.Sat", width = 120, height = 120, units = c("mm"), dpi = 600)

# Full Temporal Duration UD plot
D_full <- ggplot() +
  geom_sf(data = land, fill = "grey", col = "grey") +
  geom_sf(data = UD.Ds.95, fill = "deepskyblue1", col = "deepskyblue1", lwd = 0.4, alpha = 0.3) +
  geom_sf(data = UD.Ds.50, fill = "blue2", col = "blue2", lwd = 0.4, alpha = 0.5) +
  geom_sf(data = UD.Da.95, fill = "violet", col = "violet", lwd = 0.4, alpha = 0.7) +
  geom_sf(data = UD.Da.50, fill = "violetred", col = "violetred", lwd = 0.4, alpha = 0.5) +
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
ggsave(filename = "D_full.tiff", plot = D_full, device = "tiff",path = "/Users/EmilyHardin/Desktop/Research/Acous.vs.Sat", width = 120, height = 120, units = c("mm"), dpi = 600)


## TURTLE E ##

load(file = "Es")
# Satellite 95% 
UD.Es.95 <- raster2contour(dBB.Es, levels = 0.95) 
UD.Es.95 <- st_as_sf(UD.Es.95, coords = c("long", "lat"), crs = 3395) 
UD.Es.95 <- st_cast(UD.Es.95, "POLYGON") 
UD.Es.95 <- st_make_valid(UD.Es.95)
UD.Es.95 <- st_difference(UD.Es.95, land)
st_area(UD.Es.95) 
# Satellite 50% 
UD.Es.50 <- raster2contour(dBB.Es, levels = 0.50) 
UD.Es.50 <- st_as_sf(UD.Es.50, coords = c("long", "lat"), crs = 3395)
UD.Es.50 <- st_cast(UD.Es.50, "POLYGON") 
UD.Es.50 <- st_make_valid(UD.Es.50)
UD.Es.50 <- st_difference(UD.Es.50, land)
st_area(UD.Es.50) 

load(file = "Ess")
# Satellite Subset 95% 
UD.Ess.95 <- raster2contour(dBB.Ess, levels = 0.95) 
UD.Ess.95 <- st_as_sf(UD.Ess.95, coords = c("long", "lat"), crs = 3395) 
UD.Ess.95 <- st_cast(UD.Ess.95, "POLYGON") 
UD.Ess.95 <- st_make_valid(UD.Ess.95)
UD.Ess.95 <- st_difference(UD.Ess.95, land)
st_area(UD.Ess.95) 
# Satellite Subset 50% 
UD.Ess.50 <- raster2contour(dBB.Ess, levels = 0.50) 
UD.Ess.50 <- st_as_sf(UD.Ess.50, coords = c("long", "lat"), crs = 3395)
UD.Ess.50 <- st_cast(UD.Ess.50, "POLYGON") 
UD.Ess.50 <- st_make_valid(UD.Ess.50)
UD.Ess.50 <- st_difference(UD.Ess.50, land)
st_area(UD.Ess.50) 

# Matching Temporal Duration UD plot
E_equal <- ggplot() +
  geom_sf(data = Study_Area, fill = "grey", col = "grey") +
  geom_sf(data = UD.Ess.95, fill = "deepskyblue1", col = "deepskyblue1", lwd = 0.4, alpha = 0.3, aes(fill = "")) +
  geom_sf(data = UD.Ess.50, fill = "blue2", col = "blue2", lwd = 0.4, alpha = 0.5, aes(fill = "")) +
  geom_point(data = recdata2, aes(x = lon, y = lat), col = "grey28", size = 0.8) +
  coord_sf(crs = st_crs(3395), xlim = c(-8877729.39, -8805371.72), ylim = c(2917055.21, 2976352.57)) +
  scale_x_continuous(breaks = c(-79.6, -79.4, -79.2)) +
  scale_y_continuous(breaks = c(25.6, 25.8)) +
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
  annotate("text", x = -8877729.39, y = 2976352.57, label = "Turtle E", size = 6.5, hjust = 0, vjust = 1)
ggsave(filename = "E_equal.tiff", plot = E_equal, device = "tiff",path = "/Users/EmilyHardin/Desktop/Research/Acous.vs.Sat", width = 120, height = 120, units = c("mm"), dpi = 600)

# Full Temporal Duration UD plot 
E_full <- ggplot() +
  geom_sf(data = land, fill = "grey", col = "grey") +
  geom_sf(data = UD.Es.95, fill = "deepskyblue1", col = "deepskyblue1", lwd = 0.4, alpha = 0.3) +
  geom_sf(data = UD.Es.50, fill = "blue2", col = "blue2", lwd = 0.4, alpha = 0.5) +
  geom_point(data = recdata2, aes(x = lon, y = lat), col = "grey28", size = 0.8) +
  coord_sf(crs = st_crs(3395), xlim = c(-8799805.75, -8855465.49), ylim = c(2921961.84, 2968042.41)) + 
  scale_x_continuous(breaks = c(-79.4, -79.2)) +
  scale_y_continuous(breaks = c(25.6, 25.8)) +
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
  annotate("text", x = -8855465.49, y = 2968042.41, label = "Turtle E", size = 6.5, hjust = 0, vjust = 1)
ggsave(filename = "E_full.tiff", plot = E_full, device = "tiff",path = "/Users/EmilyHardin/Desktop/Research/Acous.vs.Sat", width = 120, height = 120, units = c("mm"), dpi = 600)


## TURTLE F ## 

load(file="Fa")
# Acoustic 95% 
UD.Fa.95 <- raster2contour(dBB.Fa, levels = 0.95) 
UD.Fa.95 <- st_as_sf(UD.Fa.95, coords = c("long", "lat"), crs = 3395) 
UD.Fa.95 <- st_cast(UD.Fa.95, "POLYGON") 
UD.Fa.95 <- st_make_valid(UD.Fa.95)
UD.Fa.95 <- st_difference(UD.Fa.95, land)
st_area(UD.Fa.95) 
# Acoustic 50% 
UD.Fa.50 <- raster2contour(dBB.Fa, levels = 0.50) 
UD.Fa.50 <- st_as_sf(UD.Fa.50, coords = c("long", "lat"), crs = 3395)
UD.Fa.50 <- st_cast(UD.Fa.50, "POLYGON") 
UD.Fa.50 <- st_make_valid(UD.Fa.50)
UD.Fa.50 <- st_difference(UD.Fa.50, land)
st_area(UD.Fa.50) 

# Full Temporal Duration UD plot
F_full <- ggplot() +
  geom_sf(data = land, fill = "grey", col = "grey") +
  geom_sf(data = UD.Fa.95, fill = "violet", col = "violet", lwd = 0.4, alpha = 0.7) +
  geom_sf(data = UD.Fa.50, fill = "violetred", col = "violetred", lwd = 0.4, alpha = 0.5) +
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
  annotate("text", x = -8833201.59, y = 2955739.88, label = "Turtle F", size = 6.5, hjust = 0, vjust = 1)
ggsave(filename = "F_full.tiff", plot = F_full, device = "tiff",path = "/Users/EmilyHardin/Desktop/Research/Acous.vs.Sat", width = 120, height = 120, units = c("mm"), dpi = 600)


## TURTLE G ##

load(file="Ga")
# Acoustic 95% 
UD.Ga.95 <- raster2contour(dBB.Ga, levels = 0.95) 
UD.Ga.95 <- st_as_sf(UD.Ga.95, coords = c("long", "lat"), crs = 3395) 
UD.Ga.95 <- st_cast(UD.Ga.95, "POLYGON") 
UD.Ga.95 <- st_make_valid(UD.Ga.95)
UD.Ga.95 <- st_difference(UD.Ga.95, land)
st_area(UD.Ga.95) 
# Acoustic 50% 
UD.Ga.50 <- raster2contour(dBB.Ga, levels = 0.50) 
UD.Ga.50 <- st_as_sf(UD.Ga.50, coords = c("long", "lat"), crs = 3395)
UD.Ga.50 <- st_cast(UD.Ga.50, "POLYGON") 
UD.Ga.50 <- st_make_valid(UD.Ga.50)
UD.Ga.50 <- st_difference(UD.Ga.50, land)
st_area(UD.Ga.50) 

load(file = "Gs")
# Satellite 95% 
UD.Gs.95 <- raster2contour(dBB.Gs, levels = 0.95) 
UD.Gs.95 <- st_as_sf(UD.Gs.95, coords = c("long", "lat"), crs = 3395) 
UD.Gs.95 <- st_cast(UD.Gs.95, "POLYGON") 
UD.Gs.95 <- st_make_valid(UD.Gs.95)
UD.Gs.95 <- st_difference(UD.Gs.95, land)
st_area(UD.Gs.95) 
# Satellite 50% 
UD.Gs.50 <- raster2contour(dBB.Gs, levels = 0.50) 
UD.Gs.50 <- st_as_sf(UD.Gs.50, coords = c("long", "lat"), crs = 3395)
UD.Gs.50 <- st_cast(UD.Gs.50, "POLYGON")  
UD.Gs.50 <- st_make_valid(UD.Gs.50)
UD.Gs.50 <- st_difference(UD.Gs.50, land)
st_area(UD.Gs.50) 

load(file = "Gss")
# Satellite Subset 95% 
UD.Gss.95 <- raster2contour(dBB.Gss, levels = 0.95) 
UD.Gss.95 <- st_as_sf(UD.Gss.95, coords = c("long", "lat"), crs = 3395) 
UD.Gss.95 <- st_cast(UD.Gss.95, "POLYGON")
UD.Gss.95 <- st_make_valid(UD.Gss.95)
UD.Gss.95 <- st_difference(UD.Gss.95, land)
st_area(UD.Gss.95) 
# Satellite Subset 50% 
UD.Gss.50 <- raster2contour(dBB.Gss, levels = 0.50) 
UD.Gss.50 <- st_as_sf(UD.Gss.50, coords = c("long", "lat"), crs = 3395)
UD.Gss.50 <- st_cast(UD.Gss.50, "POLYGON") 
UD.Gss.50 <- st_make_valid(UD.Gss.50)
UD.Gss.50 <- st_difference(UD.Gss.50, land)
st_area(UD.Gss.50) 

# Matching Temporal Duration UD plot
G_equal <- ggplot() +
  geom_sf(data = land, fill = "grey", col = "grey") +
  geom_sf(data = UD.Gss.95, fill = "deepskyblue1", col = "deepskyblue1", lwd = 0.4, alpha = 0.3) +
  geom_sf(data = UD.Gss.50, fill = "blue2", col = "blue2", lwd = 0.4, alpha = 0.5) +
  geom_sf(data = UD.Ga.95, fill = "violet", col = "violet", lwd = 0.4, alpha = 0.7) +
  geom_sf(data = UD.Ga.50, fill = "violetred", col = "violetred", lwd = 0.4, alpha = 0.5) +
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
ggsave(filename = "G_equal.tiff", plot = G_equal, device = "tiff",path = "/Users/EmilyHardin/Desktop/Research/Acous.vs.Sat", width = 120, height = 120, units = c("mm"), dpi = 600)

# Full Temporal Duration UD plot
G_full <- ggplot() +
  geom_sf(data = land, fill = "grey", col = "grey") +
  geom_sf(data = UD.Gs.95, fill = "deepskyblue1", col = "deepskyblue1", lwd = 0.4, alpha = 0.3) +
  geom_sf(data = UD.Gs.50, fill = "blue2", col = "blue2", lwd = 0.4, alpha = 0.5) +
  geom_sf(data = UD.Ga.95, fill = "violet", col = "violet", lwd = 0.4, alpha = 0.7) +
  geom_sf(data = UD.Ga.50, fill = "violetred", col = "violetred", lwd = 0.4, alpha = 0.5) +
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
ggsave(filename = "G_full.tiff", plot = G_full, device = "tiff",path = "/Users/EmilyHardin/Desktop/Research/Acous.vs.Sat", width = 120, height = 120, units = c("mm"), dpi = 600)


## TURTLE H ## 

load(file = "Hs")
# Satellite 95% 
UD.Hs.95 <- raster2contour(dBB.Hs, levels = 0.95) 
UD.Hs.95 <- st_as_sf(UD.Hs.95, coords = c("long", "lat"), crs = 3395) 
UD.Hs.95 <- st_cast(UD.Hs.95, "POLYGON") 
UD.Hs.95 <- st_make_valid(UD.Hs.95)
UD.Hs.95 <- st_difference(UD.Hs.95, land)
st_area(UD.Hs.95) 
# Satellite 50% 
UD.Hs.50 <- raster2contour(dBB.Hs, levels = 0.50) 
UD.Hs.50 <- st_as_sf(UD.Hs.50, coords = c("long", "lat"), crs = 3395)
UD.Hs.50 <- st_cast(UD.Hs.50, "POLYGON") 
UD.Hs.50 <- st_make_valid(UD.Hs.50)
UD.Hs.50 <- st_difference(UD.Hs.50, land)
st_area(UD.Hs.50) 

load(file = "Hss")
# Satellite Subset 95% 
UD.Hss.95 <- raster2contour(dBB.Hss, levels = 0.95) 
UD.Hss.95 <- st_as_sf(UD.Hss.95, coords = c("long", "lat"), crs = 3395) 
UD.Hss.95 <- st_cast(UD.Hss.95, "POLYGON") 
UD.Hss.95 <- st_make_valid(UD.Hss.95)
UD.Hss.95 <- st_difference(UD.Hss.95, land)
st_area(UD.Hss.95) 
# Satellite Subset 50% 
UD.Hss.50 <- raster2contour(dBB.Hss, levels = 0.50) 
UD.Hss.50 <- st_as_sf(UD.Hss.50, coords = c("long", "lat"), crs = 3395)
UD.Hss.50 <- st_cast(UD.Hss.50, "POLYGON") 
UD.Hss.50 <- st_make_valid(UD.Hss.50)
UD.Hss.50 <- st_difference(UD.Hss.50, land)
st_area(UD.Hss.50) # 

# Matching Temporal Duration UD plot
H_equal <- ggplot() +
  geom_sf(data = land, fill = "grey", col = "grey") +
  geom_sf(data = UD.Hss.95, fill = "deepskyblue1", col = "deepskyblue1", lwd = 0.4, alpha = 0.3) +
  geom_sf(data = UD.Hss.50, fill = "blue2", col = "blue2", lwd = 0.4, alpha = 0.5) +
  geom_point(data = recdata2, aes(x = lon, y = lat), col = "grey28", size = 0.8) +
  coord_sf(crs = st_crs(3395), xlim = c(-8810937.70, -8833201.59), ylim = c(2937305.74, 2955739.88)) + 
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
  annotate("text", x = -8833201.59, y = 2955739.88, label = "Turtle H", size = 6.5, hjust = 0, vjust = 1)
ggsave(filename = "H_equal2.tiff", plot = H_equal2, device = "tiff",path = "/Users/EmilyHardin/Desktop/Research/Acous.vs.Sat", width = 120, height = 120, units = c("mm"), dpi = 600)

# Full Temporal Duration UD plot
H_full <- ggplot() +
  geom_sf(data = land, fill = "grey", col = "grey") +
  geom_sf(data = UD.Hs.95, fill = "deepskyblue1", col = "deepskyblue1", lwd = 0.4, alpha = 0.3) +
  geom_sf(data = UD.Hs.50, fill = "blue2", col = "blue2", lwd = 0.4, alpha = 0.5) +
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
  annotate("text", x = -8833201.59, y = 2955739.88, label = "Turtle H", size = 6.5, hjust = 0, vjust = 1)
ggsave(filename = "H_full2.tiff", plot = H_full2, device = "tiff",path = "/Users/EmilyHardin/Desktop/Research/Acous.vs.Sat", width = 120, height = 120, units = c("mm"), dpi = 600)


## TURTLE I ## 

load(file="Ia")
# Acoustic 95% 
UD.Ia.95 <- raster2contour(dBB.Ia, levels = 0.95) 
UD.Ia.95 <- st_as_sf(UD.Ia.95, coords = c("long", "lat"), crs = 3395) 
UD.Ia.95 <- st_cast(UD.Ia.95, "POLYGON") 
UD.Ia.95 <- st_make_valid(UD.Ia.95)
UD.Ia.95 <- st_difference(UD.Ia.95, land)
st_area(UD.Ia.95) 
# Acoustic 50% 
UD.Ia.50 <- raster2contour(dBB.Ia, levels = 0.50) 
UD.Ia.50 <- st_as_sf(UD.Ia.50, coords = c("long", "lat"), crs = 3395)
UD.Ia.50 <- st_cast(UD.Ia.50, "POLYGON") 
UD.Ia.50 <- st_make_valid(UD.Ia.50)
UD.Ia.50 <- st_difference(UD.Ia.50, land)
st_area(UD.Ia.50) 

load(file = "Is")
# Satellite 95% 
UD.Is.95 <- raster2contour(dBB.Is, levels = 0.95) 
UD.Is.95 <- st_as_sf(UD.Is.95, coords = c("long", "lat"), crs = 3395) 
UD.Is.95 <- st_cast(UD.Is.95, "POLYGON") 
UD.Is.95 <- st_make_valid(UD.Is.95)
UD.Is.95 <- st_difference(UD.Is.95, land)
st_area(UD.Is.95) 
# Satellite 50% 
UD.Is.50 <- raster2contour(dBB.Is, levels = 0.50) 
UD.Is.50 <- st_as_sf(UD.Is.50, coords = c("long", "lat"), crs = 3395)
UD.Is.50 <- st_cast(UD.Is.50, "POLYGON") 
UD.Is.50 <- st_make_valid(UD.Is.50)
UD.Is.50 <- st_difference(UD.Is.50, land)
st_area(UD.Is.50) 

load(file = "Iss")
# Satellite Subset 95% 
UD.Iss.95 <- raster2contour(dBB.Iss, levels = 0.95) 
UD.Iss.95 <- st_as_sf(UD.Iss.95, coords = c("long", "lat"), crs = 3395) 
UD.Iss.95 <- st_cast(UD.Iss.95, "POLYGON") 
UD.Iss.95 <- st_make_valid(UD.Iss.95)
UD.Iss.95 <- st_difference(UD.Iss.95, land)
st_area(UD.Iss.95) 
# Satellite Subset 50% 
UD.Iss.50 <- raster2contour(dBB.Iss, levels = 0.50) 
UD.Iss.50 <- st_as_sf(UD.Iss.50, coords = c("long", "lat"), crs = 3395)
UD.Iss.50 <- st_cast(UD.Iss.50, "POLYGON") 
UD.Iss.50 <- st_make_valid(UD.Iss.50)
UD.Iss.50 <- st_difference(UD.Iss.50, land)
st_area(UD.Iss.50) 

# Matching Temporal Duration UD plot
I_equal <- ggplot() +
  geom_sf(data = land, fill = "grey", col = "grey") +
  geom_sf(data = UD.Iss.95, fill = "deepskyblue1", col = "deepskyblue1", lwd = 0.4, alpha = 0.3, aes(fill = "")) +
  geom_sf(data = UD.Iss.50, fill = "blue2", col = "blue2", lwd = 0.4, alpha = 0.5, aes(fill = "")) +
  geom_sf(data = UD.Ia.95, fill = "violet", col = "violet", lwd = 0.4, alpha = 0.7, aes(fill = "")) +
  geom_sf(data = UD.Ia.50, fill = "violetred", col = "violetred", lwd = 0.4, alpha = 0.5, aes(fill = "")) +
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
ggsave(filename = "I_equal.tiff", plot = I_equal, device = "tiff",path = "/Users/EmilyHardin/Desktop/Research/Acous.vs.Sat", width = 120, height = 120, units = c("mm"), dpi = 600)

# Full Temporal Duration UD plot 
I_full <- ggplot() +
  geom_sf(data = land, fill = "grey", col = "grey") +
  geom_sf(data = UD.Is.95, fill = "deepskyblue1", col = "deepskyblue1", lwd = 0.4, alpha = 0.3) +
  geom_sf(data = UD.Is.50, fill = "blue2", col = "blue2", lwd = 0.4, alpha = 0.5) +
  geom_sf(data = UD.Ia.95, fill = "violet", col = "violet", lwd = 0.4, alpha = 0.7) +
  geom_sf(data = UD.Ia.50, fill = "violetred", col = "violetred", lwd = 0.4, alpha = 0.5) +
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

ggsave(filename = "I_full.tiff", plot = I_full, device = "tiff",path = "/Users/EmilyHardin/Desktop/Research/Acous.vs.Sat/From Mariana", width = 120, height = 120, units = c("mm"), dpi = 600)


# Next Script: OD Comparisons
