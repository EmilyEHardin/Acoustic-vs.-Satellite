#### FIGURES ####


## STUDY AREA/BENTHIC MAP ##

# benthic data retrieved from Allen Coral Atlas https://allencoralatlas.org/atlas/#1.00/0.0000/-145.0000

library(geojsonsf)
library(sf)
library(ggplot2)
library(ggspatial)

bimini2 <- geojsonsf::geojson_sf("~/Downloads/Bimini2/Benthic-Map/benthic.geojson")
bimini2 <- st_transform(bimini2, 3395)
# extract each benthic layer
rubble <- bimini2[bimini2$class == "Rubble", ]
coral <- bimini2[bimini2$class == "Coral/Algae", ]
rock <- bimini2[bimini2$class == "Rock", ]
seagrass <- bimini2[bimini2$class == "Seagrass", ]
sand <- bimini2[bimini2$class == "Sand", ]

# study area
Study_Area <- st_read(dsn = "bimini shapefile/bimini_shape.shp", quiet = TRUE)
land <- Study_Area[Study_Area$AltMode == "0", ] #extract just one layer from the collection 
land <- st_union(land) #make it a polygon layer
land <- st_transform(land, 3395)

# receiver information 
recdata2 <- read.csv("rec_final.csv", header = TRUE)
recdata2 <- st_as_sf(recdata2, coords = c("lon", "lat"), crs = 4326)
recdata2 <- st_transform(recdata2, 3395)
recdata2$lon<-st_coordinates(recdata2)[,1]
recdata2$lat<-st_coordinates(recdata2)[,2]

# map 
habitatmap <- ggplot() + 
  geom_sf(data = sand, col = "wheat1", lwd = 0.3, aes(fill = "wheat1"), show.legend = "polygon") +
  geom_sf(data = rubble, col = "slategray1", lwd = 0.3, aes(fill = "slategray1"), show.legend = "polygon") +
  geom_sf(data = rock, col = "lemonchiffon4", lwd = 0.3, aes(fill = "lemonchiffon4"), show.legend = "polygon") +
  geom_sf(data = coral, col = "coral1", lwd = 0.3, aes(fill = "coral1"), show.legend = "polygon") +
  geom_sf(data = seagrass, col = "yellowgreen", lwd = 0.3, aes(fill = "yellowgreen"), show.legend = "polygon") +
  geom_sf(data = land, col = "black", lwd = 0.3, aes(fill = "grey", col = "black"), show.legend = "polygon") +
  geom_point(data = recdata2, aes(x = lon, y = lat, col = "black"), col = "black", size = 1, show.legend = T) +
  coord_sf(xlim = c(-8833201.59, -8799805.74), ylim = c(2918895.00, 2986515.99)) +
  scale_x_continuous(breaks = c(-79.3, -79.1)) +
  scale_y_continuous(breaks = c(25.6, 25.8, 26.0)) +
  annotation_scale(style = "ticks", location = "br", text_cex = 0.85, line_width = 1, height = unit(0.35, "cm"), text_pad = unit(0.15, "cm"), pad_x = unit(0.35, "cm"), pad_y = unit(0.35, "cm")) + 
  scale_fill_identity(name = "Benthic Habitat", labels = c(coral1 = "Coral/Algae", grey = "Land", lemonchiffon4 = "Rock", slategray1 = "Rubble", wheat1 = "Sand", yellowgreen = "Seagrass"), limits = c("coral1", "grey", "lemonchiffon4", "slategray1", "wheat1", "yellowgreen"), guide = guide_legend(override.aes = list(linetype = c(rep("blank", 6)), shape = c(rep(NA, 6))))) + 
  scale_color_manual(values = c("black" = "black"), labels = c(black = "Acoustic\nReceivers"), name = NULL, guide = guide_legend(override.aes = list(linetype = "blank", shape = 20, fill = NA, size = 3.5))) + 
  theme_bw() +
  theme(legend.position = "right") +
  theme(text = element_text(size = 15)) +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5)) + 
  theme(axis.ticks.y = element_line(size = 0.5)) + 
  theme(axis.ticks.x = element_line(size = 0.5)) +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

ggsave(filename = "habitatmap.tiff", plot = habitatmap, device = "tiff", path = "/Users/EmilyHardin/Desktop/Research/Acous.vs.Sat/From Mariana", width = 240, height = 240, units = c("mm"), dpi = 400)


## SSM SATELLITE TRACKS AND COAs MAPS ## 

# individual maps combined with outside software
# this code uses the post-SSM satellite data {move} objects and COA locations {move} objects from "Individual dBBMMs" script. If these objects are not already loaded, please visit that script, or load in data from associated csv files. 

library(sf)
library(ggplot2)
library(ggspatial)
library(dplyr)
library(move)

# satellite track {move} objects needs to be converted to dataframes 
track.As <- as.data.frame(As)
track.Bs <- as.data.frame(Bs)
track.Cs <- as.data.frame(Cs)
track.Ds <- as.data.frame(Ds)
track.Es <- as.data.frame(Es)
track.Gs <- as.data.frame(Gs)
track.Hs <- as.data.frame(Hs)
track.Is <- as.data.frame(Is)

# acoustic COA info needs to be converted to 
Ea <- acoustic[["X64480"]] # Turtle E needs to be subset from overall {move} object, since it didn't have enough data for the dBBMMs 
Ha <- acoustic[["X64483"]] # Turtle H needs to be subset from overall {move} object, since it didn't have enough data for the dBBMMs  
det.Aa <- as.data.frame(Aa)
det.Ca <- as.data.frame(Ca)
det.Da <- as.data.frame(Da)
det.Ea <- as.data.frame(Ea)
det.Fa <- as.data.frame(Fa)
det.Ga <- as.data.frame(Ga)
det.Ha <- as.data.frame(Ha)
det.Ia <- as.data.frame(Ia)

# map code for all turtles except E and I (which have different coordinate system extents) + legend for COAs
# change out IDs
A_track <- ggplot() +
  geom_sf(data = land, fill = "grey", col = "grey") +
  geom_point(data = recdata2, aes(x = lon, y = lat), col = "grey60", size = 0.8) +
  geom_path(data = track.As, aes(x = x, y = y, col = "grey28"), col = "grey28") +
  geom_point(data = det.Aa, aes(x = coords.x1, y = coords.x2), col = "red", show.legend = T) + 
  coord_sf(crs = st_crs(3395), xlim = c(-8810937.70, -8838767.56), ylim = c(2937305.74, 2958814.52)) + 
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
  annotate("text", x = -8838767.56, y = 2958814.52, label = "Turtle A", size = 6.5, hjust = 0, vjust = 1) + 
  scale_color_manual(values = c("red" = "red"), labels = c(red = "COAs"), name = NULL, guide = guide_legend(override.aes = list(linetype = c("blank"), shape = c(20), fill = c("red"), size = c(3.5))))
ggsave(filename = "A_track.tiff", plot = A_track, device = "tiff",path = "/Users/EmilyHardin/Desktop/Research/Acous.vs.Sat", width = 120, height = 120, units = c("mm"), dpi = 600)

# map code for Turtle E + satellite track legend 
E_track <- ggplot() +
  geom_sf(data = land, fill = "grey", col = "grey") +
  geom_point(data = recdata2, aes(x = lon, y = lat), col = "grey60", size = 0.8) +
  geom_path(data = track.Es, aes(x = x, y = y, col = "grey28"), col = "grey28", show.legend = T) +
  geom_point(data = det.Ea, aes(x = coords.x1, y = coords.x2), col = "red") + 
  coord_sf(crs = st_crs(3395), xlim = c(-8794239.77, -8877729.39), ylim = c(2913231.07, 2977796.37)) + 
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
  annotate("text", x = -8877729.39, y = 2977796.37, label = "Turtle E", size = 6.5, hjust = 0, vjust = 1) + 
  scale_color_manual(values = c("grey28" = "grey28"), labels = c(grey28 = "Satellite Track"), name = NULL,guide = guide_legend(override.aes = list(linetype = c(1))))


ggsave(filename = "E_track.tiff", plot = E_track, device = "tiff",path = "/Users/EmilyHardin/Desktop/Research/Acous.vs.Sat", width = 120, height = 120, units = c("mm"), dpi = 600)

# map code for Turtle I + legend for grey receiver points 
I_track <- ggplot() +
  geom_sf(data = land, fill = "grey", col = "grey") +
  geom_point(data = recdata2, aes(x = lon, y = lat, col = "grey60"), col = "grey60", size = 0.8, show.legend = T) +
  geom_path(data = track.Is, aes(x = x, y = y, col = "grey28"), col = "grey28") +
  geom_point(data = det.Ia, aes(x = coords.x1, y = coords.x2), col = "red", show.legend = T) + 
  coord_sf(crs = st_crs(3395), xlim = c(-8849899.51, -8794239.77), ylim = c(2912763.26, 2955739.88)) + 
  scale_x_continuous(breaks = c(-79.4, -79.20)) +
  scale_y_continuous(breaks = c(25.55, 25.75)) +
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
  annotate("text", x = -8849899.51, y = 2955739.88, label = "Turtle I", size = 6.5, hjust = 0, vjust = 1) + 
  theme(legend.position = "bottom", 
        legend.text = element_text(size = 14), 
        legend.title = element_text(size = 14)) + 
  scale_color_manual(values = c("grey60" = "grey60"), labels = c(grey60 = "Acoustic Receivers"), name = NULL, guide = guide_legend(override.aes = list(linetype = c("blank"), shape = c(20), fill = c("grey60"), size = c(3.5))))

ggsave(filename = "I_track.tiff", plot = I_track, device = "tiff",path = "/Users/EmilyHardin/Desktop/Research/Acous.vs.Sat/From Mariana", width = 120, height = 120, units = c("mm"), dpi = 600)


