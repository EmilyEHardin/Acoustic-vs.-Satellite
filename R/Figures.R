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


## CTMM, SSM, and COAs MAPS ## 

# individual maps combined with outside software
library(sf)
library(ggplot2)
library(ggspatial)
library(dplyr)
library(move)

# CTMM satellite transmissions = sat_ctmm
sat_ctmm <- read.csv("sat_ctmm.csv", header = T)
# SSM satellite transmissions = fG_noland
sat_ssm <- read.csv("fG_noland.csv", header = T)
# COAs = COA_all
coas <- read.csv("COAall_loc.err.csv", header = T)
# map code for all turtles except E and I (which have different coordinate system extents) + legend for COAs

# add geometry 
sat_ctmm <- st_as_sf(sat_ctmm, coords = c("x", "y"), crs = 3395)
sat_ctmm$lon<-st_coordinates(sat_ctmm)[,1]
sat_ctmm$lat<-st_coordinates(sat_ctmm)[,2]
sat_ctmm <- sat_ctmm %>%
  rename(id = tag.local.identifier)

sat_ssm <- st_as_sf(sat_ssm, coords = c("x", "y"), crs = 3395)
sat_ssm$lon<-st_coordinates(sat_ssm)[,1]
sat_ssm$lat<-st_coordinates(sat_ssm)[,2]
sat_ssm <- sat_ssm %>%
  filter(Location.Err < 3000)

coas <- st_as_sf(coas, coords = c("Longitude.coa", "Latitude.coa"), crs = 4326)
coas <- st_transform(coas, 3395)
coas$lon<-st_coordinates(coas)[,1]
coas$lat<-st_coordinates(coas)[,2]

# Turtle C
C_all <- ggplot() +
  geom_sf(data = land, fill = "grey", col = "grey") +
  geom_point(data = recdata2, aes(x = lon, y = lat), col = "grey60", size = 0.8) +
  geom_path(data = (sat_ctmm %>% filter(id == "X169269")), aes(x = lon, y = lat), col = "grey28") +
  geom_point(data = (coas %>% filter(Tag.ID == "64477")), aes(x = lon, y = lat), col = "red2", size = 0.8, show.legend = T) +
  geom_path(data = (sat_ssm %>% filter(id == "169269")), aes(x = lon, y = lat), col = "dodgerblue3") +
  coord_sf(crs = st_crs(3395), xlim = c(-8837467, -8791537), ylim = c(2922185, 2967935)) + 
  scale_x_continuous(breaks = c(-79.30, -79.10)) +
  scale_y_continuous(breaks = c(25.55, 25.70, 25.85)) +
  annotation_scale(style = "ticks", location = "br", text_cex = 0.80, line_width = 1, height = unit(0.30, "cm"), text_pad = unit(0.15, "cm"), pad_x = unit(0.35, "cm"), pad_y = unit(0.35, "cm")) + 
  theme_bw() +
  theme(text = element_text(size = 12)) +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5)) + 
  theme(axis.ticks.y = element_line(size = 0.5)) + 
  theme(axis.ticks.x = element_line(size = 0.5)) +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  ggplot2::annotate("text", x = -8837467, y =  2967935, label = "Turtle C", size = 6.5, hjust = 0, vjust = 1) + 
  scale_color_manual(values = c("red" = "red"), labels = c(red = "COAs"), name = NULL, guide = guide_legend(override.aes = list(linetype = c("blank"), shape = c(20), fill = c("red"), size = c(3.5))))

ggsave(filename = "C_all.tiff", plot = C_all, device = "tiff",path = "/Users/EmilyHardin/Desktop/Research/Acous.vs.Sat", width = 120, height = 120, units = c("mm"), dpi = 600)


# Turtle A, B, D, F, and G
G_all <- ggplot() +
  geom_sf(data = land, fill = "grey", col = "grey") +
  geom_point(data = recdata2, aes(x = lon, y = lat), col = "grey60", size = 0.8) +
  geom_path(data = (sat_ctmm %>% filter(id == "X169268")), aes(x = lon, y = lat), col = "grey28") +
  geom_path(data = (sat_ssm %>% filter(id == "169268")), aes(x = lon, y = lat), col = "dodgerblue") +
  geom_point(data = (coas %>% filter(Tag.ID == "64482")), aes(x = lon, y = lat, col = "red2"), col = "red2", size = 0.8, show.legend = T) +
  coord_sf(crs = st_crs(3395), xlim = c(-8837467, -8805037), ylim = c(2929945, 2962375)) + 
  scale_x_continuous(breaks = c(-79.35, -79.25, -79.15)) +
  scale_y_continuous(breaks = c(25.65, 25.75, 25.85)) +
  annotation_scale(style = "ticks", location = "br", text_cex = 0.80, line_width = 1, height = unit(0.30, "cm"), text_pad = unit(0.15, "cm"), pad_x = unit(0.35, "cm"), pad_y = unit(0.35, "cm")) + 
  theme_bw() +
  theme(text = element_text(size = 12)) +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5)) + 
  theme(axis.ticks.y = element_line(size = 0.5)) + 
  theme(axis.ticks.x = element_line(size = 0.5)) +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  ggplot2::annotate("text", x = -8837467, y =  2962375, label = "Turtle G", size = 6.5, hjust = 0, vjust = 1) + 
  scale_color_manual(values = c("red2" = "red2"), labels = c(red = "COAs"), name = NULL, guide = guide_legend(override.aes = list(linetype = c("blank"), shape = c(20), fill = c("red2"), size = c(3.5))))

ggsave(filename = "G_all.tiff", plot = G_all, device = "tiff",path = "/Users/EmilyHardin/Desktop/Research/Acous.vs.Sat", width = 120, height = 120, units = c("mm"), dpi = 600)


# Turtle H and I 
H_all <- ggplot() +
  geom_sf(data = land, fill = "grey", col = "grey") +
  geom_point(data = recdata2, aes(x = lon, y = lat), col = "grey60", size = 0.8) +
  geom_path(data = (sat_ctmm %>% filter(id == "X169274")), aes(x = lon, y = lat), col = "grey28") +
  geom_path(data = (sat_ssm %>% filter(id == "169274")), aes(x = lon, y = lat), col = "dodgerblue") +
  geom_point(data = (coas %>% filter(Tag.ID == "64483")), aes(x = lon, y = lat), col = "red2", size = 0.8, show.legend = T) +
  coord_sf(crs = st_crs(3395), xlim = c(-8836467, -8770037), ylim = c(2905870, 2972300)) + 
  scale_x_continuous(breaks = c(-79.30, -79.10, -78.90)) +
  scale_y_continuous(breaks = c(25.45, 25.65, 25.85)) +
  annotation_scale(style = "ticks", location = "br", text_cex = 0.80, line_width = 1, height = unit(0.30, "cm"), text_pad = unit(0.15, "cm"), pad_x = unit(0.35, "cm"), pad_y = unit(0.35, "cm")) + 
  theme_bw() +
  theme(text = element_text(size = 12)) +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5)) + 
  theme(axis.ticks.y = element_line(size = 0.5)) + 
  theme(axis.ticks.x = element_line(size = 0.5)) +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  ggplot2::annotate("text", x = -8836467, y =  2972300, label = "Turtle H", size = 6.5, hjust = 0, vjust = 1) + 
  scale_color_manual(values = c("grey60", "dodgerblue", "red2)", labels = c("CTMM", "SSM", "COAs"), name = NULL))


ggsave(filename = "H_all.tiff", plot = H_all, device = "tiff",path = "/Users/EmilyHardin/Desktop/Research/Acous.vs.Sat", width = 120, height = 120, units = c("mm"), dpi = 600)


# Turtle E 
E_all <- ggplot() +
  geom_sf(data = land, fill = "grey", col = "grey") +
  geom_point(data = recdata2, aes(x = lon, y = lat), col = "grey60", size = 0.8) +
  geom_path(data = (sat_ctmm %>% filter(id == "X169273")), aes(x = lon, y = lat), col = "grey28") +
  geom_path(data = (sat_ssm %>% filter(id == "169273")), aes(x = lon, y = lat), col = "dodgerblue") +
  geom_point(data = (coas %>% filter(Tag.ID == "64480")), aes(x = lon, y = lat), col = "red2", size = 0.8, show.legend = T) +
  coord_sf(crs = st_crs(3395), xlim = c(-8892870, -8763266), ylim = c(2890395, 3020000)) + 
  scale_x_continuous(breaks = c(-79.0, -79.4, -79.8)) +
  scale_y_continuous(breaks = c(25.4, 25.7, 26.0, 26.3)) +
  annotation_scale(style = "ticks", location = "br", text_cex = 0.80, line_width = 1, height = unit(0.30, "cm"), text_pad = unit(0.15, "cm"), pad_x = unit(0.35, "cm"), pad_y = unit(0.35, "cm")) + 
  theme_bw() +
  theme(text = element_text(size = 12)) +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5)) + 
  theme(axis.ticks.y = element_line(size = 0.5)) + 
  theme(axis.ticks.x = element_line(size = 0.5)) +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  ggplot2::annotate("text", x = -8892870, y =  3020000, label = "Turtle E", size = 6.5, hjust = 0, vjust = 1) + 
  scale_color_manual(values = c("grey60", "dodgerblue", "red2)", labels = c("CTMM", "SSM", "COAs"), name = NULL))

ggsave(filename = "E_all.tiff", plot = E_all, device = "tiff",path = "/Users/EmilyHardin/Desktop/Research/Acous.vs.Sat", width = 120, height = 120, units = c("mm"), dpi = 600)

# rec legend
ggplot() +
  geom_sf(data = land, fill = "grey", col = "grey") +
  geom_point(data = recdata2, aes(x = lon, y = lat, col = "grey60"), size = 0.8, show.legend = T) +
  theme_bw() +
  theme(text = element_text(size = 15)) +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5)) + 
  theme(axis.ticks.y = element_line(size = 0.5)) + 
  theme(axis.ticks.x = element_line(size = 0.5)) +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  theme(legend.position = "bottom", 
        legend.text = element_text(size = 14), 
        legend.title = element_text(size = 14)) + 
  scale_color_manual(values = "grey60", labels = c(grey60 = "Acoustic Receivers"), name = NULL, guide = guide_legend(override.aes = list(linetype = c("blank"), shape = c(20), fill = c("grey60"), size = c(3.5))))

ggsave(filename = "I_track.tiff", plot = I_track, device = "tiff",path = "/Users/EmilyHardin/Desktop/Research/Acous.vs.Sat", width = 120, height = 120, units = c("mm"), dpi = 600)


# CTMM legend
ctmm_legend <- ggplot() +
  geom_sf(data = land, fill = "grey", col = "grey") +
  geom_path(data = test, aes(x = lon, y = lat, col = "dodgerblue"), size = 0.8, show.legend = T) +
  theme_bw() +
  theme(text = element_text(size = 15)) +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5)) + 
  theme(axis.ticks.y = element_line(size = 0.5)) + 
  theme(axis.ticks.x = element_line(size = 0.5)) +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  theme(legend.position = "bottom", 
        legend.text = element_text(size = 14), 
        legend.title = element_text(size = 14)) + 
  scale_color_manual(values = c("dodgerblue" = "dodgerblue"), labels = c(dodgerblue = "CTMM track"), name = NULL, guide = guide_legend(override.aes = list(linetype = c(1), size = c(10))))

ggsave(filename = "ctmm_legend.tiff", plot = ctmm_legend, device = "tiff",path = "/Users/EmilyHardin/Desktop/Research/Acous.vs.Sat", width = 120, height = 120, units = c("mm"), dpi = 600)

# SSM legend
ssm <- ggplot() +
  geom_sf(data = land, fill = "grey", col = "grey") +
  geom_path(data = test, aes(x = lon, y = lat, col = "grey28"), size = 0.8, show.legend = T) +
  theme_bw() +
  theme(text = element_text(size = 15)) +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5)) + 
  theme(axis.ticks.y = element_line(size = 0.5)) + 
  theme(axis.ticks.x = element_line(size = 0.5)) +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  theme(legend.position = "bottom", 
        legend.text = element_text(size = 14), 
        legend.title = element_text(size = 14)) + 
  scale_color_manual(values = c("grey28" = "grey28"), labels = c(grey28 = "SSM track"), name = NULL, guide = guide_legend(override.aes = list(linetype = c(1), size = c(10))))

ggsave(filename = "ssm_legend.tiff", plot = ssm, device = "tiff",path = "/Users/EmilyHardin/Desktop/Research/Acous.vs.Sat", width = 120, height = 120, units = c("mm"), dpi = 600)

