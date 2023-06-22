### RANGE DISTRIBUTIONS ###
# Previous script: Individual AKDEs_RDs

library(ggplot2)
library(ggspatial)
library(sf)
library(dplyr)

# here we will convert all of the AKDEs to sf object polygons to plot within ggplot2

# import land layer in as an sf object 
bimini <- st_read(dsn = "bimini shapefile/bimini_shape.shp", quiet = TRUE)
bimini <- bimini[bimini$AltMode == "0", ] # extract just one layer from the collection 
bimini <- st_union(bimini) # make it a polygon layer
bimini <- st_transform(bimini, 3395) #transform to world mercator 

# import receiver locations to plot 
recdata2 <- read.csv("rec_final.csv", header = TRUE)
recdata2 <- st_as_sf(recdata2, coords = c("lon", "lat"), crs = 4326)
recdata2 <- st_transform(recdata2, 3395)
recdata2$lon<-st_coordinates(recdata2)[,1]
recdata2$lat<-st_coordinates(recdata2)[,2]

# convert the AKDE 95% and 50% UDs to sf object
# Turtle A #
As_95 <- as.sf(akde_As, level.UD = 0.95)
As_95 <- As_95[2,] # the 95% CI min and max will also be converted, so select just the estimate
As_50 <- as.sf(akde_As, level.UD = 0.50)
As_50 <- As_50[2,]

Ass_95 <- as.sf(akde_Ass, level.UD = 0.95)
Ass_95 <- Ass_95[2,] 
Ass_50 <- as.sf(akde_Ass, level.UD = 0.50)
Ass_50 <- Ass_50[2,]

Aa_95 <- as.sf(akde_Aa, level.UD = 0.95)
Aa_95 <- Aa_95[2,] 
Aa_50 <- as.sf(akde_Aa, level.UD = 0.50)
Aa_50 <- Aa_50[2,]

Aas_95 <- as.sf(akde_Aas, level.UD = 0.95)
Aas_95 <- Aas_95[2,] 
Aas_50 <- as.sf(akde_Aas, level.UD = 0.50)
Aas_50 <- Aas_50[2,]

# Turtle B
Bs_95 <- as.sf(akde_Bs, level.UD = 0.95)
Bs_95 <- Bs_95[2,] 
Bs_50 <- as.sf(akde_Bs, level.UD = 0.50)
Bs_50 <- Bs_50[2,]

Bss_95 <- as.sf(akde_Bss, level.UD = 0.95)
Bss_95 <- Bss_95[2,] 
Bss_50 <- as.sf(akde_Bss, level.UD = 0.50)
Bss_50 <- Bss_50[2,]

# Turtle C 
Cs_95 <- as.sf(akde_Cs, level.UD = 0.95)
Cs_95 <- Cs_95[2,] 
Cs_50 <- as.sf(akde_Cs, level.UD = 0.50)
Cs_50 <- Cs_50[2,]

Css_95 <- as.sf(akde_Css, level.UD = 0.95)
Css_95 <- Css_95[2,] 
Css_50 <- as.sf(akde_Css, level.UD = 0.50)
Css_50 <- Css_50[2,]

Ca_95 <- as.sf(akde_Ca, level.UD = 0.95)
Ca_95 <- Ca_95[2,] 
Ca_50 <- as.sf(akde_Ca, level.UD = 0.50)
Ca_50 <- Ca_50[2,]

Cas_95 <- as.sf(akde_Cas, level.UD = 0.95)
Cas_95 <- Cas_95[2,] 
Cas_50 <- as.sf(akde_Cas, level.UD = 0.50)
Cas_50 <- Cas_50[2,]

# Turtle D
Ds_95 <- as.sf(akde_Ds, level.UD = 0.95)
Ds_95 <- Ds_95[2,] 
Ds_50 <- as.sf(akde_Ds, level.UD = 0.50)
Ds_50 <- Ds_50[2,]

Dss_95 <- as.sf(akde_Dss, level.UD = 0.95)
Dss_95 <- Dss_95[2,] 
Dss_50 <- as.sf(akde_Dss, level.UD = 0.50)
Dss_50 <- Dss_50[2,]

Da_95 <- as.sf(akde_Da, level.UD = 0.95)
Da_95 <- Da_95[2,] 
Da_50 <- as.sf(akde_Da, level.UD = 0.50)
Da_50 <- Da_50[2,]

Das_95 <- as.sf(akde_Das, level.UD = 0.95)
Das_95 <- Das_95[2,] 
Das_50 <- as.sf(akde_Das, level.UD = 0.50)
Das_50 <- Das_50[2,]

# Turtle E
Es_95 <- as.sf(akde_Es, level.UD = 0.95)
Es_95 <- Es_95[2,] 
Es_50 <- as.sf(akde_Es, level.UD = 0.50)
Es_50 <- Es_50[2,]

Ess_95 <- as.sf(akde_Ess, level.UD = 0.95)
Ess_95 <- Ess_95[2,] 
Ess_50 <- as.sf(akde_Ess, level.UD = 0.50)
Ess_50 <- Ess_50[2,]

# Turtle F
Fa_95 <- as.sf(akde_Fa, level.UD = 0.95)
Fa_95 <- Fa_95[2,] 
Fa_50 <- as.sf(akde_Fa, level.UD = 0.50)
Fa_50 <- Fa_50[2,]

# Turtle G
Gs_95 <- as.sf(akde_Gs, level.UD = 0.95)
Gs_95 <- Gs_95[2,] 
Gs_50 <- as.sf(akde_Gs, level.UD = 0.50)
Gs_50 <- Gs_50[2,]

Gss_95 <- as.sf(akde_Gss, level.UD = 0.95)
Gss_95 <- Gss_95[2,] 
Gss_50 <- as.sf(akde_Gss, level.UD = 0.50)
Gss_50 <- Gss_50[2,]

Ga_95 <- as.sf(akde_Ga, level.UD = 0.95)
Ga_95 <- Ga_95[2,] 
Ga_50 <- as.sf(akde_Ga, level.UD = 0.50)
Ga_50 <- Ga_50[2,]

# Turtle H
Hs_95 <- as.sf(akde_Hs, level.UD = 0.95)
Hs_95 <- Hs_95[2,] 
Hs_50 <- as.sf(akde_Hs, level.UD = 0.50)
Hs_50 <- Hs_50[2,]

Hss_95 <- as.sf(akde_Hss, level.UD = 0.95)
Hss_95 <- Hss_95[2,] 
Hss_50 <- as.sf(akde_Hss, level.UD = 0.50)
Hss_50 <- Hss_50[2,]

# Turtle I 
Is_95 <- as.sf(akde_Is, level.UD = 0.95)
Is_95 <- Is_95[2,] 
Is_50 <- as.sf(akde_Is, level.UD = 0.50)
Is_50 <- Is_50[2,]

Iss_95 <- as.sf(akde_Iss, level.UD = 0.95)
Iss_95 <- Iss_95[2,] 
Iss_50 <- as.sf(akde_Iss, level.UD = 0.50)
Iss_50 <- Iss_50[2,]

Ia_95 <- as.sf(akde_Ia, level.UD = 0.95)
Ia_95 <- Ia_95[2,] 
Ia_50 <- as.sf(akde_Ia, level.UD = 0.50)
Ia_50 <- Ia_50[2,]


## PLOTTING ##

# Turtle A - matching temporal duration
A_mat_AKDE <- ggplot() +
  geom_sf(data = bimini, fill = "grey", col = "grey") +
  geom_sf(data = Ass_95, fill = "turquoise", col = "turquoise2", lwd = 0.4, alpha = 0.3) + 
  geom_sf(data = Ass_50, fill = "cyan4", col = "cyan4", lwd = 0.4, alpha = 0.7) +   
  geom_sf(data = Aas_95, fill = "tomato", col = "tomato", lwd = 0.4, alpha = 0.7) + 
  geom_sf(data = Aas_50, fill = "orangered4", col = "orangered4", lwd = 0.4, alpha = 0.5) +   
  geom_point(data = recdata2, aes(x = lon, y = lat, col = "grey28"), col = "grey28", size = 0.8) +
  coord_sf(crs = st_crs(3395), xlim = c(-8803537.70, -8840201.59), ylim = c(2932987.06, 2963539.88)) + 
  scale_x_continuous(breaks = c(-79.35, -79.25, -79.15)) +
  scale_y_continuous(breaks = c(25.66, 25.74, 25.82)) +
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
  ggplot2::annotate("text", x = -8840201.59, y = 2963539.88, label = "Turtle A", size = 6.5, hjust = 0, vjust = 1)

ggsave(filename = "A_mat_AKDE.tiff", plot = A_mat_AKDE, device = "tiff",path = "/Users/EmilyHardin/Desktop/Research/Acous.vs.Sat", width = 120, height = 120, units = c("mm"), dpi = 600)

# Turtle A - full temporal duration
A_full_AKDE <- ggplot() +
  geom_sf(data = bimini, fill = "grey", col = "grey") +
  geom_sf(data = As_95, fill = "turquoise", col = "turquoise2", lwd = 0.4, alpha = 0.3) + 
  geom_sf(data = Aa_95, fill = "tomato", col = "tomato", lwd = 0.4, alpha = 0.7) + 
  geom_sf(data = As_50, fill = "cyan4", col = "cyan4", lwd = 0.4, alpha = 0.7) +   
  geom_sf(data = Aa_50, fill = "orangered4", col = "orangered4", lwd = 0.4, alpha = 0.5) +   
  geom_point(data = recdata2, aes(x = lon, y = lat, col = "grey28"), col = "grey28", size = 0.8) +
  coord_sf(crs = st_crs(3395), xlim = c(-8803537.70, -8840201.59), ylim = c(2932987.06, 2963539.88)) + 
  scale_x_continuous(breaks = c(-79.35, -79.25, -79.15)) +
  scale_y_continuous(breaks = c(25.66, 25.74, 25.82)) +
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
  ggplot2::annotate("text", x = -8840201.59, y = 2963539.88, label = "Turtle A", size = 6.5, hjust = 0, vjust = 1)
ggsave(filename = "A_full_AKDE.tiff", plot = A_full_AKDE, device = "tiff",path = "/Users/EmilyHardin/Desktop/Research/Acous.vs.Sat", width = 120, height = 120, units = c("mm"), dpi = 600)

# Turtle B
B_full_AKDE <- ggplot() +
  geom_sf(data = bimini, fill = "grey", col = "grey") +
  geom_sf(data = Bs_95, fill = "turquoise", col = "turquoise2", lwd = 0.4, alpha = 0.3) +
  geom_sf(data = Bs_50, fill = "cyan4", col = "cyan4", lwd = 0.4, alpha = 0.7) +
  geom_point(data = recdata2, aes(x = lon, y = lat), col = "grey28", size = 0.8) +
  coord_sf(crs = st_crs(3395), xlim = c(-8803537.70, -8840201.59), ylim = c(2932987.06, 2963539.88)) + 
  scale_x_continuous(breaks = c(-79.35, -79.25, -79.15)) +
  scale_y_continuous(breaks = c(25.66, 25.74, 25.82)) +
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
  ggplot2::annotate("text", x = -8840201.59, y = 2963539.88, label = "Turtle B", size = 6.5, hjust = 0, vjust = 1)
ggsave(filename = "B_full_AKDE.tiff", plot = B_full_AKDE, device = "tiff",path = "/Users/EmilyHardin/Desktop/Research/Acous.vs.Sat", width = 120, height = 120, units = c("mm"), dpi = 600)

# Turtle C 
C_mat_AKDE <- ggplot() +
  geom_sf(data = bimini, fill = "grey", col = "grey") +
  geom_sf(data = Css_95, fill = "turquoise", col = "turquoise2", lwd = 0.4, alpha = 0.3) +
  geom_sf(data = Css_50, fill = "cyan4", col = "cyan4", lwd = 0.4, alpha = 0.7) +
  geom_sf(data = Cas_95, fill = "tomato", col = "tomato", lwd = 0.4, alpha = 0.7) +
  geom_sf(data = Cas_50, fill = "orangered4", col = "orangered4", lwd = 0.4, alpha = 0.5) +
  geom_point(data = recdata2, aes(x = lon, y = lat), col = "grey28", size = 0.8) +
  coord_sf(crs = st_crs(3395), xlim = c(-8803537.70, -8840201.59), ylim = c(2932987.06, 2963539.88)) + 
  scale_x_continuous(breaks = c(-79.35, -79.25, -79.15)) +
  scale_y_continuous(breaks = c(25.66, 25.74, 25.82)) +
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
  ggplot2::annotate("text", x = -8840201.59, y = 2963539.88, label = "Turtle C", size = 6.5, hjust = 0, vjust = 1)
ggsave(filename = "C_mat_AKDE.tiff", plot = C_mat_AKDE, device = "tiff",path = "/Users/EmilyHardin/Desktop/Research/Acous.vs.Sat", width = 120, height = 120, units = c("mm"), dpi = 600)


C_full_AKDE <- ggplot() +
  geom_sf(data = bimini, fill = "grey", col = "grey") +
  geom_sf(data = Cs_95, fill = "turquoise", col = "turquoise2", lwd = 0.4, alpha = 0.3) +
  geom_sf(data = Cs_50, fill = "cyan4", col = "cyan4", lwd = 0.4, alpha = 0.7) +
  geom_sf(data = Ca_95, fill = "tomato", col = "tomato", lwd = 0.4, alpha = 0.7) +
  geom_sf(data = Ca_50, fill = "orangered4", col = "orangered4", lwd = 0.4, alpha = 0.5) +
  geom_point(data = recdata2, aes(x = lon, y = lat), col = "grey28", size = 0.8) +
  coord_sf(crs = st_crs(3395), xlim = c(-8803537.70, -8840201.59), ylim = c(2932987.06, 2963539.88)) + 
  scale_x_continuous(breaks = c(-79.35, -79.25, -79.15)) +
  scale_y_continuous(breaks = c(25.66, 25.74, 25.82)) +
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
  ggplot2::annotate("text", x = -8840201.59, y = 2963539.88, label = "Turtle C", size = 6.5, hjust = 0, vjust = 1)
ggsave(filename = "C_full_AKDE.tiff", plot = C_full_AKDE, device = "tiff",path = "/Users/EmilyHardin/Desktop/Research/Acous.vs.Sat", width = 120, height = 120, units = c("mm"), dpi = 600)

# Turtle D
D_mat_AKDE <- ggplot() +
  geom_sf(data = bimini, fill = "grey", col = "grey") +
  geom_sf(data = Dss_95, fill = "turquoise", col = "turquoise2", lwd = 0.4, alpha = 0.3) +
  geom_sf(data = Dss_50, fill = "cyan4", col = "cyan4", lwd = 0.4, alpha = 0.7) +
  geom_sf(data = Das_95, fill = "tomato", col = "tomato", lwd = 0.4, alpha = 0.7) +
  geom_sf(data = Das_50, fill = "orangered4", col = "orangered4", lwd = 0.4, alpha = 0.5) +
  geom_point(data = recdata2, aes(x = lon, y = lat), col = "grey28", size = 0.8) +
  coord_sf(crs = st_crs(3395), xlim = c(-8803537.70, -8840201.59), ylim = c(2932987.06, 2963539.88)) + 
  scale_x_continuous(breaks = c(-79.35, -79.25, -79.15)) +
  scale_y_continuous(breaks = c(25.66, 25.74, 25.82)) +
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
  ggplot2::annotate("text", x = -8840201.59, y = 2963539.88, label = "Turtle D", size = 6.5, hjust = 0, vjust = 1)
ggsave(filename = "D_mat_AKDE.tiff", plot = D_mat_AKDE, device = "tiff",path = "/Users/EmilyHardin/Desktop/Research/Acous.vs.Sat", width = 120, height = 120, units = c("mm"), dpi = 600)


D_full_AKDE <- ggplot() +
  geom_sf(data = bimini, fill = "grey", col = "grey") +
  geom_sf(data = Ds_95, fill = "turquoise", col = "turquoise2", lwd = 0.4, alpha = 0.3) +
  geom_sf(data = Ds_50, fill = "cyan4", col = "cyan4", lwd = 0.4, alpha = 0.7) +
  geom_sf(data = Da_95, fill = "tomato", col = "tomato", lwd = 0.4, alpha = 0.7) +
  geom_sf(data = Da_50, fill = "orangered4", col = "orangered4", lwd = 0.4, alpha = 0.5) +
  geom_point(data = recdata2, aes(x = lon, y = lat), col = "grey28", size = 0.8) +
  coord_sf(crs = st_crs(3395), xlim = c(-8803537.70, -8840201.59), ylim = c(2932987.06, 2963539.88)) + 
  scale_x_continuous(breaks = c(-79.35, -79.25, -79.15)) +
  scale_y_continuous(breaks = c(25.66, 25.74, 25.82)) +
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
  ggplot2::annotate("text", x = -8840201.59, y = 2963539.88, label = "Turtle D", size = 6.5, hjust = 0, vjust = 1)
ggsave(filename = "D_full_AKDE.tiff", plot = D_full_AKDE, device = "tiff",path = "/Users/EmilyHardin/Desktop/Research/Acous.vs.Sat", width = 120, height = 120, units = c("mm"), dpi = 600)

# Turtle E
E_mat_AKDE <- ggplot() +
  geom_sf(data = bimini, fill = "grey", col = "grey") +
  geom_sf(data = Ess_95, fill = "turquoise", col = "turquoise2", lwd = 0.4, alpha = 0.3, aes(fill = "")) +
  geom_sf(data = Ess_50, fill = "cyan4", col = "cyan4", lwd = 0.4, alpha = 0.7, aes(fill = "")) +
  geom_point(data = recdata2, aes(x = lon, y = lat), col = "grey28", size = 0.8) +
  coord_sf(crs = st_crs(3395), xlim = c(-8785537.70, -8876201.59), ylim = c(2900987.06, 2976540.3)) + 
  scale_x_continuous(breaks = c(-79.6, -79.3, -79.0)) +
  scale_y_continuous(breaks = c(25.45, 25.65, 25.85)) +
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
  ggplot2::annotate("text", x = -8876201.59, y = 2976540.3, label = "Turtle E", size = 6.5, hjust = 0, vjust = 1)
ggsave(filename = "E_mat_AKDE.tiff", plot = E_mat_AKDE, device = "tiff",path = "/Users/EmilyHardin/Desktop/Research/Acous.vs.Sat", width = 120, height = 120, units = c("mm"), dpi = 600)


E_full_AKDE <- ggplot() +
  geom_sf(data = bimini, fill = "grey", col = "grey") +
  geom_sf(data = Es_95, fill = "turquoise", col = "turquoise2", lwd = 0.4, alpha = 0.3) +
  geom_sf(data = Es_50, fill = "cyan4", col = "cyan4", lwd = 0.4, alpha = 0.7) +
  geom_point(data = recdata2, aes(x = lon, y = lat), col = "grey28", size = 0.8) +
  coord_sf(crs = st_crs(3395), xlim = c(-8785537.70, -8876201.59), ylim = c(2900987.06, 2976540.3)) + 
  scale_x_continuous(breaks = c(-79.6, -79.3, -79.0)) +
  scale_y_continuous(breaks = c(25.45, 25.65, 25.85)) +
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
  ggplot2::annotate("text", x = -8876201.59, y = 2976540.3, label = "Turtle E", size = 6.5, hjust = 0, vjust = 1)
ggsave(filename = "E_full_AKDE.tiff", plot = E_full_AKDE, device = "tiff",path = "/Users/EmilyHardin/Desktop/Research/Acous.vs.Sat", width = 120, height = 120, units = c("mm"), dpi = 600)
# xlim = c(-8785537.70, -8876201.59)

# Turtle F
F_full_AKDE <- ggplot() +
  geom_sf(data = bimini, fill = "grey", col = "grey") +
  geom_sf(data = Fa_95, fill = "tomato", col = "tomato", lwd = 0.4, alpha = 0.7) +
  geom_sf(data = Fa_50, fill = "orangered4", col = "orangered4", lwd = 0.4, alpha = 0.5) +
  geom_point(data = recdata2, aes(x = lon, y = lat), col = "grey28", size = 0.8) +
  coord_sf(crs = st_crs(3395), xlim = c(-8803537.70, -8840201.59), ylim = c(2932987.06, 2963539.88)) + 
  scale_x_continuous(breaks = c(-79.35, -79.25, -79.15)) +
  scale_y_continuous(breaks = c(25.66, 25.74, 25.82)) +
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
  ggplot2::annotate("text", x = -8840201.59, y = 2963539.88, label = "Turtle F", size = 6.5, hjust = 0, vjust = 1)
ggsave(filename = "F_full_AKDE.tiff", plot = F_full_AKDE, device = "tiff",path = "/Users/EmilyHardin/Desktop/Research/Acous.vs.Sat", width = 120, height = 120, units = c("mm"), dpi = 600)

# Turtle G
G_mat_AKDE <- ggplot() +
  geom_sf(data = bimini, fill = "grey", col = "grey") +
  geom_sf(data = Gss_95, fill = "turquoise", col = "turquoise2", lwd = 0.4, alpha = 0.3) +
  geom_sf(data = Gss_50, fill = "cyan4", col = "cyan4", lwd = 0.4, alpha = 0.7) +
  geom_sf(data = Ga_95, fill = "tomato", col = "tomato", lwd = 0.4, alpha = 0.7) +
  geom_sf(data = Ga_50, fill = "orangered4", col = "orangered4", lwd = 0.4, alpha = 0.5) +
  geom_point(data = recdata2, aes(x = lon, y = lat), col = "grey28", size = 0.8) +
  coord_sf(crs = st_crs(3395), xlim = c(-8803537.70, -8840201.59), ylim = c(2932987.06, 2963539.88)) + 
  scale_x_continuous(breaks = c(-79.35, -79.25, -79.15)) +
  scale_y_continuous(breaks = c(25.66, 25.74, 25.82)) +
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
  ggplot2::annotate("text", x = -8840201.59, y = 2963539.88, label = "Turtle G", size = 6.5, hjust = 0, vjust = 1)
ggsave(filename = "G_mat_AKDE.tiff", plot = G_mat_AKDE, device = "tiff",path = "/Users/EmilyHardin/Desktop/Research/Acous.vs.Sat", width = 120, height = 120, units = c("mm"), dpi = 600)

# Full Temporal Duration UD plot
G_full_AKDE <- ggplot() +
  geom_sf(data = bimini, fill = "grey", col = "grey") +
  geom_sf(data = Gs_95, fill = "turquoise", col = "turquoise", lwd = 0.4, alpha = 0.3) +
  geom_sf(data = Gs_50, fill = "cyan4", col = "cyan4", lwd = 0.4, alpha = 0.7) +
  geom_sf(data = Ga_95, fill = "tomato", col = "tomato", lwd = 0.4, alpha = 0.7) +
  geom_sf(data = Ga_50, fill = "orangered4", col = "orangered4", lwd = 0.4, alpha = 0.5) +
  geom_point(data = recdata2, aes(x = lon, y = lat), col = "grey28", size = 0.8) +
  coord_sf(crs = st_crs(3395), xlim = c(-8803537.70, -8840201.59), ylim = c(2932987.06, 2963539.88)) + 
  scale_x_continuous(breaks = c(-79.35, -79.25, -79.15)) +
  scale_y_continuous(breaks = c(25.66, 25.74, 25.82)) +
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
  ggplot2::annotate("text", x = -8840201.59, y = 2963539.88, label = "Turtle G", size = 6.5, hjust = 0, vjust = 1)
ggsave(filename = "G_full_AKDE.tiff", plot = G_full_AKDE, device = "tiff",path = "/Users/EmilyHardin/Desktop/Research/Acous.vs.Sat", width = 120, height = 120, units = c("mm"), dpi = 600)


# Turtle H
H_mat_AKDE <- ggplot() +
  geom_sf(data = bimini, fill = "grey", col = "grey") +
  geom_sf(data = Hss_95, fill = "turquoise", col = "turquoise2", lwd = 0.4, alpha = 0.3) +
  geom_sf(data = Hss_50, fill = "cyan4", col = "cyan4", lwd = 0.4, alpha = 0.7) +
  geom_point(data = recdata2, aes(x = lon, y = lat), col = "grey28", size = 0.8) +
  coord_sf(crs = st_crs(3395), xlim = c(-8803537.70, -8840201.59), ylim = c(2932987.06, 2963539.88)) + 
  scale_x_continuous(breaks = c(-79.35, -79.25, -79.15)) +
  scale_y_continuous(breaks = c(25.66, 25.74, 25.82)) +
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
  ggplot2::annotate("text", x = -8840201.59, y = 2963539.88, label = "Turtle H", size = 6.5, hjust = 0, vjust = 1)
ggsave(filename = "H_mat_AKDE.tiff", plot = H_mat_AKDE, device = "tiff",path = "/Users/EmilyHardin/Desktop/Research/Acous.vs.Sat", width = 120, height = 120, units = c("mm"), dpi = 600)

# Full Temporal Duration UD plot
H_full_AKDE <- ggplot() +
  geom_sf(data = bimini, fill = "grey", col = "grey") +
  geom_sf(data = Hs_95, fill = "turquoise", col = "turquoise2", lwd = 0.4, alpha = 0.3) +
  geom_sf(data = Hs_50, fill = "cyan4", col = "cyan4", lwd = 0.4, alpha = 0.7) +
  geom_point(data = recdata2, aes(x = lon, y = lat), col = "grey28", size = 0.8) +
  coord_sf(crs = st_crs(3395), xlim = c(-8803537.70, -8840201.59), ylim = c(2932987.06, 2963539.88)) + 
  scale_x_continuous(breaks = c(-79.35, -79.25, -79.15)) +
  scale_y_continuous(breaks = c(25.66, 25.74, 25.82)) +
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
  ggplot2::annotate("text", x = -8840201.59, y = 2963539.88, label = "Turtle H", size = 6.5, hjust = 0, vjust = 1)
ggsave(filename = "H_full_AKDE.tiff", plot = H_full_AKDE, device = "tiff",path = "/Users/EmilyHardin/Desktop/Research/Acous.vs.Sat", width = 120, height = 120, units = c("mm"), dpi = 600)

# Turtle I 
I_mat_AKDE <- ggplot() +
  geom_sf(data = bimini, fill = "grey", col = "grey") +
  geom_sf(data = Iss_95, fill = "turquoise", col = "turquoise2", lwd = 0.4, alpha = 0.3, aes(fill = "")) +
  geom_sf(data = Ia_95, fill = "tomato", col = "tomato", lwd = 0.4, alpha = 0.7, aes(fill = "")) +
  geom_sf(data = Iss_50, fill = "cyan4", col = "cyan4", lwd = 0.4, alpha = 0.7, aes(fill = "")) +
  geom_sf(data = Ia_50, fill = "orangered4", col = "orangered4", lwd = 0.4, alpha = 0.5, aes(fill = "")) +
  geom_point(data = recdata2, aes(x = lon, y = lat), col = "grey28", size = 0.8) +
  coord_sf(crs = st_crs(3395), xlim = c(-8795197.95, -8848541.34), ylim = c(2914087.06, 2958539.88)) + 
  scale_x_continuous(breaks = c(-79.40, -79.25, -79.10)) +
  scale_y_continuous(breaks = c(25.50, 25.62, 25.74)) +
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
  ggplot2::annotate("text", x = -8848541.34, y = 2958539.88, label = "Turtle I", size = 6.5, hjust = 0, vjust = 1)
ggsave(filename = "I_mat_AKDE.tiff", plot = I_mat_AKDE, device = "tiff",path = "/Users/EmilyHardin/Desktop/Research/Acous.vs.Sat", width = 120, height = 120, units = c("mm"), dpi = 600)


# Full Temporal Duration UD plot 
I_full_AKDE <- ggplot() +
  geom_sf(data = bimini, fill = "grey", col = "grey") +
  geom_sf(data = Is_95, fill = "turquoise", col = "turquoise2", lwd = 0.4, alpha = 0.3) +
  geom_sf(data = Ia_95, fill = "tomato", col = "tomato", lwd = 0.4, alpha = 0.7) +
  geom_sf(data = Is_50, fill = "cyan4", col = "cyan4", lwd = 0.4, alpha = 0.7) +
  geom_sf(data = Ia_50, fill = "orangered4", col = "orangered4", lwd = 0.4, alpha = 0.5) +
  geom_point(data = recdata2, aes(x = lon, y = lat), col = "grey28", size = 0.8) +
  coord_sf(crs = st_crs(3395), xlim = c(-8795197.95, -8848541.34), ylim = c(2914087.06, 2958539.88)) + 
  scale_x_continuous(breaks = c(-79.40, -79.25, -79.10)) +
  scale_y_continuous(breaks = c(25.50, 25.62, 25.74)) +
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
  ggplot2::annotate("text", x = -8848541.34, y = 2958539.88, label = "Turtle I", size = 6.5, hjust = 0, vjust = 1)
ggsave(filename = "I_full_AKDE.tiff", plot = I_full_AKDE, device = "tiff",path = "/Users/EmilyHardin/Desktop/Research/Acous.vs.Sat", width = 120, height = 120, units = c("mm"), dpi = 600)


## Legend 
legend_comp <- ggplot() +
  geom_sf(data = land, fill = "grey", col = "grey") +
  geom_sf(data = Ass_95, aes(fill = "turquoise"), col = "turquoise2", lwd = 0.25, alpha = 0.3, show.legend = "polygon") +
  geom_sf(data = Ass_50, aes(fill = "cyan4"), col = "cyan4", lwd = 0.25, alpha = 0.7, show.legend = "polygon") +
  geom_sf(data = Aas_95, aes(fill = "tomato"), col = "tomato", lwd = 0.25, alpha = 0.7, show.legend = "polygon") +
  geom_sf(data = Aas_50, aes(fill = "orangered4"), col = "orangered4", lwd = 0.25, alpha = 0.5, show.legend = "polygon") +
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
  scale_fill_identity(name = "Range Distributions", labels = c(turquoise = "Satellite 95%", cyan4 = "Satellite 50%", tomato = "Acoustic 95%", orangered4 = "Acoustic 50%"), limits = c("tomato", "orangered4", "turquoise", "cyan4"), guide = guide_legend(override.aes = list(linetype = c(rep("blank", 4)), shape = c(rep(NA, 4))))) + 
  scale_color_manual(values = c("grey28" = "grey28"), labels = c(grey28 = "Acoustic\nReceivers"), name = NULL,guide = guide_legend(override.aes = list(linetype = "blank", shape = 20, fill = NA, size = 3.5))) + 
  theme(legend.position = "right", 
        legend.text = element_text(size = 14), 
        legend.title = element_text(size = 14))
ggsave(filename = "legend_comp.tiff", plot = legend_comp, device = "tiff",path = "/Users/EmilyHardin/Desktop/Research/Acous.vs.Sat", width = 120, height = 120, units = c("mm"), dpi = 600)


## Next Script: RD Comparisons