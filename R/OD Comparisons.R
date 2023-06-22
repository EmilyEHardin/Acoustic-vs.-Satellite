#### OD COMPARISONS ####
# Previous Script: Occurrence Distributions 

# The areas of all ODs were compiled into a table (UD.Comp.csv) and converted into km2. Here we will run comparisons and Welch's t-tests/Bayesian t-tests to compare the differences in size
# The areas of the ODs estimated from the matching and full temporal durations were also compiled into a table and will be compared here

library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggbreak)

UD.Comp <- read.csv("UD.Comp.csv", header = T)
Comp.Full.Eq <- read.csv("Comp.Full.Eq.csv", header = T)


## COMPARISONS OF ALL UDs ## 

# Acoustic Equal Temporal Scale 
mean(UD.Comp$Equal_Ac_95, na.rm = T) ## 3.237058 (A, C, D, G, I)
sd <- sd(UD.Comp$Equal_Ac_95, na.rm = T) ## 2.09204 
sd/sqrt(5) ## 0.94
mean(UD.Comp$Equal_Ac_50, na.rm = T) ## 0.2945678 (A, C, D, G, I)
sd <- sd(UD.Comp$Equal_Ac_50, na.rm = T) ## 0.08116695     
sd/sqrt(5)
# Satellite Equal Temporal Scale 
mean(UD.Comp$Equal_Sat_95, na.rm = T) ## 195.696 (A, C, D, E, G, H, I)
sd <- sd(UD.Comp$Equal_Sat_95, na.rm = T) ## 385.6383   
sd/sqrt(7)
mean(UD.Comp$Equal_Sat_50, na.rm = T) ## 10.24263 (A, C, D, E, G, H, I)
sd <- sd(UD.Comp$Equal_Sat_50, na.rm = T) ## 14.53882
sd/sqrt(7)

boxplot(UD.Comp$Equal_Ac_95, UD.Comp$Equal_Ac_50, UD.Comp$Equal_Sat_95, UD.Comp$Equal_Sat_50, main = "Matching Temporal Duration UDs", ylab = "km^2", ylim = c(0, 140), names = c("Acoustic 95%", "Acoustic 50%","Satellite 95%", "Satellite 50%"))

# Acoustic Full Temporal Scale 
mean(UD.Comp$Full_Ac_95, na.rm = T) ## 3.227929 (A, C, D, F, G, I)
sd <- sd(UD.Comp$Full_Ac_95, na.rm = T) ## 1.753581 
sd/sqrt(6)
mean(UD.Comp$Full_Ac_50, na.rm = T) ## 0.2993999 (A, C, D, F, G, I)
sd <- sd(UD.Comp$Full_Ac_50, na.rm = T) ## 0.06744268 
sd/sqrt(6)
# Satellite Full Temporal Scale 
mean(UD.Comp$Full_Sat_95, na.rm = T) ## 157.7311 (A, B, C, D, E, G, H, I)
sd <- sd(UD.Comp$Full_Sat_95, na.rm = T) ## 336.8726 
sd/sqrt(8)
mean(UD.Comp$Full_Sat_50, na.rm = T) ## 5.690106 (A, B, C, D, E, G, H, I)
sd <- sd(UD.Comp$Full_Sat_50, na.rm = T) ## 8.622876 
sd/sqrt(8)

boxplot(UD.Comp$Full_Ac_95, UD.Comp$Full_Ac_50, UD.Comp$Full_Sat_95, UD.Comp$Full_Sat_50, main = "Full Temporal Duration UDs", ylab = "km^2", ylim = c(0, 140), names = c("Acoustic 95%", "Acoustic 50%","Satellite 95%", "Satellite 50%"))


## COMPARISONS OF UDs OF 5 TURTLES SUCCESSFULLY TRACKED WITH BOTH TELEMETRY METHODS ## 

# Only Turtles A, C, D, G, and I have both Acoustic and Satellite UDs and therefore only these individuals can be used in comparisons of tracking types 
# Create a new dataframe containing only these individuals 
UD.Comp.5 <- UD.Comp %>%
  filter(ID == "A" | ID == "C" | ID == "D" | ID == "G" | ID == "I")

# Acoustic Equal Temporal Scale 
mean(UD.Comp.5$Equal_Ac_95)
sd <- sd(UD.Comp.5$Equal_Ac_95)
sd/(sqrt(5))
mean(UD.Comp.5$Equal_Ac_50)
sd <- sd(UD.Comp.5$Equal_Ac_50)
sd/(sqrt(5))
# Satellite Equal Temporal Scale 
mean(UD.Comp.5$Equal_Sat_95)
sd <- sd(UD.Comp.5$Equal_Sat_95)
sd/(sqrt(5))
mean(UD.Comp.5$Equal_Sat_50)
sd <- sd(UD.Comp.5$Equal_Sat_50)
sd/(sqrt(5))

# Acoustic Full Temporal Scale 
mean(UD.Comp.5$Full_Ac_95)
  range(UD.Comp.5$Full_Ac_95)
  sd <- sd(UD.Comp.5$Full_Ac_95)
  sd/(sqrt(5))  
mean(UD.Comp.5$Full_Ac_50)
  range(UD.Comp.5$Full_Ac_50)
  sd <- sd(UD.Comp.5$Full_Ac_50)
  sd/(sqrt(5))  
# Satellite Full Temporal Scale 
mean(UD.Comp.5$Full_Sat_95)
  range(UD.Comp.5$Full_Sat_95)
  sd <- sd(UD.Comp.5$Full_Sat_95)
  sd/(sqrt(5))  
mean(UD.Comp.5$Full_Sat_50)
  range(UD.Comp.5$Full_Sat_50)
  sd <- sd(UD.Comp.5$Full_Sat_50)
  sd/(sqrt(5))  
  

## BOXPLOTS ## 

# Matching Temporal Duration
UD <- read.csv("UD.Comp_Equal.csv", header = T) # matching temporal duration UDs for 5 comparative turtles 
yl <- expression(paste("OD Size (",km^2,")",sep=""))

M.UD.box <- ggplot(UD, aes(x = Method, y = UD, fill = Method)) + 
  geom_boxplot(outlier.colour = "black", outlier.shape = 21, outlier.size = 2, show.legend = T) +
  ylab(yl) +
  xlab("Occurrence Distributions (ODs) ") +
  scale_fill_manual(values = c("grey40", "grey")) +
  scale_y_continuous(limits = c(0, 130), breaks = c(0, 20, 80, 100, 120)) +
  theme(legend.position = "right", 
        legend.text = element_text(size = 8)) +
  theme_bw() +
  theme(text = element_text(size = 30)) +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5)) + 
  theme(axis.ticks.y = element_line(size = 0.75), 
        axis.ticks.y.right = element_blank(), 
        axis.text.y.right = element_blank()) + 
  theme(axis.ticks.x = element_line(size = 0.75)) +
  theme(axis.text.x=element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.spacing.x = unit(0, "line")) + 
  facet_wrap(~Contour) + 
  scale_y_break(c(22, 80), space = 0.06)
ggsave(filename = "M.UD.box.tiff", plot = M.UD.box, device = "tiff", path = "/Users/EmilyHardin/Desktop/Research/Acous.vs.Sat/Data & Code", width = 240, height = 240, units = c("mm"), dpi = 600)

# Equal Temporal Duration
UD.Full <- read.csv("UD.Comp_Full.csv", header = T)
F.UD.box <- ggplot(UD.Full, aes(x = Method, y = UD, fill = Method)) + 
  geom_boxplot(outlier.colour = "black", outlier.shape = 21, outlier.size = 2, show.legend = T) +
  ylab(yl) +
  xlab("Occurrence Distributions (ODs) ") +
  scale_fill_manual(values = c("grey40", "grey")) +
  scale_y_continuous(limits = c(0, 130), breaks = c(0, 10, 20, 80, 100, 120)) +
  theme(legend.position = "right", 
        legend.text = element_text(size = 8)) +
  labs(fill = "Telemetry\nMethod") +
  theme_bw() +
  theme(text = element_text(size = 30)) +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5)) + 
  theme(axis.ticks.y = element_line(size = 0.75), 
        axis.ticks.y.right = element_blank(), 
        axis.text.y.right = element_blank()) + 
  theme(axis.ticks.x = element_line(size = 0.75)) +
  theme(axis.text.x=element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.spacing.x = unit(0, "line")) + 
  facet_wrap(~Contour) +
  scale_y_break(c(22, 80), space = 0.06)
ggsave(filename = "F.UD.box.tiff", plot = F.UD.box, device = "tiff", path = "/Users/EmilyHardin/Desktop/Research/Acous.vs.Sat/Data & Code", width = 240, height = 240, units = c("mm"), dpi = 600)

## T-TESTS ##

# Equal Temporal Duration - 95% 
boxplot(UD.Comp.5$Equal_Ac_95, UD.Comp.5$Equal_Sat_95)
t.test(UD.Comp.5$Equal_Ac_95, UD.Comp.5$Equal_Sat_95, paired = T, var.equal = F)


# Equal Temporal Duration - 50% 
boxplot(UD.Comp.5$Equal_Ac_50, UD.Comp.5$Equal_Sat_50) 
t.test(UD.Comp.5$Equal_Ac_50, UD.Comp.5$Equal_Sat_50, paired = T, var.equal = F)


# Full Temporal Duration - 95% 
boxplot(UD.Comp.5$Full_Ac_95, UD.Comp.5$Full_Sat_95)
t.test(UD.Comp.5$Full_Ac_95, UD.Comp.5$Full_Sat_95, paired = T, var.equal = F)


# Full Temporal Duration - 50% 
boxplot(UD.Comp.5$Full_Ac_50, UD.Comp.5$Full_Sat_50) 
t.test(UD.Comp.5$Full_Ac_50, UD.Comp.5$Full_Sat_50, paired = T, var.equal = F)


## BAYESIAN TESTS ##

# Equal Temporal Duration - 95% 
t.test(UD.Comp.5$Equal_Ac_95, UD.Comp.5$Equal_Sat_95, paired = T, var.equal = F)
BEST.mat.95.BB <- BESTmcmc(UD.Comp.5$Equal_Ac_95, UD.Comp.5$Equal_Sat_95, parallel= F)
plot(BEST.mat.95.BB)
# 89.2% chance that satellite UDs are larger than acoustic UDs

# Equal Temporal Duration - 50% 
BEST.mat.50.BB <- BESTmcmc(UD.Comp.5$Equal_Ac_50, UD.Comp.5$Equal_Sat_50, parallel = F)
plot(BEST.mat.50.BB)
# 90.9% 

# Full Temporal Duration - 95% 
BEST.full.95.BB <- BESTmcmc(UD.Comp.5$Full_Ac_95, UD.Comp.5$Full_Sat_95, parallel = F)
plot(BEST.full.95.BB)
# 89.3%

# Full Temporal Duration - 50% 
BEST.full.50.BB <- BESTmcmc(UD.Comp.5$Full_Ac_50, UD.Comp.5$Full_Sat_50, parallel = F)
plot(BEST.full.50.BB)
# 88.4% 


## CALCULATING HOW MUCH LARGER SATELLITE UDs ARE THAN ACOUSTIC UDs ## 

# Equal Temporal Duration 95% 
UD.Comp.5$Equal_Inc_95 <- UD.Comp.5$Equal_Sat_95/UD.Comp.5$Equal_Ac_95
mean(UD.Comp.5$Equal_Inc_95, na.rm = T) # on average 12x larger than acoustic 

# Equal Temporal Duration 50% 
UD.Comp.5$Equal_Inc_50 <- UD.Comp.5$Equal_Sat_50/UD.Comp.5$Equal_Ac_50
mean(UD.Comp.5$Equal_Inc_50, na.rm = T) # on average 15x larger than acoustic

# Full Temporal Duration 95% 
UD.Comp.5$Full_Inc_95 <- UD.Comp.5$Full_Sat_95/UD.Comp.5$Full_Ac_95
mean(UD.Comp.5$Full_Inc_95, na.rm = T) # on average 11.5x larger than acoustic 

# Full Temporal Duration 50%
UD.Comp.5$Full_Inc_50 <- UD.Comp.5$Full_Sat_50/UD.Comp.5$Full_Ac_50
mean(UD.Comp.5$Full_Inc_50, na.rm = T) # on average 10x larger than acoustic


## ASSESSING EFFECT OF FULL TEMPORAL DURATION VS. EQUAL TEMPORAL DURATION ##

# Equal vs. Full Temporal Duration 95% UDs
boxplot(Comp.Full.Eq$Equal_95, Comp.Full.Eq$Full_95) # E is a potential outlier
t.test(Comp.Full.Eq$Equal_95, Comp.Full.Eq$Full_95, paired = T, var.equal = F)

BEST.7 <- BESTmcmc(Comp.Full.Eq$Equal_95, Comp.Full.Eq$Full_95, parallel = F)
plot(BEST.7) # 47.6% chance that full is larger than matching (52.4% that matching is larger than full)


# Equal vs. Full Temporal Duration 50% UDs
boxplot(Comp.Full.Eq$Equal_50, Comp.Full.Eq$Full_50) # E is a potential outlier
t.test(Comp.Full.Eq$Equal_50, Comp.Full.Eq$Full_50, paired = T, var.equal = F)

BEST.8 <- BESTmcmc(Comp.Full.Eq$Equal_50, Comp.Full.Eq$Full_50, parallel = F)
plot(BEST.8) # 34.8% chance that full is larger than matching (65.2% that equal is larger)


# If grouped by which tracking lasted longer:
# Acoustic 
Comp.Ac <- Comp.Full.Eq %>% filter(X == "A" | X == "C" | X == "D") # filter the turtles that were tracked longer with acoustic 

t.test(Comp.Ac$Equal_95, Comp.Ac$Full_95, paired = T, var.equal = F) # compare equal and full 95% UDs
BEST.9 <- BESTmcmc(Comp.Ac$Equal_95, Comp.Ac$Full_95, parallel = F)
plot(BEST.9) # 52.5% chance that full is larger
Comp.Ac$Inc_95 <- Comp.Ac$Full_95/Comp.Ac$Equal_95
  mean(Comp.Ac$Inc_95) # Full acoustic were  1.2 x larger than Matching acoustic UDs

  
t.test(Comp.Ac$Equal_50, Comp.Ac$Full_50, paired = T, var.equal = F) # compare equal and full 50% UDs
BEST.10 <- BESTmcmc(Comp.Ac$Equal_50, Comp.Ac$Full_50, parallel = F) 
plot(BEST.10) #52.7% chance that full is larger than matching 
Comp.Ac$Inc_50 <- Comp.Ac$Full_50/Comp.Ac$Equal_50
  mean(Comp.Ac$Inc_50) # Full acoustic were 1.06x larger than Matching acoustic UDs

# Satellite 
Comp.Sat <- Comp.Full.Eq %>% filter(X == "E" | X == "G" | X == "H" | X == "I") # filter the turtles that were tracked longer with satellite 
t.test(Comp.Sat$Equal_95, Comp.Sat$Full_95, paired = T, var.equal = F) # compare equal and full 95% UDs
BEST.11 <- BESTmcmc(Comp.Sat$Equal_95, Comp.Sat$Full_95, parallel = F)
plot(BEST.11) #47.8% that full is larger
Comp.Sat$Inc_95 <- Comp.Sat$Equal_95/Comp.Sat$Full_95
  mean(Comp.Sat$Inc_95) # Matching UD were 1.2 x larger than Full satellite UDs

t.test(Comp.Sat$Equal_50, Comp.Sat$Full_50, paired = T, var.equal = F) # compare equal and full 50% UDs
BEST.12 <- BESTmcmc(Comp.Sat$Equal_50, Comp.Sat$Full_50, parallel = F)
plot(BEST.12) #36.2% chance that full is larger than equal (63.8% equal is larger)
Comp.Sat$Inc_50 <- Comp.Sat$Equal_50/Comp.Sat$Full_50 
mean(Comp.Sat$Inc_50) # Equal UD were 2x larger than Full satellite UDs


# Next Script: Overlap Indices for ODs






