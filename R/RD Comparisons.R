#### RD COMPARISONS ####
# Previous Script: Range Distributions 

# The areas of all RDs were compiled into a table (UD.Comp.AKDE.csv) and converted into km2. Here we will run comparisons, including Welch's t-tests and Bayesian t-tests to compare the differences in size
# The areas of the RDs estimated from the matching and full temporal durations were also compiled into a table and will be compared here

library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggbreak)

UD.Comp <- read.csv("UD.Comp.AKDE.csv", header = T)
Comp.Full.Eq <- read.csv("Comp.Full.Eq.AKDE.csv", header = T)

## COMPARISONS OF ALL RDs ## 

# Acoustic Equal Temporal Scale 
mean(UD.Comp$Equal_Ac_95, na.rm = T) ## (A, C, D, G, I)
sd <- sd(UD.Comp$Equal_Ac_95, na.rm = T) 
sd/sqrt(5) 
mean(UD.Comp$Equal_Ac_50, na.rm = T) ## (A, C, D, G, I)
sd <- sd(UD.Comp$Equal_Ac_50, na.rm = T)     
sd/sqrt(5)
# Satellite Equal Temporal Scale 
mean(UD.Comp$Equal_Sat_95, na.rm = T) ## (A, C, D, E, G, H, I)
sd <- sd(UD.Comp$Equal_Sat_95, na.rm = T)  
sd/sqrt(7) 
mean(UD.Comp$Equal_Sat_50, na.rm = T) # (A, C, D, E, G, H, I)
sd <- sd(UD.Comp$Equal_Sat_50, na.rm = T) 
sd/sqrt(7) 

boxplot(UD.Comp$Equal_Ac_95, UD.Comp$Equal_Ac_50, UD.Comp$Equal_Sat_95, UD.Comp$Equal_Sat_50, main = "Matching Temporal Duration UDs", ylab = "km^2", ylim = c(0, 900), names = c("Acoustic 95%", "Acoustic 50%","Satellite 95%", "Satellite 50%")) 

# Acoustic Full Temporal Scale 
mean(UD.Comp$Full_Ac_95, na.rm = T) # (A, C, D, F, G, I)
sd <- sd(UD.Comp$Full_Ac_95, na.rm = T) 
sd/sqrt(6) 
mean(UD.Comp$Full_Ac_50, na.rm = T) # (A, C, D, F, G, I)
sd <- sd(UD.Comp$Full_Ac_50, na.rm = T)  
sd/sqrt(6) 
# Satellite Full Temporal Scale 
mean(UD.Comp$Full_Sat_95, na.rm = T) # (A, B, C, D, E, G, H, I)
sd <- sd(UD.Comp$Full_Sat_95, na.rm = T)  
sd/sqrt(8) 
mean(UD.Comp$Full_Sat_50, na.rm = T) # (A, B, C, D, E, G, H, I)
sd <- sd(UD.Comp$Full_Sat_50, na.rm = T) 
sd/sqrt(8) 

boxplot(UD.Comp$Full_Ac_95, UD.Comp$Full_Ac_50, UD.Comp$Full_Sat_95, UD.Comp$Full_Sat_50, main = "Full Temporal Duration UDs", ylab = "km^2", ylim = c(0, 500), names = c("Acoustic 95%", "Acoustic 50%","Satellite 95%", "Satellite 50%"))


## COMPARISONS OF RDs OF 5 TURTLES SUCCESSFULLY TRACKED WITH BOTH TELEMETRY METHODS ## 

# Only Turtles A, C, D, G, and I have both Acoustic and Satellite RDs and therefore only these individuals can be used in comparisons of tracking types 
# Create a new data frame containing only these individuals 
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
UD <- read.csv("UD.Comp_Equal.AKDE.csv", header = T) # matching temporal duration UDs for 5 comparative turtles 
yl <- expression(paste("RD Size (",km^2,")",sep=""))

M.UD.box.AKDE <- ggplot(UD, aes(x = Method, y = UD, fill = Method)) + 
  geom_boxplot(outlier.colour = "black", outlier.shape = 21, outlier.size = 2, show.legend = T) +
  ylab(yl) +
  xlab("Range Distributions (RDs)             ") +
  scale_fill_manual(values = c("grey40", "grey")) +
  scale_y_continuous(limits = c(0, 900), breaks = c(0, 100, 200, 300, 875)) +
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
  scale_y_break(c(340, 830), space = 0.06)
ggsave(filename = "M.UD.box.AKDE.tiff", plot = M.UD.box.AKDE, device = "tiff", path = "/Users/EmilyHardin/Desktop/Research/Acous.vs.Sat/Data & Code", width = 240, height = 240, units = c("mm"), dpi = 600)


# Full Temporal Duration
UD.Full <- read.csv("UD.Comp_Full.AKDE.csv", header = T)
F.UD.box.AKDE <- ggplot(UD.Full, aes(x = Method, y = UD, fill = Method)) + 
  geom_boxplot(outlier.colour = "black", outlier.shape = 21, outlier.size = 2, show.legend = T) +
  ylab(yl) +
  xlab("Range Distributions (RDs)             ") +
  scale_fill_manual(values = c("grey40", "grey")) +
  scale_y_continuous(limits = c(0, 400), breaks = c(0, 100, 200, 300, 400)) +
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
  facet_wrap(~Contour)
ggsave(filename = "F.UD.box.AKDE.tiff", plot = F.UD.box.AKDE, device = "tiff", path = "/Users/EmilyHardin/Desktop/Research/Acous.vs.Sat/Data & Code", width = 240, height = 240, units = c("mm"), dpi = 600)


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

## Bayesian Tests ##
# Equal Temporal Duration - 95% 
BEST.mat.95 <- BESTmcmc(UD.Comp.5$Equal_Sat_95, UD.Comp.5$Equal_Ac_95, parallel=FALSE)
plot(BEST.mat.95)
# 87.6% chance that satellite are larger than acoustic 

# Equal Temporal Duration - 50% 
BEST.mat.50 <- BESTmcmc(UD.Comp.5$Equal_Ac_50, UD.Comp.5$Equal_Sat_50, parallel=FALSE)
plot(BEST.mat.50)
# 82.5% 

# Full Temporal Duration - 95% 
BEST.full.95 <- BESTmcmc(UD.Comp.5$Full_Ac_95, UD.Comp.5$Full_Sat_95, parallel=FALSE)
plot(BEST.full.95)
# 81.5% 

# Full Temporal Duration - 50% 
t.test(UD.Comp.5$Full_Ac_50, UD.Comp.5$Full_Sat_50, paired = T, var.equal = F)
BEST.full.50 <- BESTmcmc(UD.Comp.5$Full_Ac_50, UD.Comp.5$Full_Sat_50, parallel=FALSE)
plot(BEST.full.50)
# 67.6% 


## CALCULATING HOW MUCH LARGER SATELLITE RDs ARE THAN ACOUSTIC RDs ## 

# Equal Temporal Duration 95% 
UD.Comp.5$Equal_Inc_95 <- UD.Comp.5$Equal_Sat_95/UD.Comp.5$Equal_Ac_95
mean(UD.Comp.5$Equal_Inc_95, na.rm = T) # on average 130x larger than acoustic
range(UD.Comp.5$Equal_Inc_95) # 4 - 403 times larger

# Equal Temporal Duration 50% 
UD.Comp.5$Equal_Inc_50 <- UD.Comp.5$Equal_Sat_50/UD.Comp.5$Equal_Ac_50
mean(UD.Comp.5$Equal_Inc_50, na.rm = T) # on average 112x larger than acoustic
range(UD.Comp.5$Equal_Inc_50) # 2 - 356 times larger

# Full Temporal Duration 95% 
UD.Comp.5$Full_Inc_95 <- UD.Comp.5$Full_Sat_95/UD.Comp.5$Full_Ac_95
mean(UD.Comp.5$Full_Inc_95, na.rm = T) # on average 89x larger than acoustic 
range(UD.Comp.5$Full_Inc_95) # 1 - 305 

# Full Temporal Duration 50%
UD.Comp.5$Full_Inc_50 <- UD.Comp.5$Full_Sat_50/UD.Comp.5$Full_Ac_50
mean(UD.Comp.5$Full_Inc_50, na.rm = T) # on average 84x larger than acoustic
range(UD.Comp.5$Full_Inc_50) # 1 - 315


## ASSESSING EFFECT OF FULL TEMPORAL DURATION VS. MATCHING TEMPORAL DURATION ##

# Equal vs. Full Temporal Duration 95% UDs
boxplot(Comp.Full.Eq$Equal_95, Comp.Full.Eq$Full_95) # 
t.test(Comp.Full.Eq$Equal_95, Comp.Full.Eq$Full_95, paired = T, var.equal = F)

BEST.1 <- BESTmcmc(Comp.Full.Eq$Equal_95, Comp.Full.Eq$Full_95, parallel = F)
plot(BEST.1) # 61% chance that full 95 are larger than equal 95


# Equal vs. Full Temporal Duration 50% UDs
boxplot(Comp.Full.Eq$Equal_50, Comp.Full.Eq$Full_50) # 
t.test(Comp.Full.Eq$Equal_50, Comp.Full.Eq$Full_50, paired = T, var.equal = F)

BEST.2 <- BESTmcmc(Comp.Full.Eq$Equal_50, Comp.Full.Eq$Full_50, parallel = F)
plot(BEST.2) # 55.7% chance that full is larger than equal 


# If grouped by which tracking lasted longer:
# Acoustic 
Comp.Ac <- Comp.Full.Eq %>% filter(X == "A" | X == "C" | X == "D") # filter the turtles that were tracked longer with acoustic 

t.test(Comp.Ac$Equal_95, Comp.Ac$Full_95, paired = T, var.equal = F) # compare equal and full 95% UDs

BEST.3 <- BESTmcmc(Comp.Ac$Equal_95, Comp.Ac$Full_95, parallel = F)
plot(BEST.3) # 69.7% that full are larger than equal 

Comp.Ac$Inc_95 <- Comp.Ac$Full_95/Comp.Ac$Equal_95
mean(Comp.Ac$Inc_95) # Full acoustic were 20x larger than Matching acoustic UDs
range(Comp.Ac$Inc_95) # 1 - 59

t.test(Comp.Ac$Equal_50, Comp.Ac$Full_50, paired = T, var.equal = F) # compare equal and full 50% UDs

BEST.4 <- BESTmcmc(Comp.Ac$Equal_50, Comp.Ac$Full_50, parallel = F)
plot(BEST.4) # 68.9% 

Comp.Ac$Inc_50 <- Comp.Ac$Full_50/Comp.Ac$Equal_50
mean(Comp.Ac$Inc_50) # Full acoustic were 25x larger than Matching acoustic UDs
range(Comp.Ac$Inc_50) # 1 - 73


# Satellite 
Comp.Sat <- Comp.Full.Eq %>% filter(X == "E" | X == "G" | X == "H" | X == "I") # filter the turtles that were tracked longer with satellite 
t.test(Comp.Sat$Equal_95, Comp.Sat$Full_95, paired = T, var.equal = F) # compare equal and full 95% UDs

BEST.5 <- BESTmcmc(Comp.Sat$Equal_95, Comp.Sat$Full_95, parallel = F)
plot(BEST.5) # 52.5%

Comp.Sat$Inc_95 <- Comp.Sat$Equal_95/Comp.Sat$Full_95
mean(Comp.Sat$Inc_95) # Matching UD were 1x larger than Full satellite UDs
range(Comp.Sat$Inc_95) # 0 - 3

t.test(Comp.Sat$Equal_50, Comp.Sat$Full_50, paired = T, var.equal = F) # compare equal and full 50% UDs

BEST.6 <- BESTmcmc(Comp.Sat$Equal_50, Comp.Sat$Full_50, parallel = F)
plot(BEST.6) #47.8%

Comp.Sat$Inc_50 <- Comp.Sat$Equal_50/Comp.Sat$Full_50 
mean(Comp.Sat$Inc_50) # Equal UD were 1 x larger than Full satellite UDs
range(Comp.Sat$Inc_50) # 0 - 2

# Next Script: Overlap Indices for RDs



