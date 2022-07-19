#### UD COMPARISONS ####
# Previous Script: Utilization Distributions 

# The areas of all UDs were compiled into a table (UD.Comp.csv)and converted into km2. Here we will run comparisons and Welch's t-tests to compares the differences in size
# The areas of the UDs estimated from the matching and full temporal durations were also compiled into a table () and will be compared here

library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggbreak)

UD.Comp <- read.csv("UD.Comp.csv", header = T)
Comp.Full.Eq <- read.csv("Comp.Full.Eq.csv", header = T)


## COMPARISONS OF ALL UDs ## 

# Acoustic Equal Temporal Scale 
mean(UD.Comp$Equal_Ac_95, na.rm = T) # 2.984581 (A, C, D, G, I)
  sd(UD.Comp$Equal_Ac_95, na.rm = T) # 1.606479
mean(UD.Comp$Equal_Ac_50, na.rm = T) # 0.4277687 (A, C, D, G, I)
  sd(UD.Comp$Equal_Ac_50, na.rm = T) # 0.0902846
# Satellite Equal Temporal Scale 
mean(UD.Comp$Equal_Sat_95, na.rm = T) # 195.6508 (A, C, D, E, G, H, I)
  sd(UD.Comp$Equal_Sat_95, na.rm = T) # 385.622
mean(UD.Comp$Equal_Sat_50, na.rm = T) # 10.21823 (A, C, D, E, G, H, I)
  sd(UD.Comp$Equal_Sat_50, na.rm = T) # 14.49039

boxplot(UD.Comp$Equal_Ac_95, UD.Comp$Equal_Ac_50, UD.Comp$Equal_Sat_95, UD.Comp$Equal_Sat_50, main = "Matching Temporal Duration UDs", ylab = "km^2", ylim = c(0, 140), names = c("Acoustic 95%", "Acoustic 50%","Satellite 95%", "Satellite 50%"))

# Acoustic Full Temporal Scale 
mean(UD.Comp$Full_Ac_95, na.rm = T) # 2.797363 (A, C, D, F, G, I)
  sd(UD.Comp$Full_Ac_95, na.rm = T) # 1.507683
mean(UD.Comp$Full_Ac_50, na.rm = T) # 0.4254983 (A, C, D, F, G, I)
  sd(UD.Comp$Full_Ac_50, na.rm = T) # 0.08090547
# Satellite Full Temporal Scale 
mean(UD.Comp$Full_Sat_95, na.rm = T) # 161.4345 (A, B, C, D, E, G, H, I)
  sd(UD.Comp$Full_Sat_95, na.rm = T) # 335.1602
mean(UD.Comp$Full_Sat_50, na.rm = T) # 6.526008 (A, B, C, D, E, G, H, I)
  sd(UD.Comp$Full_Sat_50, na.rm = T) # 8.576171

boxplot(UD.Comp$Full_Ac_95, UD.Comp$Full_Ac_50, UD.Comp$Full_Sat_95, UD.Comp$Full_Sat_50, main = "Full Temporal Duration UDs", ylab = "km^2", ylim = c(0, 140), names = c("Acoustic 95%", "Acoustic 50%","Satellite 95%", "Satellite 50%"))


## COMPARISONS OF UDs OF 5 TURTLES SUCCESSFULLY TRACKED WITH BOTH TELEMETRY METHODS ## 

# Only Turtles A, C, D, G, and I have both Acoustic and Satellite UDs and therefore only these individuals can be used in comparisons of tracking types 
# Create a new dataframe containing only these individuals 
UD.Comp.5 <- UD.Comp %>%
  filter(X == "A" | X == "C" | X == "D" | X == "G" | X == "I")

# Acoustic Equal Temporal Scale 
mean(UD.Comp.5$Equal_Ac_95)
  sd(UD.Comp.5$Equal_Ac_95)
mean(UD.Comp.5$Equal_Ac_50)
  sd(UD.Comp.5$Equal_Ac_50)
# Satellite Equal Temporal Scale 
mean(UD.Comp.5$Equal_Sat_95)
  sd(UD.Comp.5$Equal_Sat_95)
mean(UD.Comp.5$Equal_Sat_50)
  sd(UD.Comp.5$Equal_Sat_50)

# Acoustic Full Temporal Scale 
mean(UD.Comp.5$Full_Ac_95)
  range(UD.Comp.5$Full_Ac_95)
mean(UD.Comp.5$Full_Ac_50)
  range(UD.Comp.5$Full_Ac_50)
# Satellite Full Temporal Scale 
mean(UD.Comp.5$Full_Sat_95)
  range(UD.Comp.5$Full_Sat_95)
mean(UD.Comp.5$Full_Sat_50)
  range(UD.Comp.5$Full_Sat_50)
  

## BOXPLOTS ## 

# Matching Temporal Duration
UD <- read.csv("UD.Comp_Equal.csv", header = T) # matching temporal duration UDs for 5 comparative turtles 
yl <- expression(paste("UD Size (",km^2,")",sep=""))

M.UD.box <- ggplot(UD, aes(x = Method, y = UD, fill = Method)) + 
  geom_boxplot(outlier.colour = "black", outlier.shape = 21, outlier.size = 2, show.legend = T) +
  ylab(yl) +
  xlab("") +
  scale_fill_manual(values = c("gray34", "grey80")) +
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
ggsave(filename = "M.UD.box.tiff", plot = M.UD.box, device = "tiff", path = "/Users/EmilyHardin/Desktop/Research/Acous.vs.Sat", width = 240, height = 240, units = c("mm"), dpi = 600)

# Equal Temporal Duration
UD.Full <- read.csv("UD.Comp_Full.csv", header = T)
F.UD.box <- ggplot(UD.Full, aes(x = Method, y = UD, fill = Method)) + 
  geom_boxplot(outlier.colour = "black", outlier.shape = 21, outlier.size = 2, show.legend = T) +
  ylab(yl) +
  xlab("") +
  scale_fill_manual(values = c("gray34", "grey80")) +
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
ggsave(filename = "F.UD.box.tiff", plot = F.UD.box, device = "tiff", path = "/Users/EmilyHardin/Desktop/Research/Acous.vs.Sat", width = 240, height = 240, units = c("mm"), dpi = 600)


## T-TESTS ##

# Equal Temporal Duration - 95% 
boxplot(UD.Comp.5$Equal_Ac_95, UD.Comp.5$Equal_Sat_95)
t.test(UD.Comp.5$Equal_Ac_95, UD.Comp.5$Equal_Sat_95, paired = T, var.equal = F)
  # t = -1.9169
  # df = 4
  # p-value = 0.1277 
  # 95% CI = -127.0 - 23.3
  # NOT significant 

# Equal Temporal Duration - 50% 
boxplot(UD.Comp.5$Equal_Ac_50, UD.Comp.5$Equal_Sat_50) 
t.test(UD.Comp.5$Equal_Ac_50, UD.Comp.5$Equal_Sat_50, paired = T, var.equal = F)
  # t = -1.9818
  # df = 4
  # p-value = 0.1185
  # 95% CI = 
  # NOT significant 

# Full Temporal Duration - 95% 
boxplot(UD.Comp.5$Full_Ac_95, UD.Comp.5$Full_Sat_95)
t.test(UD.Comp.5$Full_Ac_95, UD.Comp.5$Full_Sat_95, paired = T, var.equal = F)
  # t = -1.9329
  # df = 4
  # p-value = 0.1254 
  # 95% CI = 
  # NOT significant 

# Full Temporal Duration - 50% 
boxplot(UD.Comp.5$Full_Ac_50, UD.Comp.5$Full_Sat_50) 
t.test(UD.Comp.5$Full_Ac_50, UD.Comp.5$Full_Sat_50, paired = T, var.equal = F)
  # t = -1.7047
  # df = 4
  # p-value = 0.1635
  # 95% CI = 
  # NOT significant 


## CALCULATING HOW MUCH LARGER SATELLITE UDs ARE THAN ACOUSTIC UDs ## 

# Equal Temporal Duration 95% 
UD.Comp$Equal_Inc_95 <- UD.Comp$Equal_Sat_95/UD.Comp$Equal_Ac_95
mean(UD.Comp$Equal_Inc_95, na.rm = T) # on average 15 x larger than acoustic 

# Equal Temporal Duration 50% 
UD.Comp$Equal_Inc_50 <- UD.Comp$Equal_Sat_50/UD.Comp$Equal_Ac_50
mean(UD.Comp$Equal_Inc_50, na.rm = T) # on average 12 x larger than acoustic

# Full Temporal Duration 95% 
UD.Comp$Full_Inc_95 <- UD.Comp$Full_Sat_95/UD.Comp$Full_Ac_95
mean(UD.Comp$Full_Inc_95, na.rm = T) # on average 13 x larger than acoustic 

# Full Temporal Duration 50%
UD.Comp$Full_Inc_50 <- UD.Comp$Full_Sat_50/UD.Comp$Full_Ac_50
mean(UD.Comp$Full_Inc_50, na.rm = T) # on average 7 x larger than acoustic


## ASSESSING EFFECT OF FULL TEMPORAL DURATION VS. EQUAL TEMPORAL DURATION ##

# Equal vs. Full Temporal Duration 95% UDs
boxplot(Comp.Full.Eq$Equal_95, Comp.Full.Eq$Full_95) # E is a potential outlier
t.test(Comp.Full.Eq$Equal_95, Comp.Full.Eq$Full_95, paired = T, var.equal = F)
  # t= 1.0233
  # df = 6
  # p-value = 0.3456
  # NOT significant
  # 95% CI = 

# Equal vs. Full Temporal Duration 50% UDs
boxplot(Comp.Full.Eq$Equal_50, Comp.Full.Eq$Full_50) # E is a potential outlier
t.test(Comp.Full.Eq$Equal_50, Comp.Full.Eq$Full_50, paired = T, var.equal = F)
  # t= 1.0468
  # df = 6
  # p-value = 0.3355
  # 95% CI = 
  # NOT significant 

# If grouped by which tracking lasted longer:
# Acoustic 
Comp.Ac <- Comp.Full.Eq %>% filter(X == "A" | X == "C" | X == "D") # filter the turtles that were tracked longer with acoustic 

t.test(Comp.Ac$Equal_95, Comp.Ac$Full_95, paired = T, var.equal = F) # compare equal and full 95% UDs
  # NOT significant; t = 1, df = 2, p-value = 0.4226
Comp.Ac$Inc_95 <- Comp.Ac$Full_95/Comp.Ac$Equal_95
  mean(Comp.Ac$Inc_95) # Full acoustic were  0.99 x larger than Matching acoustic UDs

t.test(Comp.Ac$Equal_50, Comp.Ac$Full_50, paired = T, var.equal = F) # compare equal and full 50% UDs
  # NOT significant; t = 1, df = 2, p-value = 0.4226 
Comp.Ac$Inc_50 <- Comp.Ac$Full_50/Comp.Ac$Equal_50
  mean(Comp.Ac$Inc_50) # Full acoustic were 0.99 x larger than Matching acoustic UDs

# Satellite 
Comp.Sat <- Comp.Full.Eq %>% filter(X == "E" | X == "G" | X == "H" | X == "I") # filter the turtles that were tracked longer with satellite 
t.test(Comp.Sat$Equal_95, Comp.Sat$Full_95, paired = T, var.equal = F) # compare equal and full 95% UDs
  # NOT significant; t = 1.0267, df = 3, p-value = 0.3801
Comp.Sat$Inc_95 <- Comp.Sat$Equal_95/Comp.Sat$Full_95
  mean(Comp.Sat$Inc_95) # Matching UD were 0.97 x larger than Full satellite UDs

t.test(Comp.Sat$Equal_50, Comp.Sat$Full_50, paired = T, var.equal = F) # compare equal and full 50% UDs
  # NOT significant; t = 1.054, df = 3, p-value = 0.3693
Comp.Sat$Inc_50 <- Comp.Sat$Equal_50/Comp.Sat$Full_50 
mean(Comp.Sat$Inc_50) # Equal UD were 1.7 x larger than Full satellite UDs


# Next Script: Overlap Indices






