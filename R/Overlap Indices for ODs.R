#### Overlap Indices for ODs ####
# Previous Script: OD Comparisons 

# Here we will calculate Bhattacharyya's Affinity overlap metrics to determine the similarity between each individuals' satellite and acoustic ODs 
# Steps:
# 1. Load the dBBMM object 
# 2. Extract the UD 
# 3. Replace values > 0.95/0.50 with 0 
# 4. Get the values for each cell in the raster 
# 5. Apply overlap function 

library(move)

## CREATE OVERLAP FUNCTION ## 

# Bhattacharyya's Affinity overlap index, adapted from Fieberg & Kochanny 2005
BA.index <- function(x,y){
  z<-sum(sqrt(x)*sqrt(y))
  z
}


## CALCULATE OVERLAP FOR TURTLE A ## 

load(file="Aas") # dBBMM object (dBB.Aas) for Turtle A's matching temporal duration acoustic data
load(file="Ass") # dBBMM object (dBB.Ass) for Turtle A's matching temporal duration satellite data 
load(file= "Aa") # dBBMM object (dBB.Aa) for Turtle A's full temporal duration acoustic data 
load(file= "As") # dBBMM object (dBB.As) for Turtle A's full temporal duration satellite data 

# Matching 95% 
# Acoustic
UD.Aas <- getVolumeUD(dBB.Aas) # get UD
Aas.95 <- dBB.Aas # create new object 
Aas.95[UD.Aas > 0.95] <- 0 # replace raster values > 0.95 with 0
# Satellite 
UD.Ass <- getVolumeUD(dBB.Ass) # same process for satellite data
Ass.95 <- dBB.Ass
Ass.95[UD.Ass > 0.95] <- 0

AAvec.95 <- raster::values(Aas.95) # get raster values 
ASvec.95 <- raster::values(Ass.95) # get raster values

BA.A.95.mat <- print(BA.index(AAvec.95, ASvec.95)) # 0.4766894
# Naming convention: 'BA' overlap index for Turtle 'A's '95'% 'mat'ching UDs

# Matching 50% 
UD.Aas <- getVolumeUD(dBB.Aas)
Aas.50 <- dBB.Aas
Aas.50[UD.Aas > 0.50] <- 0
UD.Ass <- getVolumeUD(dBB.Ass)
Ass.50 <- dBB.Ass
Ass.50[UD.Ass > 0.50] <- 0
AAvec.50 <- raster::values(Aas.50)
ASvec.50 <- raster::values(Ass.50)
BA.A.50.mat <- print(BA.index(AAvec.50, ASvec.50)) # 0.128152

# Matching 95% acoustic vs. 50% satellite 
BA.A.mat.AS <- print(BA.index(ASvec.50, AAvec.95)) # 0.4486972

# Full 95% 
UD.Aa <- getVolumeUD(dBB.Aa)
Aa.95 <- dBB.Aa
Aa.95[UD.Aa > 0.95] <- 0
UD.As <- getVolumeUD(dBB.As)
As.95 <- dBB.As
As.95[UD.As > 0.95] <- 0
AAvec.95 <- raster::values(Aa.95)
ASvec.95 <- raster::values(As.95)
BA.A.95.full <- print(BA.index(AAvec.95, ASvec.95)) # 0.4733062

# Full 50% 
UD.Aa <- getVolumeUD(dBB.Aa)
Aa.50 <- dBB.Aa
Aa.50[UD.Aa > 0.50] <- 0
UD.As <- getVolumeUD(dBB.As)
As.50 <- dBB.As
As.50[UD.As > 0.50] <- 0
AAvec.50 <- raster::values(Aa.50)
ASvec.50 <- raster::values(As.50)
BA.A.50.full <- print(BA.index(AAvec.50, ASvec.50)) # 0.1270657

# Matching 95% acoustic vs. 50% satellite 
BA.A.full.AS <- print(BA.index(ASvec.50, AAvec.95)) # 0.4498753


## TURTLE C ##

load(file="Cas")
load(file="Css") 
load(file= "Ca") 
load(file= "Cs")

# Matching 95% 
UD.Cas <- getVolumeUD(dBB.Cas)
Cas.95 <- dBB.Cas
Cas.95[UD.Cas > 0.95] <- 0
UD.Css <- getVolumeUD(dBB.Css)
Css.95 <- dBB.Css
Css.95[UD.Css > 0.95] <- 0
CAvec.95 <- raster::values(Cas.95)
CSvec.95 <- raster::values(Css.95)
BA.C.95.mat <- print(BA.index(CAvec.95, CSvec.95)) # 0.5616326

# Matching 50%
UD.Cas <- getVolumeUD(dBB.Cas)
Cas.50 <- dBB.Cas
Cas.50[UD.Cas > 0.50] <- 0
UD.Css <- getVolumeUD(dBB.Css)
Css.50 <- dBB.Css
Css.50[UD.Css > 0.50] <- 0
CAvec.50 <- raster::values(Cas.50)
CSvec.50 <- raster::values(Css.50)
BA.C.50.mat <- print(BA.index(CAvec.50, CSvec.50)) # 0.04397541

# Matching 95% acoustic vs 50% satellite 
BA.C.mat.AS <- print(BA.index(CSvec.50, CAvec.95)) # 0.1480079

# Full 95% 
UD.Ca <- getVolumeUD(dBB.Ca)
Ca.95 <- dBB.Ca
Ca.95[UD.Ca > 0.95] <- 0
UD.Cs <- getVolumeUD(dBB.Cs)
Cs.95 <- dBB.Cs
Cs.95[UD.Cs > 0.95] <- 0
CAvec.95 <- raster::values(Ca.95)
CSvec.95 <- raster::values(Cs.95)
BA.C.95.full <- print(BA.index(CAvec.95, CSvec.95)) # 0.6408069

# Full 50%
UD.Ca <- getVolumeUD(dBB.Ca)
Ca.50 <- dBB.Ca
Ca.50[UD.Ca > 0.50] <- 0
UD.Cs <- getVolumeUD(dBB.Cs)
Cs.50 <- dBB.Cs
Cs.50[UD.Cs > 0.50] <- 0
CAvec.50 <- raster::values(Ca.50)
CSvec.50 <- raster::values(Cs.50)
BA.C.50.full <- print(BA.index(CAvec.50, CSvec.50)) # 0.09666418

# Matching 95% acoustic vs 50% satellite 
BA.C.full.AS <- print(BA.index(CSvec.50, CAvec.95)) # 0.1737375


## TURTLE D ##

load(file="Das") 
load(file="Dss") 
load(file= "Da") 
load(file= "Ds")


# Matching 95%
UD.Das <- getVolumeUD(dBB.Das)
Das.95 <- dBB.Das
Das.95[UD.Das > 0.95] <- 0
UD.Dss <- getVolumeUD(dBB.Dss)
Dss.95 <- dBB.Dss
Dss.95[UD.Dss > 0.95] <- 0
DAvec.95 <- raster::values(Das.95)
DSvec.95 <- raster::values(Dss.95)
BA.D.95.mat <- print(BA.index(DAvec.95, DSvec.95)) # 0.5300545

# Matching 50% 
UD.Das <- getVolumeUD(dBB.Das)
Das.50 <- dBB.Das
Das.50[UD.Das > 0.50] <- 0
UD.Dss <- getVolumeUD(dBB.Dss)
Dss.50 <- dBB.Dss
Dss.50[UD.Dss > 0.50] <- 0
DAvec.50 <- raster::values(Das.50)
DSvec.50 <- raster::values(Dss.50)
BA.D.50.mat <- print(BA.index(DAvec.50, DSvec.50)) # 0.1767899

# Matching 95% acoustic vs. 50% satellite 
BA.D.mat.AS <- print(BA.index(DSvec.50, DAvec.95)) # 0.503002

# Full 95%
UD.Da <- getVolumeUD(dBB.Da)
Da.95 <- dBB.Da
Da.95[UD.Da > 0.95] <- 0
UD.Ds <- getVolumeUD(dBB.Ds)
Ds.95 <- dBB.Ds
Ds.95[UD.Ds > 0.95] <- 0
DAvec.95 <- raster::values(Da.95)
DSvec.95 <- raster::values(Ds.95)
BA.D.95.full <- print(BA.index(DAvec.95, DSvec.95)) # 0.5378574

# Full 50% 
UD.Da <- getVolumeUD(dBB.Da)
Da.50 <- dBB.Da
Da.50[UD.Da > 0.50] <- 0
UD.Ds <- getVolumeUD(dBB.Ds)
Ds.50 <- dBB.Ds
Ds.50[UD.Ds > 0.50] <- 0
DAvec.50 <- raster::values(Da.50)
DSvec.50 <- raster::values(Ds.50)
BA.D.50.full <- print(BA.index(DAvec.50, DSvec.50)) # 0.175897

# Matching 95% acoustic vs. 50% satellite 
BA.D.full.AS <- print(BA.index(DSvec.50, DAvec.95)) # 0.5050441


## TURTLE G ##

load(file="Gas") 
load(file="Gss") 
load(file= "Ga") 
load(file= "Gs")

# Matching 95%
UD.Ga <- getVolumeUD(dBB.Ga)
Ga.95 <- dBB.Ga
Ga.95[UD.Ga > 0.95] <- 0
UD.Gss <- getVolumeUD(dBB.Gss)
Gss.95 <- dBB.Gss
Gss.95[UD.Gss > 0.95] <- 0
GAvec.95 <- raster::values(Ga.95)
GSvec.95 <- raster::values(Gss.95) 
BA.G.95.mat <- print(BA.index(GAvec.95, GSvec.95)) # 0.6690965

# Matching 50% 
UD.Ga <- getVolumeUD(dBB.Ga)
Ga.50 <- dBB.Ga
Ga.50[UD.Ga > 0.50] <- 0
UD.Gss <- getVolumeUD(dBB.Gss)
Gss.50 <- dBB.Gss
Gss.50[UD.Gss > 0.50] <- 0
GAvec.50 <- raster::values(Ga.50)
GSvec.50 <- raster::values(Gss.50)
BA.G.50.mat <- print(BA.index(GAvec.50, GSvec.50)) # 0.2698157

# Matching 95% acoustic vs. 50% satellite 
BA.G.mat.AS <- print(BA.index(GSvec.50, GAvec.95)) # 0.6235285

# Full 95%
UD.Ga <- getVolumeUD(dBB.Ga)
Ga.95 <- dBB.Ga
Ga.95[UD.Ga > 0.95] <- 0
UD.Gs <- getVolumeUD(dBB.Gs)
Gs.95 <- dBB.Gs
Gs.95[UD.Gs > 0.95] <- 0
GAvec.95 <- raster::values(Ga.95)
GSvec.95 <- raster::values(Gs.95) 
BA.G.95.full <- print(BA.index(GAvec.95, GSvec.95)) # 0.6935734

# Full 50% 
UD.Ga <- getVolumeUD(dBB.Ga)
Ga.50 <- dBB.Ga
Ga.50[UD.Ga > 0.50] <- 0
UD.Gs <- getVolumeUD(dBB.Gs)
Gs.50 <- dBB.Gs
Gs.50[UD.Gs > 0.50] <- 0
GAvec.50 <- raster::values(Ga.50)
GSvec.50 <- raster::values(Gs.50)
BA.G.50.full <- print(BA.index(GAvec.50, GSvec.50)) # 0.2846959

# Matching 95% acoustic vs. 50% satellite 
BA.G.full.AS <- print(BA.index(GSvec.50, GAvec.95)) # 0.6193397


## TURTLE I ##

load(file="Ias") 
load(file="Iss") 
load(file= "Ia") 
load(file= "Is")

# Matching 95%
UD.Ia <- getVolumeUD(dBB.Ia)
Ia.95 <- dBB.Ia
Ia.95[UD.Ia > 0.95] <- 0
UD.Iss <- getVolumeUD(dBB.Iss)
Iss.95 <- dBB.Iss
Iss.95[UD.Iss > 0.95] <- 0
IAvec.95 <- raster::values(Ia.95)
ISvec.95 <- raster::values(Iss.95)
BA.I.95.mat <- print(BA.index(IAvec.95, ISvec.95)) # 0.2027163

# Matching 50% 
UD.Ia <- getVolumeUD(dBB.Ia)
Ia.50 <- dBB.Ia
Ia.50[UD.Ia > 0.50] <- 0
UD.Iss <- getVolumeUD(dBB.Iss)
Iss.50 <- dBB.Iss
Iss.50[UD.Iss > 0.50] <- 0
IAvec.50 <- raster::values(Ia.50)
ISvec.50 <- raster::values(Iss.50)
BA.I.50.mat <- print(BA.index(IAvec.50, ISvec.50)) # 0.05813116

# Matching 95% acoustic vs. 50% satellite 
BA.I.mat.AS <- print(BA.index(ISvec.50, IAvec.95)) # 0.10121

# Full 95%
UD.Ia <- getVolumeUD(dBB.Ia)
Ia.95 <- dBB.Ia
Ia.95[UD.Ia > 0.95] <- 0
UD.Is <- getVolumeUD(dBB.Is)
Is.95 <- dBB.Is
Is.95[UD.Is > 0.95] <- 0
IAvec.95 <- raster::values(Ia.95)
ISvec.95 <- raster::values(Is.95)
BA.I.95.full <- print(BA.index(IAvec.95, ISvec.95)) # 0.3255976

# Full 50% 
UD.Ia <- getVolumeUD(dBB.Ia)
Ia.50 <- dBB.Ia
Ia.50[UD.Ia > 0.50] <- 0
UD.Is <- getVolumeUD(dBB.Is)
Is.50 <- dBB.Is
Is.50[UD.Is > 0.50] <- 0
IAvec.50 <- raster::values(Ia.50)
ISvec.50 <- raster::values(Is.50)
BA.I.50.full <- print(BA.index(IAvec.50, ISvec.50)) # 0.1375119



## COMBINE DATA/COMPARISONS ##

# Matching Temporal Duration 95% overlap indices
mat.95 <- c(BA.A.95.mat, BA.C.95.mat, BA.D.95.mat, BA.G.95.mat, BA.I.95.mat)
mean(mat.95) # 0.4880379
sd(mat.95)/sqrt(5) # 0.07794261
range(mat.95) # 0.2027163 0.6690965

# Matching Temporal Duration 50% overlap indices
mat.50 <- c(BA.A.50.mat, BA.C.50.mat, BA.D.50.mat, BA.G.50.mat, BA.I.50.mat)
mean(mat.50) # 0.1353728
sd(mat.50)/sqrt(5) # 0.04132926
range(mat.50) # 0.04397541 0.26981569

# Full Temporal Duration 95% overlap indices
full.95 <- c(BA.A.95.full, BA.C.95.full, BA.D.95.full, BA.G.95.full,  BA.I.95.full) 
mean(full.95) #  0.5342283
sd(full.95)/sqrt(5) # 0.06480791
range(full.95) # 0.3255976 0.6935734

# Full Temporal Duration 50% overlap indices
full.50 <- c(BA.A.50.full, BA.C.50.full, BA.D.50.full, BA.G.50.full,  BA.I.50.full)
mean(full.50) # 0.1643669
sd(full.50)/sqrt(5) # 0.03264065
range(full.50) # 0.09666418 0.28469589


ID <- c("A", "C", "D", "G", "I")
dat <- data.frame(ID, mat.95, full.95, mat.50, full.50)

t.test(dat$mat.95, dat$full.95, paired = T, var.equal = F)
# t = -1.9371, df = 4, p-value = 0.1248
t.test(dat$mat.50, dat$full.50, paired = T, var.equal = F)
# t = -1.8161, df = 4, p-value = 0.1435

# bayesian test to look at differences in full temporal duration overlap indices and matching temporal duration overlap indices 
BEST.overlaps <- BESTmcmc(dat$full.95, dat$mat.95, parallel=FALSE)
plot(BEST.overlaps) # 61.8% probability that full temporal duration overlap indices are greater than matching temporal duration overlap indices 
BEST.overlaps.50 <- BESTmcmc(dat$full.50, dat$mat.50, parallel=FALSE)
plot(BEST.overlaps.50) # 64.4%


# Make dataframe of all overlap indices (both from regular study and supplemental study)
dat <- read.csv("overlap2.csv", header = T)
# t = -2.1912, df = 4, p-value = 0.09357


# Difference between supplemental study overlap indices and regular study indices 

# Matching 95% 
t.test(dat$S_E_95, dat$E_95, paired = T, var.equal = F)
# t = 2.3548, df = 4, p-value = 0.07811

# Matching 50% 
t.test(dat$S_E_50, dat$E_50, paired = T, var.equal = F)
# t = 2.2952, df = 4, p-value = 0.08338

# Full 95% 
t.test(dat$S_F_95, dat$F_95, paired = T, var.equal = F)
# t = 2.5292, df = 4, p-value = 0.06472

# Full 50% 
t.test(dat$S_F_50, dat$F_50, paired = T, var.equal = F)
# t = 1.609, df = 4, p-value = 0.1829
