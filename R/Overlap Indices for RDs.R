#### OVERLAP INDICES FOR RDs ####
# Previous Script: RD Comparisons 

# Here we will calculate Bhattacharyya's Affinity overlap metrics to determine the similarity between each individuals' satellite and acoustic UDs 


## CREATE OVERLAP FUNCTION ## 

# Bhattacharyya's Affinity overlap index, adapted from Fieberg & Kochanny 2005
BA.index <- function(x,y){
  z<-sum(sqrt(x)*sqrt(y))
  z
}


## TURTLE A ##

# acoustic full temporal duration
Aa_ras <- raster(akde_Aa) # convert AKDE object to raster
Aa_ras_95 <- Aa_ras # make a raster object to later fit with 95% contour values
Aa_ras_50 <- Aa_ras # make a raster object to later fit with 50% contour values
Aa_vec <- raster::values(Aa_ras) # get values of each raster cell 
range(Aa_vec) #  7.214338e-05 9.996252e-01
sum(Aa_vec) # 8229567
Aa_vec_95 <- Aa_vec # make a vector for 95% contour
Aa_vec_95[Aa_vec_95 > 0.95] <- 0 # replace all cell values greater than 0.95 with 0 
values(Aa_ras_95) <- Aa_vec_95 # replace values of raster with new 95% vector so we can visualize the raster
raster::plot(Aa_ras_95) # visualize
# do the same with 50% contour
Aa_vec_50 <- Aa_vec
Aa_vec_50[Aa_vec_50 > 0.50] <- 0
values(Aa_ras_50) <- Aa_vec_50
raster::plot(Aa_ras_50)

# satellite full temporal duration 
As_ras <- raster(akde_As) 
As_ras_95 <- As_ras 
As_ras_50 <- As_ras
As_vec <- raster::values(As_ras) 
As_vec_95 <- As_vec 
As_vec_50 <- As_vec
As_vec_95[As_vec_95 > 0.95] <- 0 
As_vec_50[As_vec_50 > 0.50] <- 0
values(As_ras_95) <- As_vec_95 
raster::plot(As_ras_95)
values(As_ras_50) <- As_vec_50
raster::plot(As_ras_50)

# calculate full temporal duration overlaps 
# Naming convention: 'BA' overlap index for Turtle 'A's '95'% 'full' temporal UDs
BA.A.95.full <- print(BA.index(Aa_vec_95/sum(Aa_vec_95), As_vec_95/sum(As_vec_95))) # divide all cell values by the sum of the cells when doing the overlap calculation 
BA.A.50.full <- print(BA.index(Aa_vec_50/sum(Aa_vec_50), As_vec_50/sum(As_vec_50))) 


# acoustic matching temporal duration
Aas_ras <- raster(akde_Aas) 
Aas_ras_95 <- Aas_ras 
Aas_ras_50 <- Aas_ras
Aas_vec <- raster::values(Aas_ras) 
Aas_vec_95 <- Aas_vec 
Aas_vec_50 <- Aas_vec
Aas_vec_95[Aas_vec_95 > 0.95] <- 0 
Aas_vec_50[Aas_vec_50 > 0.50] <- 0

# satellite matching temporal duration 
Ass_ras <- raster(akde_Ass) 
Ass_ras_95 <- Ass_ras 
Ass_ras_50 <- Ass_ras
Ass_vec <- raster::values(Ass_ras) 
Ass_vec_95 <- Ass_vec 
Ass_vec_50 <- Ass_vec
Ass_vec_95[Ass_vec_95 > 0.95] <- 0 
Ass_vec_50[Ass_vec_50 > 0.50] <- 0

# calculate full temporal duration overlaps 
BA.A.95.mat <- print(BA.index(Aas_vec_95/sum(Aas_vec_95), Ass_vec_95/sum(Ass_vec_95))) # divide all cell values by the sum of the cells when doing the overlap calculation 
BA.A.50.mat <- print(BA.index(Aas_vec_50/sum(Aas_vec_50), Ass_vec_50/sum(Ass_vec_50))) 


## TURTLE C ##
# acoustic full temporal duration
Ca_ras <- raster(akde_Ca) 
Ca_vec <- raster::values(Ca_ras) 
Ca_vec_95 <- Ca_vec 
Ca_vec_50 <- Ca_vec
Ca_vec_95[Ca_vec_95 > 0.95] <- 0 
Ca_vec_50[Ca_vec_50 > 0.50] <- 0

# satellite full temporal duration 
Cs_ras <- raster(akde_Cs) 
Cs_vec <- raster::values(Cs_ras) 
Cs_vec_95 <- Cs_vec 
Cs_vec_50 <- Cs_vec
Cs_vec_95[Cs_vec_95 > 0.95] <- 0 
Cs_vec_50[Cs_vec_50 > 0.50] <- 0

BA.C.95.full <- print(BA.index(Ca_vec_95/sum(Ca_vec_95), Cs_vec_95/sum(Cs_vec_95))) 
BA.C.50.full <- print(BA.index(Ca_vec_50/sum(Ca_vec_50), Cs_vec_50/sum(Cs_vec_50))) 

# acoustic matching temporal duration
Cas_ras <- raster(akde_Cas) 
Cas_vec <- raster::values(Cas_ras) 
Cas_vec_95 <- Cas_vec 
Cas_vec_50 <- Cas_vec
Cas_vec_95[Cas_vec_95 > 0.95] <- 0 
Cas_vec_50[Cas_vec_50 > 0.50] <- 0

# satellite matching temporal duration 
Css_ras <- raster(akde_Css) 
Css_vec <- raster::values(Css_ras) 
Css_vec_95 <- Css_vec 
Css_vec_50 <- Css_vec
Css_vec_95[Css_vec_95 > 0.95] <- 0 
Css_vec_50[Css_vec_50 > 0.50] <- 0

BA.C.95.mat <- print(BA.index(Cas_vec_95/sum(Cas_vec_95), Css_vec_95/sum(Css_vec_95))) 
BA.C.50.mat <- print(BA.index(Cas_vec_50/sum(Cas_vec_50), Css_vec_50/sum(Css_vec_50))) 


## TURTLE D ##
# acoustic full temporal duration
Da_ras <- raster(akde_Da) 
Da_vec <- raster::values(Da_ras) 
Da_vec_95 <- Da_vec 
Da_vec_50 <- Da_vec
Da_vec_95[Da_vec_95 > 0.95] <- 0 
Da_vec_50[Da_vec_50 > 0.50] <- 0

# satellite full temporal duration 
Ds_ras <- raster(akde_Ds) 
Ds_vec <- raster::values(Ds_ras) 
Ds_vec_95 <- Ds_vec 
Ds_vec_50 <- Ds_vec
Ds_vec_95[Ds_vec_95 > 0.95] <- 0 
Ds_vec_50[Ds_vec_50 > 0.50] <- 0

BA.D.95.full <- print(BA.index(Da_vec_95/sum(Da_vec_95), Ds_vec_95/sum(Ds_vec_95))) 
BA.D.50.full <- print(BA.index(Da_vec_50/sum(Da_vec_50), Ds_vec_50/sum(Ds_vec_50))) 

# acoustic matching temporal duration
Das_ras <- raster(akde_Das) 
Das_vec <- raster::values(Das_ras) 
Das_vec_95 <- Das_vec 
Das_vec_50 <- Das_vec
Das_vec_95[Das_vec_95 > 0.95] <- 0 
Das_vec_50[Das_vec_50 > 0.50] <- 0

# satellite matching temporal duration 
Dss_ras <- raster(akde_Dss) 
Dss_vec <- raster::values(Dss_ras) 
Dss_vec_95 <- Dss_vec 
Dss_vec_50 <- Dss_vec
Dss_vec_95[Dss_vec_95 > 0.95] <- 0 
Dss_vec_50[Dss_vec_50 > 0.50] <- 0

BA.D.95.mat <- print(BA.index(Das_vec_95/sum(Das_vec_95), Dss_vec_95/sum(Dss_vec_95))) 
BA.D.50.mat <- print(BA.index(Das_vec_50/sum(Das_vec_50), Dss_vec_50/sum(Dss_vec_50))) 


## TURTLE G ##
# acoustic full temporal duration
Ga_ras <- raster(akde_Ga) 
Ga_vec <- raster::values(Ga_ras) 
Ga_vec_95 <- Ga_vec 
Ga_vec_50 <- Ga_vec
Ga_vec_95[Ga_vec_95 > 0.95] <- 0 
Ga_vec_50[Ga_vec_50 > 0.50] <- 0

# satellite full temporal duration 
Gs_ras <- raster(akde_Gs) 
Gs_vec <- raster::values(Gs_ras) 
Gs_vec_95 <- Gs_vec 
Gs_vec_50 <- Gs_vec
Gs_vec_95[Gs_vec_95 > 0.95] <- 0 
Gs_vec_50[Gs_vec_50 > 0.50] <- 0

BA.G.95.full <- print(BA.index(Ga_vec_95/sum(Ga_vec_95), Gs_vec_95/sum(Gs_vec_95))) 
BA.G.50.full <- print(BA.index(Ga_vec_50/sum(Ga_vec_50), Gs_vec_50/sum(Gs_vec_50))) 


# satellite matching temporal duration 
Gss_ras <- raster(akde_Gss) 
Gss_vec <- raster::values(Gss_ras) 
Gss_vec_95 <- Gss_vec 
Gss_vec_50 <- Gss_vec
Gss_vec_95[Gss_vec_95 > 0.95] <- 0 
Gss_vec_50[Gss_vec_50 > 0.50] <- 0

BA.G.95.mat <- print(BA.index(Ga_vec_95/sum(Ga_vec_95), Gss_vec_95/sum(Gss_vec_95))) 
BA.G.50.mat <- print(BA.index(Ga_vec_50/sum(Ga_vec_50), Gss_vec_50/sum(Gss_vec_50))) 


## TURTLE I ##
# acoustic full temporal duration
Ia_ras <- raster(akde_Ia) 
Ia_vec <- raster::values(Ia_ras) 
Ia_vec_95 <- Ia_vec 
Ia_vec_50 <- Ia_vec
Ia_vec_95[Ia_vec_95 > 0.95] <- 0 
Ia_vec_50[Ia_vec_50 > 0.50] <- 0

# satellite full temporal duration 
Is_ras <- raster(akde_Is) 
Is_vec <- raster::values(Is_ras) 
Is_vec_95 <- Is_vec 
Is_vec_50 <- Is_vec
Is_vec_95[Is_vec_95 > 0.95] <- 0 
Is_vec_50[Is_vec_50 > 0.50] <- 0

BA.I.95.full <- print(BA.index(Ia_vec_95/sum(Ia_vec_95), Is_vec_95/sum(Is_vec_95))) 
BA.I.50.full <- print(BA.index(Ia_vec_50/sum(Ia_vec_95), Is_vec_50/sum(Is_vec_95))) 


# satellite matching temporal duration 
Iss_ras <- raster(akde_Iss) 
Iss_vec <- raster::values(Iss_ras) 
Iss_vec_95 <- Iss_vec 
Iss_vec_50 <- Iss_vec
Iss_vec_95[Iss_vec_95 > 0.95] <- 0 
Iss_vec_50[Iss_vec_50 > 0.50] <- 0

BA.I.95.mat <- print(BA.index(Ia_vec_95/sum(Ia_vec_95), Iss_vec_95/sum(Iss_vec_95))) 
BA.I.50.mat <- print(BA.index(Ia_vec_50/sum(Ia_vec_95), Iss_vec_50/sum(Iss_vec_95))) 


## COMBINE DATA/COMPARISONS ##

# Matching Temporal Duration 95% overlap indices
mat.95 <- c(BA.A.95.mat, BA.C.95.mat, BA.D.95.mat, BA.G.95.mat, BA.I.95.mat)
mean(mat.95) 
sd(mat.95)/sqrt(5) 
range(mat.95)

# Matching Temporal Duration 50% overlap indices
mat.50 <- c(BA.A.50.mat, BA.C.50.mat, BA.D.50.mat, BA.G.50.mat, BA.I.50.mat)
mean(mat.50) 
sd(mat.50)/sqrt(5) 
range(mat.50) 

# Full Temporal Duration 95% overlap indices
full.95 <- c(BA.A.95.full, BA.C.95.full, BA.D.95.full, BA.G.95.full,  BA.I.95.full) 
mean(full.95) 
sd(full.95)/sqrt(5) 
range(full.95) 

# Full Temporal Duration 50% overlap indices
full.50 <- c(BA.A.50.full, BA.C.50.full, BA.D.50.full, BA.G.50.full,  BA.I.50.full)
mean(full.50) 
sd(full.50)/sqrt(5) 
range(full.50) 


ID <- c("A", "C", "D", "G", "I")
dat <- data.frame(ID, mat.95, full.95, mat.50, full.50)

t.test(dat$mat.95, dat$full.95, paired = T, var.equal = F)
t.test(dat$mat.50, dat$full.50, paired = T, var.equal = F)

# bayesian test to look at differences in full temporal duration overlap indices and matching temporal duration overlap indices 
BEST.overlaps <- BESTmcmc(dat$full.95, dat$mat.95, parallel=FALSE)
plot(BEST.overlaps) # 76.6% probability that full temporal duration overlap indices are greater than matching temporal duration overlap indices 


# Next Script: Array.Satellite RD Overlap