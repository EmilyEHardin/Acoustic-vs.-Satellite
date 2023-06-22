# Acoustic-vs.-Satellite
Comparing acoustic and satellite telemetry: an analysis quantifying the space use of Chelonia mydas in Bimini, Bahamas
Hardin, E.E., Cullen, J.A., Fuentes, M.M.P.B.

This code is associated with the manuscript above, based off the thesis work of Hardin, E.E., entitled "Comparison of acoustic and satellite telemetry as methods for quantifying space use of marine species" submitted to Florida State University. 

This repository store the R scripts used to analyze passive acoustic and Argos satellite telemetry data of marine turtles, apply dynamic Brownian Bridge Movement Models and autocorrelated kernel density estimators to estimate utilization distributions, and generate figures. Raw data is not included because green sea turtles are a federally protected species, but data can be available to researchers upon reasonable request. 

R scripts should be run in the following order:

  1. Acoustic Filtering.R
  2. Detection Reassignment.R 
  3. Calculate COAs.R
  4. Satellite Filtering.R
  5. Continuous Time SSM_ODs.R
  6. Individual dBBMMs_ODs.R
  7. Occurrence Distributions.R
  8. OD Comparisons.R 
  9. Overlap Indices for ODs.R
  10. Array.Satellite OD Overlap.R
  11. Supplementary Study_ODs.R
  12. Individual AKDEs_RDs.R
  13. Range Distributions.R
  14. RD Comparisons.R
  15. Overlap Indices for RDs.R
  16. Array.Satellite RD Overlap.R
  11. Figures.R

doi: 10.5281/zenodo.6863943