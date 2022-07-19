# Acoustic-vs.-Satellite
Comparing passive acoustic and Argos satellite telemetry as methods of quantifying space use of marine species

This code is associated with Hardin, E.E. thesis entitled "Comparison of acoustic and satellite telemetry as methods for quantifying space use of marine species" submitted to Florida State University. 

This repository store the R scripts used to analyze passive acoustic and Argos satellite telemetry data of marine turtles, apply dynamic Brownian Bridge Movement Models to estimate utilization distributions, and generate figures. Raw data is not included given since green sea turtles are a federally protected species, but data can be available to researchers upon reasonable request. 

R scripts should be run in the following order:

  1. Acoustic Filtering.R
  2. Detection Reassignment.R 
  3. Calculate COAs.R
  4. Satellite Filtering.R
  5. Continuous Time SSM.R
  6. Individual dBBMMs.R
  7. Utilization Distributions.R
  8. UD Comparisons.R 
  9. Overlap Indices.R
  10. Array.Satellite Overlap.R
  11. Figures.R


doi: 10.5281/zenodo.6863943