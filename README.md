# MS296 - Heatwave distribution, trends, and association with urban characteristics in 371 Latin American cities

This repository contains the working code for MS296, a SALURBAL-CLIMATE study describing the spatial distribution and temporal trends of heatwaves, and their associations with urban characteristics across 371 Latin American cities from 2000 to 2024.  

## Script Overview  
| Script | Description |
| --- | --- |
|`00_prepare_dataset` | Reads the raw city-level files, selects the study variables and eligible cities, and exports the analytical dataset used in subsequent scripts.  |  
|`01_pop_exposed` |  Calculates the population exposed to heatwaves.  | 
|`02_modelling_trends` | Fits multilevel models to estimate overall temporal trends in annual heatwave metrics and exports the results. | 
|`03_modelling_time_invariant_variables` | Fits models for time-invariant variables (climate zone and latitude) and exports the results.  |
|`04_modelling_hybrid_time_varying` | Fits hybrid models for time-varying variables and exports the results.|
|`05_tables` | Generates the manuscript tables. | 
|`05a_sup_tables` |  Generates the supplementary tables. |
|`06_figures` | Plot all manuscrit figures |  
|`06a_sup_figures` | Plot all manuscrit supplementary figures | 
