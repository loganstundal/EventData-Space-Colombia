Script explanations
================
17 March, 2021

\#—————————————————————————–\# Author : Logan Stundal Date : February
25, 2021

Purpose : This document explains the order-of-operations in which to
execute scripts that replicate all analysis in the paper / appendix as
well as produce all figures and tables.

Additionally, data imports and data exports are noted for each script.
All imported and exported data files are stored in the Data/ directory.
\#—————————————————————————–\#

1.0: Spatial Admin - Purpose: Prepares a simple features spatial object
to hold all data for and analysis. As well, static geography variables
such as distance from Bogota are estimated in this script. - Imports:
Administrative-Units/col\_admbnda\_adm2\_unodc\_ocha.shp - Exports:
colombia\_admin.RData

# ———————————–

2.0: Data Tidy - Purpose: Organizes all variables used in models: event
data, independent and control variables. Construct panel, grouped-years,
and cross-section data structures - Imports: colombia\_admin.RData
event\_data/gedicews\_FARC\_20190908.csv
event\_data/CINEP\_HRV\_Farc.dta
covariates/Colombia\_ForestCover\_municipality\_Panel.csv
covariates/Colombia\_Population\_municipality\_Panel.csv
covariates/Colombia\_tri\_municipality\_mean.csv
covariates/Colombia\_NightLights\_municipality\_Panel.csv - Exports:
data\_variables.RData - colombia\_pn \[full panel\] - colombia\_yg
\[years, grouped\] - colombia\_cs \[cross section\]

# ———————————–

2.1: Spatial Tidy - Purpose: Collects and organize spatial data used in
models (spatial weights) or in figures (Colombia Admin 0 shapefile). -
Imports:
<https://biogeo.ucdavis.edu/data/gadm3.6/Rsf/gadm36_COL_0_sf.rds>, -
Colombia; Level-0 shapefile for mapping data\_variables.RData - Exports:
data\_spatial.Rdata

# ———————————–

3.0: EDA - Purpose: Perform exploratory analysis on data including
identifying municipalities with greatest FARC activity, estimating
Cohen’s Kappa, and confusion matricies as well as summary statistics. -
Imports: data\_variables.RData - Exports: Various figures to Plots/EDA/

# ———————————–

3.1: EDA - Maps - Purpose: Generates various plots of the distribution
of the DVs as well as their spatial distributions. - Imports:
data\_variables.RData - Exports: Various figures to Plots/EDA/

# ———————————–

4.0: Analysis - Discrete - Purpose: Estimate a series of discrete
spatial lag and spatial error probit models. - Imports:
data\_variables.RData data\_spatial.RData - Exports:
models\_discrete.RData - Notes : Move the spatial lags to the appendix
script later?

# ———————————–

4.1: Analysis - Continuous - Purpose: - Imports: data\_variables.RData
data\_spatial.RData - Exports:

# ———————————–

5.0: Plots - Model Outputs - Purpose: - Imports: - Exports:

# ———————————–

6.0: Appendix - Supplemental Appendix output - Purpose: - Imports: -
Exports:

# ———————————–

\#—————————————————————————–\#

x.x: xyz - Purpose: - Imports: - Exports:

# ———————————–
