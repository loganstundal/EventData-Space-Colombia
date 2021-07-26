Read Me: Replication Directory Overview
================
26 July, 2021

------------------------------------------------------------------------

# README OVERVIEW

|         |                                                                                                                                                                                                                                                                                                                                      |
|---------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Author  | Logan Stundal                                                                                                                                                                                                                                                                                                                        |
| Date    | July 26, 2021                                                                                                                                                                                                                                                                                                                        |
| Purpose | This document explains the order-of-operations in which to execute scripts that replicate all analysis in the paper / appendix as well as produce all figures and tables. Additionally, data imports and data exports are noted for each script. All imported and exported data files are stored in the Replication/Data/ directory. |

------------------------------------------------------------------------

# Software

The primary analysis in this paper was conducted using R-INLA version
21.02.23 compiled on Feb 22, 2021. The tar.gz files for this version of
INLA are provided here:  
- <https://inla.r-inla-download.org/R/stable/src/contrib/>

-   <https://inla.r-inla-download.org/R/stable/src/contrib/INLA_21.02.23.tar.gz>

------------------------------------------------------------------------

# Scripts

| Script Name | **replication-models.R**                                                                                                                             |
|-------------|------------------------------------------------------------------------------------------------------------------------------------------------------|
| Purpose     | This script estimates all main models reported graphically in the main draft as well as in table format in the Appendix.                             |
| Imports     | data.Rdata                                                                                                                                           |
| Exports     | results-main.Rdata                                                                                                                                   |
| Notes       | Due to the large file size of INLA models the data exported here contain only vectors necessary to reproduce tables or figures in subsequent scripts |

## replication-tables.R

|         |     |
|---------|-----|
| Purpose |     |
| Imports |     |
| Exports |     |
| Notes   |     |
