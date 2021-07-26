Read Me: Replication Directory Overview
================
26 July, 2021

------------------------------------------------------------------------

# README OVERVIEW

|         |                                                                                                                                                                                                                                                                                                                                          |
|---------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Author  | Logan Stundal                                                                                                                                                                                                                                                                                                                            |
| Date    | July 26, 2021                                                                                                                                                                                                                                                                                                                            |
| Purpose | This document explains the order-of-operations in which to execute scripts that replicate all analysis in the paper / appendix as well as produce all figures and tables. Additionally, data imports and data exports are noted for each script. All imported and exported data files are stored in the **Replication/Data/** directory. |

These replication files are also located on the [Harvard
Dataverse](here.com).
\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_

# Software

The primary analysis in this paper was conducted using R-INLA version
21.02.23 compiled on Feb 22, 2021. The tar.gz files for this version of
INLA are provided here:  
- <https://inla.r-inla-download.org/R/stable/src/contrib/>

-   <https://inla.r-inla-download.org/R/stable/src/contrib/INLA_21.02.23.tar.gz>

------------------------------------------------------------------------

# Data

The *Replication/Data/data.Rdata* file contains all data necessary to
replicate the analysis. This file contains an r-object called `dat`
which is a named list comprising of 4 data frames: `2002-2004`,
`2005-2007`, `2008-2009`, and `2002-2009` each corresponding to a
temporal aggregation of the data implied by the name. These data frames
each contain the following variables:

| Variable Name            | Description                                                             |
|:-------------------------|:------------------------------------------------------------------------|
| Department               | Colombia Administrative level-1, Department name                        |
| Municipality             | Colombia Administrative level-2, Municipality name                      |
| yr\_grp                  | Years for cross-section                                                 |
| cinep                    | FARC activity, count (CINEP source)                                     |
| cinep\_bin               | FARC activity, binary-indicator (CINEP source)                          |
| icews                    | FARC activity, count (ICEWS source)                                     |
| icews\_bin               | FARC activity, binary-indicator (ICEWS source)                          |
| icews\_cinep\_under      | FARC activity, ICEWS underreporting relative to CINEP                   |
| ged                      | FARC activity, count (GED source)                                       |
| ged\_bin                 | FARC activity, binary-indicator (GED source)                            |
| ged\_cinep\_under        | FARC activity, GED underreporting relative to CINEP                     |
| distance\_bogota\_km\_ln | Distance between municipality centroid and Bogota in kilometers, logged |
| pop\_sum\_ln             | Population at municipality level, logged                                |
| terrain\_ri\_mean\_m     | Terrain roughness indicator, municipality average (meters)              |
| centroid\_mun\_lat       | Municipality centroid, latitude                                         |
| centroid\_mun\_long      | Municipality centroid, longitude                                        |

------------------------------------------------------------------------

# Scripts

| Script Name | **replication-models.R**                                                                                                                             |
|:------------|:-----------------------------------------------------------------------------------------------------------------------------------------------------|
| Purpose     | This script estimates all main models reported graphically in the main draft as well as in table format in the Appendix.                             |
| Imports     | data.Rdata                                                                                                                                           |
| Exports     | results\_main\_models.Rdata                                                                                                                          |
| Notes       | Due to the large file size of INLA models the data exported here contain only vectors necessary to reproduce tables or figures in subsequent scripts |

| Script Name | **replication-tables.R** |
|:------------|:-------------------------|
| Purpose     |                          |
| Imports     |                          |
| Exports     |                          |
| Notes       |                          |

| Script Name | **replication-figures.R** |
|:------------|:--------------------------|
| Purpose     |                           |
| Imports     |                           |
| Exports     |                           |
| Notes       |                           |

| Script Name | **replication-appendix.R** |
|:------------|:---------------------------|
| Purpose     |                            |
| Imports     |                            |
| Exports     |                            |
| Notes       |                            |
