Read Me: Replication Directory Overview
================
31 July, 2021

------------------------------------------------------------------------

# README OVERVIEW

|            |                                                                                                                                                                                                                                                                                                                                                                                                                                    |
|------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Author     | Logan Stundal                                                                                                                                                                                                                                                                                                                                                                                                                      |
| Date       | July 26, 2021                                                                                                                                                                                                                                                                                                                                                                                                                      |
| Paper      | Human Rights Violations in Space: Assessing the External Validity of Machine Geo-coded Vs. Human Geo-coded Data                                                                                                                                                                                                                                                                                                                    |
| Co-Authors | Bagozzi, Benjamin; Freeman, John; Holmes, Jennifer                                                                                                                                                                                                                                                                                                                                                                                 |
| Purpose    | This document explains script execution order to replicate all results in the paper / appendix as well as produce all figures and tables. Data imports and data exports are noted for each script.<br /><br />• Estimation results are exported to `Results/Replication-Estimates`<br />• Figures are exported to `Results/Replication-Figures`<br />• Published models were compressed and exported to `Results/Published-Models` |

These replication files are provided at:  
- [GitHub
repository](https://github.com/loganstundal/EventData-Space-Colombia/tree/main/Replication)  
- [Harvard Dataverse]()

------------------------------------------------------------------------

# Software

The primary analysis in this paper was conducted using R-INLA version
21.02.23 compiled on Feb 22, 2021. The tar.gz installation file for this
version of INLA is provided here:  
- <https://inla.r-inla-download.org/R/stable/src/contrib/>

-   <https://inla.r-inla-download.org/R/stable/src/contrib/INLA_21.02.23.tar.gz>

These models were estimated with R version 4.0.4 (2021-02-15) running on
Windows 10 x64 (build 19043).
\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_

# Data

All data for this project are stored in the `Data/` subdirectory which
contains three files:

-   **`/farc_events.Rdata`** contains all data necessary to replicate
    the analysis. This file contains an r-object called `dat` which is a
    named list comprising of 4 data frames: `2002-2004`, `2005-2007`,
    `2008-2009`, and `2002-2009` each corresponding to a temporal
    aggregation of the data implied by the name. These data frames each
    contain the following variables:

| Variable Name            | Description                                                             |
|:-------------------------|:------------------------------------------------------------------------|
| department               | Colombia Administrative level-1, Department name                        |
| municipality             | Colombia Administrative level-2, Municipality name                      |
| yr\_grp                  | Years for cross-section                                                 |
| cinep                    | FARC activity, count (CINEP source)                                     |
| cinep\_bin               | FARC activity, binary-indicator (CINEP source)                          |
| icews                    | FARC activity, count (ICEWS source)                                     |
| icews\_bin               | FARC activity, binary-indicator (ICEWS source)                          |
| icews\_cinep\_under      | FARC activity, ICEWS underreporting relative to CINEP, binary-indicator |
| ged                      | FARC activity, count (GED source)                                       |
| ged\_bin                 | FARC activity, binary-indicator (GED source)                            |
| ged\_cinep\_under        | FARC activity, GED underreporting relative to CINEP, binary-indicator   |
| area\_km2                | Municipality area                                                       |
| area\_km2\_ln            | Municipality area, logged                                               |
| distance\_bogota\_km\_ln | Distance (km) between municipality centroid and Bogota, logged          |
| distance\_bogota\_km     | Distance (km) between municipality centroid and Bogota                  |
| pop\_sum                 | Population count at municipality level                                  |
| pop\_sum\_ln             | Population count at municipality level, logged                          |
| terrain\_ri\_mean\_m     | Terrain roughness indicator, municipality average (meters)              |
| centroid\_mun\_lat       | Municipality centroid, latitude                                         |
| centroid\_mun\_long      | Municipality centroid, longitude                                        |

-   **`colombia.Rdata`** - an unprojected simple feature of Colombia’s
    international border used to provide a sharply contrasting boundary
    in maps containing posterior mean, probability, and standard
    deviation estimates of the Gaussian random field.
-   **`colombia2.Rdata`** - an unprojected simple feature object of
    level-2 administrative boundaries (Municipalities) used for mapping
    purposes.

------------------------------------------------------------------------

# Results

The R-scripts stored in the `Scripts/` directory generate outputs to
produce all figures or tables presented in the main article or appendix.
These exports are sored in one of three sub-directories in the
`Results/` folder:

-   **`Replication-Estimates/`** \~ contains Rdata files with model
    estimates used to reproduce all tables and figures
-   **`Replication-Figures/`** \~ contains png files for all figure
    exports
-   **`Published-Models/`** \~ contain compressed Rdata files with
    pre-estimated models from executing code in `2-models-spde.R` or
    `3-models-spem.R`

R-INLA models with random fields have large file sizes (\~130Mb \* 5
models \* 4 time cuts = \~2.5Gb). To reduce file sizes, after estimating
models in the primary `2-models-spde.R` script, quantities of interest
necessary for figures or tables are extracted or computed and exported
as vectors rather than complete model objects. The compressed full model
results are stored as an Rdata object in `Published-Models/`

# Scripts

**Note** - All scripts assume the top-level `Replication/` folder is set
as the working directory.

The following tables provide an overview of each script in the
`Scripts/` directory. Each table indicates what data sources are
imported - either raw data from `Data/` or estimated quantities from
`Replication-Estimates/`. Each table also indicates what component of
the paper the script exports (either a table or figure) as well as where
that element is stored within the Replication directory.

| Script Name | **1-descriptive.R**                                                                                                                                                          |
|:------------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Purpose     | Reproduces the descriptive data overview and comparison presented in paper Section 3.2 including Figures 1 & 2 and Tables 1 & 2. This script also produces Appendix Table 1. |
| Imports     | `Data/farc_events.Rdata`<br />`Data/colombia.Rdata`<br />`Data/colombia2.Rdata`                                                                                              |
| Exports     | `Replication-Figures/figure_main_1.png` - Figure 1<br />`Replication-Figures/figure_main_2.png` - Figure 2<br />                                                             |

| Script Name | **2-models-spde.R**                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  |
|:------------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Purpose     | Reproduces INLA models with SPDEs reported graphically in the main draft as well as in table format in the Appendix.                                                                                                                                                                                                                                                                                                                                                                                 |
| Imports     | `Data/farc_events.Rdata`                                                                                                                                                                                                                                                                                                                                                                                                                                                                             |
| Exports     | `Replication-Estimates/parameter-data.Rdata` - All model parameter and HPD estimates<br />`Replication-Estimates/field-data.Rdata` - Projected estimates of Gaussian field mean and SD<br />`Replication-Estimates/range-data.Rdata` - Spatial field decay and range estimates<br />`Replication-Estimates/pred-data.Rdata` - Model predicted outcomes on probability scale<br />`Published-Models/published-models-spde.Rdata` - Compressed INLA model estimates presented in the published article |

| Script Name | **3-models-spem.R**                                                                                                                                                                                                                                                                                 |
|:------------|:----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Purpose     | Reproduces discrete spatial probit error models (spem) presented in Appendix Tables A3 and A4 as well as Figure A3 (spem ROC comparison) and Figure A4 (spem predicted probabilities). This script and associated export also provides estimates for non-spatial probits presented in the Appendix. |
| Imports     | `Data/farc_events.Rdata`                                                                                                                                                                                                                                                                            |
| Exports     | `Published-Models/published-models-spem.Rdata`<br />`Replication-Figures/figure_appendix_4.png`                                                                                                                                                                                                     |

| Script Name | **4-figures\_coefplots.R**                                                                                                                                                |
|:------------|:--------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Purpose     | Reproduces Figures 3 and 4 from the main paper which present INLA model parameter estimates and credibility intervals for included regressors as well as GRMF parameters. |
| Imports     | `Replication-Estimates/parameter-data.Rdata`                                                                                                                              |
| Exports     | `Replication-Figures/figure_main_3.png` <br />`Replication-Figures/figure_main_4.png`                                                                                     |

| Script Name | **5-figures-ROCs.R**                                                                                                                                                                                                                        |
|:------------|:--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Purpose     | Produces figure 5 which presents ROCs and AUC estimates comparing ICEWS and GED model performance against CINEP ground truth observations.                                                                                                  |
| Imports     | `Data/farc_events.Rdata`<br />`Replication-Estimates/pred-data.Rdata`<br />`Published-Models/published-models-spem.Rdata`                                                                                                                   |
| Exports     | `Replication-Figures/figure_main_5.png`<br />`Replication-Figures/figure_appendix_3.png`<br />`Replication-Figures/figure_appendix_5.png`<br />`Replication-Figures/figure_appendix_6.png`<br />`Replication-Figures/figure_appendix_7.png` |

| Script Name | **6-tables.R**                                                                                                                                |
|:------------|:----------------------------------------------------------------------------------------------------------------------------------------------|
| Purpose     | Produces Appendix tables A5-A8 which present INLA GRMF model estimates in table format with median and 95% HPD estimates.                     |
| Imports     | `parameter-data.Rdata`<br />                                                                                                                  |
| Exports     | No export. Note: the `custom_table()` function in this script will return LaTex code when evaluated in an RMarkdown document compiled to pdf. |

| Script Name | **7-figures-field\_range-estimates.R**                                                                                                                      |
|:------------|:------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Purpose     | Produces figures related to posterior Gaussian Markov Random Field. Figure 8 - posterior field maps and Figure 9 - Posterior spatial error range and decay. |
| Imports     | `Replication-Estimates/field-data.Rdata`<br />`Replication-Estimates/range-data.Rdata`<br />`Data/colombia.Rdata`                                           |
| Exports     | `Replication-Figures/figure_appendix_8.png` - Figure 8<br />`Replication-Figures/figure_appendix_9.png` - Figure 9                                          |

<!-- rmarkdown::render("README.rmd", "pdf_document")
rmarkdown::render("README.rmd", "html_document") -->
