#-----------------------------------------------------------------------------#
#
# Author:        Logan Stundal
# Date:          February 26, 2021
# Purpose:       2.0 Spatial-Tidy
#
#
# Copyright (c): Logan Stundal, 2021
# Email:         stund005@umn.edu
#
#-----------------------------------------------------------------------------#
#
# Notes:
#  Tidy spatial data used in models or plots (i.e., other shapefiles, spatial
#  weights)
#
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# ADMINISTRATIVE                                                          ----
#-----------------------------------------------------------------------------#

#---------------------------#
# Clear working environment
#---------------------------#
rm(list = ls())

#---------------------------#
# Load required packages
#---------------------------#
library(tidyverse)
library(sf)
library(spdep)

#---------------------------#
# Load data
#---------------------------#
colombia0 <- read_rds(url('https://biogeo.ucdavis.edu/data/gadm3.6/Rsf/gadm36_COL_0_sf.rds'))
load("data/data_variables.Rdata")



#-----------------------------------------------------------------------------#
# SPATIAL WEIGHTS                                                         ----
#-----------------------------------------------------------------------------#

# ----------------------------------- #
# Single cross-section
# ----------------------------------- #
colombia <- colombia %>%
  filter(year == 2000) %>%
  select(ID_Mun, Department, Municipality)
# ----------------------------------- #


# ----------------------------------- #
# Spatial weights
# ----------------------------------- #
# Create a spatial polygons dataframe to pass to 'poly2nb':
colombia.shp <- colombia[,c('ID_Mun','geometry')]
colombia.shp <- as_Spatial(from = colombia.shp,
                           IDs  = colombia.shp$ID_Mun)

# QUEEN - Spatial neighbors, matrix and lists:
nb.r.queen   <- poly2nb(pl        = colombia.shp,
                        row.names = colombia.shp$ID_Mun,
                        queen     = TRUE)
nb.queen.lst <- nb2listw(nb.r.queen)
nb.queen.mat <- nb2mat(neighbours = nb.r.queen,
                       style      = "W")
colnames(nb.queen.mat) <- rownames(nb.queen.mat)

# ROOK - Spatial neighbors, matrix and lists:
nb.r.rook  <- poly2nb(pl        = colombia.shp,
                      row.names = colombia.shp$ID_Mun,
                      queen     = FALSE)
nb.rook.lst <- nb2listw(nb.r.rook)
nb.rook.mat <- nb2mat(neighbours = nb.r.rook,
                        style      = "W")
colnames(nb.rook.mat) <- rownames(nb.rook.mat)

rm(colombia.shp, nb.r.queen, nb.r.rook)
# ----------------------------------- #



#-----------------------------------------------------------------------------#
# TIDY: LEVEL-0                                                           ----
#-----------------------------------------------------------------------------#
colombia0 <- st_transform(colombia0, crs = st_crs(colombia)) %>%
  st_crop(., st_bbox(colombia))



#-----------------------------------------------------------------------------#
# SAVE & CLEAN-UP                                                         ----
#-----------------------------------------------------------------------------#
rm(colombia)

save.image(file = "data/data_spatial.RData")
rm(list = ls())

