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
library()

#---------------------------#
# Set working directory
#---------------------------#
setwd()

#---------------------------#
# Load data
#---------------------------#



#-----------------------------------------------------------------------------#
# TIDY DATA                                                               ----
#-----------------------------------------------------------------------------#


#-----------------------------------------------------------------------------#
# ANALYSIS                                                                ----
#-----------------------------------------------------------------------------#


#-----------------------------------------------------------------------------#
# SAVE                                                                    ----
#-----------------------------------------------------------------------------#
#save.image()
#rm(list = ls())












# _____________________ ----
# COLOMBIA LEVEL 0 ------------------------------------------------------------
# colombia0 <- read_rds(url('https://biogeo.ucdavis.edu/data/gadm3.6/Rsf/gadm36_COL_0_sf.rds'))
# colombia0 <- st_transform(colombia0, crs = st_crs(colombia))
#
# colombia0 <- st_crop(x = colombia0,
#                      y = st_bbox(colombia))
#
# # SPATIAL WEIGHTS -------------------------------------------------------------
# library(spdep)
#
# # Create a spatial polygons dataframe to pass to 'poly2nb':
# colombia.shp <- colombia[,c('ID_Mun','geometry')]
# colombia.shp$centroid_mun_proj <- NULL
# colombia.shp <- st_set_geometry(colombia.shp, colombia.shp$geometry)
# colombia.shp <- as_Spatial(from = colombia.shp,
#                            IDs  = colombia.shp$ID_Mun)
#
# # QUEEN - Spatial neighbors, matrix and lists:
# nb.r.queen     <- poly2nb(pl        = colombia.shp,
#                           row.names = colombia.shp$ID_Mun,
#                           queen     = TRUE)
# nb.lst.queen   <- nb2listw(nb.r.queen)
# W_matrix.queen <- nb2mat(neighbours = nb.r.queen,
#                          style      = "W")
# colnames(W_matrix.queen) <- rownames(W_matrix.queen)
#
# # ROOK - Spatial neighbors, matrix and lists:
# nb.r.rook     <- poly2nb(pl        = colombia.shp,
#                          row.names = colombia.shp$ID_Mun,
#                          queen     = FALSE)
# nb.lst.rook   <- nb2listw(nb.r.rook)
# W_matrix.rook <- nb2mat(neighbours = nb.r.rook,
#                         style      = "W")
# colnames(W_matrix.rook) <- rownames(W_matrix.rook)
#
# rm(colombia.shp)
#
# # _____________________ ----
# # SAVE ------------------------------------------------------------------------
#
#
# # CSV file for sharing with Ben and John
# write_csv(x    = colombia,
#           path = 'data/colombia.csv')
#
# # Rdata file for future modeling
# save.image(file = 'data/colombia.Rdata')
# rm(list=ls())
#
#















