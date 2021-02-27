#-----------------------------------------------------------------------------#
#
# Author:        Logan Stundal
# Date:          February 25, 2021
# Purpose:       1.0 Spatial-Admin
#
#
# Copyright (c): Logan Stundal, 2021
# Email:         stund005@umn.edu
#
#-----------------------------------------------------------------------------#
#
# Notes:
#  This organizes the spatial simple features object used to organize both
#  data employed in models as well as spatial mapping data.
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
library(rmapshaper)

#---------------------------#
# Load data
#---------------------------#
colombia  <- read_sf(paste0("data/administrative-units/",
                            "col_admbnda_adm2_unodc_ocha.shp"))



#-----------------------------------------------------------------------------#
# TIDY                                                                    ----
#-----------------------------------------------------------------------------#


# ----------------------------------- #
# Simplify Geometry
# ----------------------------------- #
colombia <- ms_simplify(colombia, keep = 0.01)


# ----------------------------------- #
# Correct invalid geometry
# ----------------------------------- #
colombia <- st_make_valid(colombia) %>%
  st_cast(to   = "MULTIPOLYGON")


# ----------------------------------- #
# Project
# ----------------------------------- #
# Albers Equal Area projection for South America
prj <- paste0("+proj=aea  +lat_1=-5 +lat_2=-42 +lat_0=-32 +lon_0=-60 +x_0=0 ",
              "+y_0=0 +ellps=aust_SA +units=m +no_defs ")
colombia <- colombia %>%
  st_transform(., crs = st_crs(prj))



#-----------------------------------------------------------------------------#
# NEW VARIABLES
#-----------------------------------------------------------------------------#


# ----------------------------------- #
# Municipality centroids
# ----------------------------------- #
# This value will serve as both the input in the continuous models and will be used
# to estimate the distance_ variables
# https://r-spatial.github.io/sf/reference/geos_unary.html

centroids_proj <- suppressWarnings({st_centroid(colombia)})$geometry
centroids_lnlt <- st_transform(centroids_proj,
                               crs = "+proj=longlat +datum=WGS84") %>%
  st_coordinates(.) %>%
  as.data.frame()   %>%
  rename(centroid_mun_long = X,
         centroid_mun_lat  = Y)

colombia <- colombia %>%
  bind_cols(., centroids_lnlt)
# ----------------------------------- #


# ----------------------------------- #
# Bogota
# ----------------------------------- #
bogota <- st_sfc(st_point(x   = c(-74.08175, 4.60971),
                          dim = "XY")) %>%
  st_set_crs(.,
             value = "+proj=longlat +datum=WGS84") %>%
  st_transform(.,
               crs = st_crs(prj))

dist_bogota <- st_distance(x = centroids_proj,
                           y = bogota)
dist_bogota <- as.numeric(dist_bogota / 1e3)


# ----------------------------------- #
# Add variables to data frame
# ----------------------------------- #
colombia <- colombia %>%
  mutate(distance_bogota_km    = dist_bogota,
         distance_bogota_km_ln = log(dist_bogota),
         area_km2              = as.numeric(st_area(.) / 1e6)) %>%
  mutate(area_km2_ln           = log(area_km2))

rm(dist_bogota, bogota, centroids_proj, centroids_lnlt, prj)



#-----------------------------------------------------------------------------#
# VARIABLE SELECT                                                         ----
#-----------------------------------------------------------------------------#
colombia <- colombia %>%
  as.data.frame() %>%
  mutate(admin1Name   = iconv(admin1Name, from = 'UTF-8', to = 'ASCII//TRANSLIT'),
         admin2RefN   = iconv(admin2RefN, from = 'UTF-8', to = 'ASCII//TRANSLIT'),
         ID_Mun       = as.numeric(str_remove_all(admin2Pcod, 'CO'))) %>%
  rename(Department   = admin1Name,
         Municipality = admin2RefN) %>%
  dplyr::select(
    # Organization variables:
    ID_Mun, Department, Municipality,

    # Municipality areas:
    area_km2, area_km2_ln,

    # Bogota distances
    distance_bogota_km, distance_bogota_km_ln,

    # Coordinates:
    centroid_mun_long, centroid_mun_lat, geometry) %>%

  # Drop islands: 'San Andres Y Providencia'
  filter(ID_Mun != 88001)

# Reset geometry since centroids_mun_proj may default
colombia <- st_set_geometry(colombia, colombia$geometry)
#-----------------------------------------------------------------------------#


#-----------------------------------------------------------------------------#
# SAVE                                                                    ----
#-----------------------------------------------------------------------------#
save(colombia, file = 'data/colombia_admin.RData')
#-----------------------------------------------------------------------------#
rm(colombia)

