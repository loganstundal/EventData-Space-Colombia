#-----------------------------------------------------------------------------#
#
# Author:        Logan Stundal
# Date:          January 18, 2020
# Purpose:       Create a Colombia administrative boundaries simple feature
#
#
# Copyright (c): Logan Stundal, 2020
# Email:         stund005@umn.edu
#
#-----------------------------------------------------------------------------#
#
# Notes:
#       This scritp estimates builds the basic Colombia spatial data, including:
#         - A more computationally efficient sf spatial object
#         - ID of municipalities sharing an international border
#         - Municipality area
#         - Municipality centroids (long, lat)
#
#         - Good refresher information on proj4string syntax here:
#           https://mgimond.github.io/Spatial/coordinate-systems-in-r.html
#    
#-----------------------------------------------------------------------------#



# ADMINISTRATIVE --------------------------------------------------------------

#---------------------------#
# Clear working directory
#---------------------------#
rm(list = ls())


# Load required libraries
#---------------------------#
library(tidyverse)
library(sf)
library(raster)
library(rmapshaper)

#---------------------------#
# Set working directory
#---------------------------#
setwd(paste('C:/Users/logan/GoogleDrive/UMN/RESEARCH/RA_JOHN/Event_Data_Project'))

#---------------------------#
# Load data
#---------------------------#
# Albers Equal Area projection for South America
prj <- "+proj=aea  +lat_1=-5 +lat_2=-42 +lat_0=-32 +lon_0=-60 +x_0=0 +y_0=0 +ellps=aust_SA +units=m +no_defs "

colombia  <- shapefile('data/administrative_units/col_admbnda_adm2_unodc_ocha/col_admbnda_adm2_unodc_ocha.shp')
ecuador   <- read_rds(url('https://biogeo.ucdavis.edu/data/gadm3.6/Rsf/gadm36_ECU_0_sf.rds'))
peru      <- read_rds(url('https://biogeo.ucdavis.edu/data/gadm3.6/Rsf/gadm36_PER_0_sf.rds'))
brazil    <- read_rds(url('https://biogeo.ucdavis.edu/data/gadm3.6/Rsf/gadm36_BRA_0_sf.rds'))
venezuela <- read_rds(url('https://biogeo.ucdavis.edu/data/gadm3.6/Rsf/gadm36_VEN_0_sf.rds'))
panama    <- read_rds(url('https://biogeo.ucdavis.edu/data/gadm3.6/Rsf/gadm36_PAN_0_sf.rds'))

s_america <- rbind(ecuador, peru, brazil, venezuela, panama)
s_america_names <- c('ecuador','peru','brazil','venezuela','panama')
rm(ecuador, peru, brazil, venezuela, panama)

# Load in gnames data:
gnames_cnames <- c('geoname_id','name','ascii_name','alternate_name','latitude','longitude',
                  'feature_class','feature_code','c_code','cc2','admin1_code','admin2_code',
                  'admin3_code','admin4_code','population','elevation','dem','time_zone',
                  'mod_date')
gnames_files <- c('CO','BR','EC','PA','PE','VE')

for(x in 1:6){
  tmp = read_tsv(file      = sprintf('data/covariate_data/geonames_co/%s.txt',gnames_files[x]),
                 col_names = gnames_cnames,
                 col_types = paste(c('d','c','c','c','d','d','c','c','c','c',
                                     'c','c','c','c','d','d','d','c','D'), collapse = ''))
  
  assign(x = paste0('gnames_',gnames_files[x]),
         value = tmp)
};rm(x, gnames_cnames, gnames_files, tmp)

gnames_SA <- bind_rows(gnames_CO, gnames_BR, gnames_EC, 
                       gnames_PA, gnames_PE, gnames_VE)
rm(gnames_BR, gnames_EC, gnames_PA, gnames_PE, gnames_VE)


# TIDY DATA -------------------------------------------------------------------

# ----------------------------------- #
# STEP 1 - simplify colombia geometry for more computationally efficient workflow
colombia <- ms_simplify(colombia, keep = 0.01)
colombia <- colombia %>% 
  st_as_sf() %>% 
  st_transform(., crs = st_crs(prj))
# ----------------------------------- #

# ----------------------------------- #
# STEP 2- Identify municipalities with an international border
# Add a 1 km distance buffer to account for imperfect geometries 
# between simple features.

# Re-project South America shapefile
s_america <- st_transform(s_america, crs = st_crs(prj))

# THIS TAKES A LONG PROCESSING TIME
border <- st_is_within_distance(x      = colombia,
                                y      = s_america,
                                dist   = 1e3,
                                sparse = FALSE)
border <- border %>%
  as.data.frame() %>%
  mutate_if(is.logical, as.numeric) %>%
  mutate(border_international = case_when(V1 == T | 
                                          V2 == T | 
                                          V3 == T | 
                                          V4 == T | 
                                          V5 == T ~ 1,
                                          TRUE ~ 0)) %>%
  mutate(border_international = as_factor(border_international)) %>%
  rename(border_ecuador   = V1,
         border_peru      = V2,
         border_brazil    = V3,
         border_venezuela = V4,
         border_panama    = V5)
write_csv(border, path = 'data/covariate_data/borders.csv')

colombia <- colombia %>%
  bind_cols(., border)
rm(border)
# ----------------------------------- #

# ----------------------------------- #
# STEP 3 - Estimate areas in square kilometers
colombia <- colombia %>%
  mutate(km2    = as.numeric(st_area(.) / 1e6)) %>%
  mutate(km2_ln = log(km2))
# ----------------------------------- #

# ----------------------------------- #
# STEP 4 - Estimate municipality centroids.
# This value will serve as both the input in the continuous models and will be used 
# to estimate the distance_ variables
# https://r-spatial.github.io/sf/reference/geos_unary.html


# STEP 4.a - Estimate projected centroids for accurate distance estimates
centroids_proj <- st_centroid(colombia)$geometry
colombia$centroid_mun_proj <- centroids_proj

# STEP 4.b - Estimate longitude and latitude for spde model
centroids_longlat <- st_transform(centroids_proj, 
                                  crs = "+proj=longlat +datum=WGS84") %>%
  st_coordinates(.) %>% 
  as.data.frame()   %>%
  rename(centroid_mun_long = X, 
         centroid_mun_lat  = Y)

colombia <- colombia %>%
  bind_cols(., centroids_longlat)
rm(centroids_longlat)
# ----------------------------------- #

# POINT LOCATIONS OF INTEREST -------------------------------------------------
# Prep gnames object to build location of: 
#     - Department capitals
#     - Population centers with at least 50,000 inhabitants.
#     - Bogota

# Department captials
dept_capitals <- gnames_CO %>%
  filter(feature_code  == 'PPLA') %>%               # seat of a first-order administrative division
  dplyr::select(name, alternate_name, latitude, longitude, population)

coordinates(dept_capitals) <- c('longitude','latitude')
dept_capitals              <- dept_capitals %>% st_as_sf()
st_crs(dept_capitals)      <- "+proj=longlat +datum=WGS84"
dept_capitals              <- st_transform(dept_capitals, prj)

# Populated places [only Colombia]
pop_50k_CO <- gnames_CO %>%
  filter(feature_class == 'P', 
         population >= 50e3) %>%    # city or village
  dplyr::select(name, alternate_name, latitude, longitude, population)

coordinates(pop_50k_CO) <- c('longitude','latitude')
pop_50k_CO              <- pop_50k_CO %>% st_as_sf()
st_crs(pop_50k_CO)      <- "+proj=longlat +datum=WGS84"
pop_50k_CO              <- st_transform(pop_50k_CO, crs = prj) 

# Populated places [Colombia and surrounding countries]
pop_50k_SA <- gnames_SA %>%
  filter(feature_class == 'P', 
         population >= 50e3) %>%    # city or village
  dplyr::select(name, alternate_name, latitude, longitude, population)

coordinates(pop_50k_SA) <- c('longitude','latitude')
pop_50k_SA              <- pop_50k_SA %>% st_as_sf()
st_crs(pop_50k_SA)      <- "+proj=longlat +datum=WGS84"
pop_50k_SA              <- st_transform(pop_50k_SA, crs = prj) 

# Bogota
bogota <- pop_50k_CO[which(pop_50k_CO$population>5e6),]
rm(gnames_CO, gnames_SA)  



# DISTANCE CALCULATIONS -------------------------------------------------------
# DISTANCE: ECUADOR -----------------------------
dist_ecu <- st_distance(x = centroids_proj,
                        y = s_america[1,])
dist_ecu <- as.numeric(dist_ecu / 1e3)
colombia$distance_ecuador_km <- dist_ecu

# DISTANCE: PERU --------------------------------
dist_per <- st_distance(x = centroids_proj,
                        y = s_america[2,])
dist_per <- as.numeric(dist_per / 1e3)
colombia$distance_peru_km <- dist_per

# DISTANCE: BRAZIL ------------------------------
dist_bra <- st_distance(x = centroids_proj, 
                        y = s_america[3,])
dist_bra <- as.numeric(dist_bra / 1e3)
colombia$distance_brazil_km <- dist_bra

# DISTANCE: VENEZUELA ---------------------------
dist_ven <- st_distance(x = centroids_proj,
                        y = s_america[4,])
dist_ven <- as.numeric(dist_ven / 1e3)
colombia$distance_venezuela_km <- dist_ven

# DISTANCE: PANAMA ------------------------------
dist_pan <- st_distance(x = centroids_proj,
                        y = s_america[5,])
dist_pan <- as.numeric(dist_pan / 1e3)
colombia$distance_panama_km <- dist_pan

# DISTANCE: INTERNATIONAL BORDER ----------------
s_america   <- st_union(s_america)
dist_border <- st_distance(x = centroids_proj, 
                           y = s_america)
dist_border <- as.numeric(dist_border / 1e3)
colombia$distance_int_border_km <- dist_border

rm(dist_border, dist_ecu, dist_per, dist_bra,
   dist_ven, dist_pan, s_america, s_america_names)


# DISTANCE: POPULATED PLACE (5OK) COLOMBIA ------
dist_50k_CO <- st_distance(x = centroids_proj,
                           y = pop_50k_CO)
dist_50k_CO <- as.data.frame(as.matrix(dist_50k_CO)) %>%
  apply(., 1, FUN=min)
dist_50k_CO <- dist_50k_CO / 1e3

colombia$distance_50k_co_km <- dist_50k_CO
rm(dist_50k_CO, pop_50k_CO)

# DISTANCE: POPULATED PLACE (5OK) REGIONAL ------
dist_50k_SA <- st_distance(x = centroids_proj,
                           y = pop_50k_SA)
dist_50k_SA <- as.data.frame(as.matrix(dist_50k_SA)) %>%
  apply(., 1, FUN=min)
dist_50k_SA <- dist_50k_SA / 1e3

colombia$distance_50k_sa_km <- dist_50k_SA
rm(dist_50k_SA, pop_50k_SA)

# DEPARTMENT CAPITAL ------------------ 
dist_dept_capital <- st_distance(x = centroids_proj,
                                 y = dept_capitals)
dist_dept_capital <- as.data.frame(as.matrix(dist_dept_capital)) %>%
  apply(., 1, FUN=min)
dist_dept_capital <- dist_dept_capital / 1e3

colombia$distance_dept_capital_km <- dist_dept_capital
rm(dist_dept_capital, dept_capitals)

# BOGOTA ------------------------------
dist_bogota <- st_distance(x = centroids_proj,
                           y = bogota)
dist_bogota <- as.numeric(dist_bogota / 1e3)
colombia$distance_bogota_km <- dist_bogota
rm(dist_bogota, bogota, centroids_proj, prj)



# FINAL CLEANUP ---------------------------------------------------------------

  
# Save data frame from shapefile as a separate object for easier merging
colombia <- colombia %>%
  mutate(ID = row_number(),
         admin1Name   = iconv(admin1Name, from = 'UTF-8', to = 'ASCII//TRANSLIT'),
         admin2RefN   = iconv(admin2RefN, from = 'UTF-8', to = 'ASCII//TRANSLIT'),
         ID_Mun       = as.numeric(str_remove_all(admin2Pcod, 'CO'))) %>%
  rename(Department   = admin1Name,
         Municipality = admin2RefN) %>%
  dplyr::select(
    # Organization variables:
    ID_Mun, Department, Municipality,
    
    # Municipality areas:
    km2, km2_ln,
    
    # Border dummy variables:
    border_ecuador, border_peru, border_brazil, 
    border_venezuela, border_panama, border_international,
    
    # Distance to borders:
    distance_ecuador_km, distance_peru_km, distance_brazil_km,
    distance_venezuela_km, distance_panama_km, distance_int_border_km,
    
    # Distance to points-of-interest:
    distance_50k_co_km, distance_50k_sa_km, distance_dept_capital_km, distance_bogota_km,
    
    # Coordintates:
    centroid_mun_proj, centroid_mun_long, centroid_mun_lat, geometry) %>%
  filter(ID_Mun != 88001) #'San Andres Y Providencia'

# Reset geometry since centroids_mun_proj may default
colombia <- st_set_geometry(colombia, colombia$geometry)

# SAVE ------------------------------------------------------------------------
save(colombia, file = 'data/colombia_admin.RData')

