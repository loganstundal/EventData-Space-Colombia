#-----------------------------------------------------------------------------#
#                                                                             
# Author:        Logan Stundal                                                    
# Date:          March 21, 2020                                                 
# Purpose:       Data tidy for Colombia project, version 3.                      
#                                                                             
#
# Copyright (c): Logan Stundal, 2020                      
# Email:         stund005@umn.edu
#
#-----------------------------------------------------------------------------# 
#
# Notes:                                                                    
#         -- This version employs much higher resolution covariate information 
#            rather than prio grid data which does not apply well for small 
#            municipalities in Colombia. 
#
#-----------------------------------------------------------------------------#



# ADMINISTRATIVE --------------------------------------------------------------
#---------------------------#
# Clear working directory   
#---------------------------#
rm(list = ls(all.names = TRUE))
# .rs.restartR()

#---------------------------#
# Load required libraries   
#---------------------------#
library(tidyverse)
library(sf)
library(sp)
library(raster)

#---------------------------#
# Set working directory     
#---------------------------#
setwd("C:/Users/logan/GoogleDrive/UMN/RESEARCH/RA_John/Event_Data_Project")

# _____________________ -----
#---------------------------#
# Load data                 
#---------------------------#
# COLOMBIA ADMIN --------------------------------------------------------------
# Colombia Administrative units - gen. in 'Colombia_Admin_Units.R' script 
# Projected to South America Albers Equal Area Conic
load('data/Administrative_units/colombia_admin.rdata')

# ICEWS / GED -----------------------------------------------------------------
# event_original <- read_csv('data/event_data/gedicews_20190627.csv',
#                            col_types = cols(.default = "d"))
event_farc     <- read_csv('data/event_data/gedicews_FARC_20190908.csv',
                           col_types = cols(.default = "d"))
# event_noUNK    <- read_csv('data/event_data/gedicews_NoUKN_20190908.csv',
#                            col_types = cols(.default = "d"))


# CINEP -----------------------------------------------------------------------
# Courtsey JEN HOLMES, UT Dallas [this file per Ben's email on March 14 at 9am]
cinep <- haven::read_dta('data/event_data/CINEP/CINEP_HRV_Farc.dta') 

# Municipality covariates -------------
cinep_cov  <- haven::read_dta('data/covariate_data/CINEP_Mun_Covariates.dta')


# PRIO ------------------------------------------------------------------------
prio.yr       <- read_csv(paste0('c:/users/logan/googledrive/umn/RESEARCH/DATA/priogrid/',
                                 'PRIO-GRID Yearly Variables for 1995-2014 - 2018-08-12.csv'))
prio.static   <- read_csv(paste0('c:/users/logan/googledrive/umn/RESEARCH/DATA/priogrid/',
                                 'PRIO-GRID Static Variables - 2018-08-12.csv'))

# Prio shapefile at: 'c:/users/logan/googledrive/umn/RESEARCH/DATA/priogrid/priogrid_cell.shp'
prio.crs      <- st_crs("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")


# GOOGLE EARTH ENGINE EXTRACTS ------------------------------------------------
forest <- read_csv('data/covariate_data/googleearthengine/Colombia_ForestCover_municipality_2005.csv') %>%
  dplyr::select(c(2,4)) %>% 
  rename(ID_Mun = 1,
         google_ee_forest_per = 2) %>%
  mutate(ID_Mun = as.numeric(str_remove_all(ID_Mun, 'CO')))

nightlight <- read_csv('data/covariate_data/googleearthengine/Colombia_NightLights_municipality_2002_2009_mean.csv') %>%
  dplyr::select(c(2,4)) %>%
  rename(ID_Mun = 1, 
         google_ee_nl_index = 2) %>%
  mutate(ID_Mun = as.numeric(str_remove_all(ID_Mun, 'CO')))

worldpop <- read_csv('data/covariate_data/googleearthengine/Colombia_Population_municipality_2002_2009_mean.csv') %>%
  dplyr::select(c(2,4)) %>%
  rename(ID_Mun = 1,
         google_ee_pop_sum = 2) %>%
  mutate(ID_Mun = as.numeric(str_remove_all(ID_Mun, 'CO')))

tri <- read_csv('data/covariate_data/googleearthengine/Colombia_tri_municipality_mean.csv') %>%
  dplyr::select(c(2,4)) %>%
  rename(ID_Mun = 1,
         google_terrain_ri_mean_m = 2) %>%
  mutate(ID_Mun = as.numeric(str_remove_all(ID_Mun, 'CO')))


# ELECTIONS: 2002, 2006 -------------------------------------------------------
# Election covariate data:
elec <- read_csv('data/covariate_data/elections/election_tidy.csv')

#---------------------------#
# Load functions            
#---------------------------#
{
  .spAg <- function(shp,pts,vr,fn,na.rm=T){
    if(class(shp)[1] == 'sf'){
      shp$ID = rownames(shp) <- 1:nrow(shp)
      shp2   = aggregate(pts[vr], by = shp, FUN = function(x) c(fn(x, na.rm = T)))
      
      shp2[is.na(shp2)] = 0
      shp2$ID = rownames(shp2) = 1:nrow(shp2)
      
      shp2$geometry = NULL
      
      return(shp = left_join(shp,shp2,by='ID'))
      
    } else {
      shp$ID = rownames(shp@data) <- 1:nrow(shp@data)
      shp2   = aggregate(pts[vr], by = shp, FUN = function(x) c(fn(x,na.rm=T)))
      
      # Replace NAs with 0 for spatial units with no events.
      shp2@data[is.na(shp2@data)] <- 0
      
      shp2$ID    <- rownames(shp2@data) <- 1:nrow(shp2@data)
      return(shp <- merge(shp,shp2,by='ID'))
    }
  }
  
  .scaler <- function(x) log(x+1) / max(log(x+1))
}

# _____________________ ----
# TIDY DATA -------------------------------------------------------------------

#---------------------------------#
# STUDY YEARS 
#---------------------------------#
years <- seq(2002,2009) 
# Re: Ben email 6/27/19 -- switched from test of 00-06 to 02-09


# ICEWS / GED -----------------------------------------------------------------

#---------------------------------#
# ICEWS AND GED - ONLY FARC       #
#---------------------------------#
event_farc <- event_farc %>%
  dplyr::select(year,latitude,longitude, sort(names(.))) %>%
  filter(year %in% years) %>%
  rowwise() %>%
  mutate(icews_farc       = rowSums(cbind(rebcivicews, rebgovicews),na.rm=T),
         icews_farc_stand = rowSums(cbind(rebcivicewsstand, rebgovicewsstand),na.rm=T),
         ged_farc         = rowSums(cbind(rebcivged, rebgovged),na.rm=T),
         ged_farc_stand   = rowSums(cbind(rebcivgedstand, rebgovgedstand),na.rm=T)) %>%
  dplyr::select(latitude, longitude, 
                icews_farc, icews_farc_stand, 
                ged_farc, ged_farc_stand)

coordinates(event_farc) <- c("longitude","latitude")
proj4string(event_farc) <- CRS("+proj=longlat +datum=WGS84")
event_farc  <- event_farc %>%
  st_as_sf() %>%
  st_transform(., st_crs(colombia))


# PRIO ------------------------------------------------------------------------
prio <- prio.yr %>%
  filter(year %in% years) %>%
  left_join(.,prio.static,by='gid') %>%
  filter(gwno %in% c(100,101,130,135,95,140)) %>%
  mutate(prio_petroleum_y_mean = ifelse(is.na(petroleum_y)==T,0,petroleum_y),
         prio_drug_y_mean      = ifelse(is.na(drug_y)==T,0,drug_y))%>%
  rename(prio_mountains_mean   = mountains_mean,
         prio_capdist_mean     = capdist,
         prio_pop_gpw_mean     = pop_gpw_sum,
         prio_ttime_mean       = ttime_mean,
         prio_forest_gc_mean   = forest_gc) %>%
  group_by(gid) %>%
  summarise_each( ~ mean(., na.action = NULL, na.rm = TRUE))

# PRIO - Spatial extract
prio_vars <- c('prio_mountains_mean','prio_capdist_mean','prio_pop_gpw_mean',
               'prio_ttime_mean','prio_forest_gc_mean',
               'prio_petroleum_y_mean','prio_drug_y_mean')

prio <- rasterFromXYZ(prio[,c('xcoord','ycoord',prio_vars)],
                      res = c(0.5, 0.5),
                      crs = prio.crs$proj4string)
prio <- projectRaster(prio, crs = st_crs(colombia)$proj4string) 
rm(prio.yr, prio.static, prio.crs)


# CINEP -----------------------------------------------------------------------
cinep <- cinep %>%
  filter(year %in% years) %>%
  group_by(id_2) %>%
  summarise(cinep_farc = n()) %>%
  rename(ID_Mun = id_2)
   
# Municipality covariates -------------
cinep_cov <- cinep_cov %>%
  filter(year %in% years) %>%
  dplyr::select(ID_MUN, pop, popch, SocServicespc_b08, Forest_pctg, slope) %>%
  group_by(ID_MUN) %>%
  summarise_each( ~ mean(., na.action = NULL, na.rm = TRUE)) %>%
  rename(ID_Mun                       = ID_MUN, 
         cinep_pop_mean               = pop,
         cinep_popch_mean             = popch,
         cinep_socservicespc_b08_mean = SocServicespc_b08,
         cinep_forest_pctg_mean       = Forest_pctg,
         cinep_slope_mean             = slope) %>%
  ungroup() %>%
  mutate(ID_Mun = as.numeric(ID_Mun))

rm(years)


# _____________________ ----
# MERGE DATA-------------------------------------------------------------------

# ICEWS / GED -------------------------
#-------------------------------------#
event_farc <- .spAg(shp = colombia[,c('ID_Mun','geometry')],
                    pts = event_farc,
                    fn  = sum,
                    vr  = names(event_farc)) %>%
  as.data.frame() %>%
  dplyr::select(-ID) %>%
  mutate(geometry = NULL)

colombia <- colombia %>%
  left_join(., event_farc, by = 'ID_Mun')
rm(event_farc)

# PRIO --------------------------------
#-------------------------------------#
for(x in 1:length(prio_vars)) {
  var = prio_vars[x]
  print(sprintf('Working on variable: %s...', var))
  
  if(var == 'prio_pop_gpw_mean'){
    tmp = raster::extract(prio[[var]],colombia,fun=sum,na.rm=T)
  } else{
    tmp = raster::extract(prio[[var]],colombia,fun=mean,na.rm=T)
  }
  
  colombia[var] = as.vector(tmp)
  
  if(var %in% c('prio_capdist_mean',
                'prio_pop_gpw_mean',
                'prio_ttime_mean')){
    tmp = colombia[,c(var)]
    tmp$geometry = NULL
    
    colombia[paste0(var, '_ln')] = log(as.vector(tmp[var]))
  }
};rm(prio_vars, prio, var, x, tmp)


# CINEP -------------------------------
#-------------------------------------#
colombia <- colombia %>%
  left_join(.,cinep, by = 'ID_Mun')
rm(cinep)


# Municipality covariates -------------
#-------------------------------------#
colombia <- colombia %>%
  left_join(., cinep_cov, by = 'ID_Mun')
rm(cinep_cov)


# Google Earth Engine extracts --------
#-------------------------------------#
colombia <- colombia %>%
  left_join(., y = forest,     by = 'ID_Mun') %>%
  left_join(., y = nightlight, by = 'ID_Mun') %>%
  left_join(., y = worldpop,   by = 'ID_Mun') %>%
  left_join(., y = tri,        by = 'ID_Mun')
rm(forest, nightlight, worldpop, tri)


# Election Results, 2002 --------------
#-------------------------------------#
# Since this is a one-to-many, need to use plyr::join
col_grid <- colombia[,c('ID_Mun','Department')]
col_grid$geometry <- NULL
col_grid$Department <- str_to_lower(col_grid$Department)

col_grid <- plyr::join(x     = col_grid, 
                       y     = elec, 
                       by    = 'Department', 
                       type  ='left', 
                       match ='all') %>%
  dplyr::select(-Department)

colombia <- colombia %>%
  left_join(., y = col_grid, by = 'ID_Mun')
rm(col_grid, elec)


# Dependent variable - agreement indicators -----
# --------------------------------------------- #
# CONSTRUCT DISAGREEMENT VARIABLES
colombia <- colombia %>%
  mutate(cinep_farc     = ifelse(is.na(cinep_farc), 0, cinep_farc)) %>%
  mutate(icews_farc_bin = as.integer(icews_farc > 0),
         ged_farc_bin   = as.integer(ged_farc > 0),
         cinep_farc_bin      = as.integer(cinep_farc > 0)) %>%
  mutate(icews_cinep_under = case_when(icews_farc_bin == 0 & cinep_bin == 1 ~ 1, TRUE ~ 0),
         icews_cinep_bias  = case_when(icews_farc_bin != cinep_bin ~ 1, TRUE ~ 0),
         
         ged_cinep_under   = case_when(ged_farc_bin == 0 & cinep_bin == 1 ~ 1, TRUE ~ 0),
         ged_cinep_bias    = case_when(ged_farc_bin != cinep_bin ~ 1, TRUE ~ 0),
         
         icews_ged_under   = case_when(icews_farc_bin == 0 & ged_farc_bin == 1 ~ 1, TRUE ~ 0),
         icews_ged_bias    = case_when(icews_farc_bin != ged_farc_bin ~ 1, TRUE ~ 0),
         
         icews_cinep_full_bias = case_when(icews_farc_bin == 0 & cinep_bin == 1 ~ 'Underreport',
                                           icews_farc_bin == 1 & cinep_bin == 0 ~ 'Overreport',
                                           icews_farc_bin == cinep_bin ~ 'Agree'),
         
         ged_cinep_full_bias = case_when(ged_farc_bin == 0 & cinep_bin == 1 ~ 'Underreport',
                                         ged_farc_bin == 1 & cinep_bin == 0 ~ 'Overreport',
                                         ged_farc_bin == cinep_bin ~ 'Agree'),
         
         distance_bogota_km_ln = log(distance_bogota_km),
         google_ee_pop_sum_ln  = log(google_ee_pop_sum),
         google_tri_ln         = log(google_terrain_ri_mean_m))


# _____________________
# FINAL TIDY ------------------------------------------------------------------
colombia <- colombia %>%
  
  # Create scaled variable versions
  mutate(icews_farc_scale           = .scaler(icews_farc),
         icews_farc_stand_scale     = .scaler(icews_farc_stand),
         ged_farc_scale             = .scaler(ged_farc),
         ged_farc_stand_scale       = .scaler(ged_farc_stand),
         cinep_farc_scale           = .scaler(cinep_farc)) %>%
  
  # Create a binary indicator for Bogota
  mutate(bogota_dummy = ifelse(Department == 'Bogota D.C.', 1, 0)) %>%
  
  rename(area_km2    = km2, 
         area_km2_ln = km2_ln) %>%
  
  dplyr::select(ID_Mun, Department, Municipality,
                
                dplyr::starts_with('icews'), 
                dplyr::starts_with('ged'),
                cinep_farc, cinep_farc_bin, cinep_farc_scale, 
                
                dplyr::starts_with('border'),
                dplyr::starts_with('distance'),
                dplyr::starts_with('area'),
                bogota_dummy,
                
                dplyr::starts_with('google'),
                dplyr::starts_with('prio'),
                
                cinep_pop_mean, cinep_popch_mean, 
                cinep_socservicespc_b08_mean, 
                cinep_forest_pctg_mean, cinep_slope_mean,
                
                dplyr::starts_with('election'), 
                
                centroid_mun_long, centroid_mun_lat, 
                
                centroid_mun_proj, geometry)

# Reset geometry to the multipolygon (only want to preserve the projected centroids for possible mapping)
colombia <- st_set_geometry(colombia, colombia$geometry)

# _____________________ ----
# COLOMBIA LEVEL 0 ------------------------------------------------------------
colombia0 <- read_rds(url('https://biogeo.ucdavis.edu/data/gadm3.6/Rsf/gadm36_COL_0_sf.rds'))
colombia0 <- st_transform(colombia0, crs = st_crs(colombia))

colombia0 <- st_crop(x = colombia0, 
                     y = st_bbox(colombia))

# SPATIAL WEIGHTS -------------------------------------------------------------
library(spdep)

# Create a spatial polygons dataframe to pass to 'poly2nb':
colombia.shp <- colombia[,c('ID_Mun','geometry')]
colombia.shp$centroid_mun_proj <- NULL
colombia.shp <- st_set_geometry(colombia.shp, colombia.shp$geometry)
colombia.shp <- as_Spatial(from = colombia.shp, 
                           IDs  = colombia.shp$ID_Mun)

# QUEEN - Spatial neighbors, matrix and lists:
nb.r.queen     <- poly2nb(pl        = colombia.shp,                    
                          row.names = colombia.shp$ID_Mun,                 
                          queen     = TRUE)                            
nb.lst.queen   <- nb2listw(nb.r.queen)                                       
W_matrix.queen <- nb2mat(neighbours = nb.r.queen, 
                         style      = "W")                            
colnames(W_matrix.queen) <- rownames(W_matrix.queen)     

# ROOK - Spatial neighbors, matrix and lists:
nb.r.rook     <- poly2nb(pl        = colombia.shp,                    
                         row.names = colombia.shp$ID_Mun,                 
                         queen     = FALSE)                            
nb.lst.rook   <- nb2listw(nb.r.rook)                                       
W_matrix.rook <- nb2mat(neighbours = nb.r.rook, 
                        style      = "W")                            
colnames(W_matrix.rook) <- rownames(W_matrix.rook)

rm(colombia.shp)

# _____________________ ----
# SAVE ------------------------------------------------------------------------


# CSV file for sharing with Ben and John
write_csv(x    = colombia, 
          path = 'data/colombia.csv')

# Rdata file for future modeling
save.image(file = 'data/colombia.Rdata')
rm(list=ls())

