#-----------------------------------------------------------------------------#
#                                                                             
# Author:        Logan Stundal                                                    
# Date:          March 06, 2020                                                 
# Purpose:       Generate latitude and longitude points for each Colombian municipality
#                sampled from uniform distributions.
#                                                                             
#
# Copyright (c): Logan Stundal, 2020                      
# Email:         stund005@umn.edu
#
#-----------------------------------------------------------------------------# 
#
# Notes:                                                                    
#                                                                             
#                                                                             
#-----------------------------------------------------------------------------#



# ADMINISTRATIVE --------------------------------------------------------------

#---------------------------#
# Clear working directory   
#---------------------------#
rm(list = ls())

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
# setwd()

#---------------------------#
# Load data                 
#---------------------------#
load('data/colombia.rdata')
rm(list = setdiff(ls(), 'colombia'))

#---------------------------#
# Load functions            
#---------------------------#
st_random <- function(sf, data_sets = 10, replace = T, full_sim = 1e4){
  # NOTE: this function returns a LIST of data frames of length 'n'
  # that each contain ONE random point sampled from within the boundaries
  # of EACH geography within the supplied 'sf' object.
  
  # sf - a simple features object
  #  n - the number of random points to sample in each unique geometry
  # replace - sample with replacement? Default is true
  
  # Set-up
  sf = sf[,'geometry']
  nr = nrow(sf)
  sf = st_transform(sf, crs = CRS("+proj=longlat +datum=WGS84"))
  
  # Extract random points in each sub-geography and add to list:
  pts = list()
  
  for(i in 1:nr){
    cat('\14')
    print(sprintf('Working on sub-unit %d of %d.', i, nr))
    
    tmp = sf[i,]
    
    e      = extent(tmp)
    rand.x = runif(n = full_sim, min = e@xmin, max = e@xmax)
    rand.y = runif(n = full_sim, min = e@ymin, max = e@ymax)
    d = data.frame('X' = rand.x,
                   'Y' = rand.y)
    coordinates(d) = ~ X + Y
    d = st_as_sf(d)
    st_crs(d) <- CRS("+proj=longlat +datum=WGS84")
    
      
    d = suppressMessages(d[as.logical(st_within(d, tmp, sparse = FALSE)),])
    d = tryCatch(
      expr = {
        sample_n(d, size = data_sets, replace = replace)
      },
      error = function(e){
        stop("Insufficient points to generate desired number of data sets. Increase full_sim parameter.")
      }
    )
    
    if(nrow(d) < data_sets){
      stop("Insufficient points to generate desired number of data sets. Increase full_sim parameter.")
    }
    
    pts[[i]] = as.data.frame(st_coordinates(d))
  }
  
  # Build count of:"data_sets" dataframes with randomly sampled points
  out = list()
  for(i in 1:data_sets){
    cat('\14')
    print(sprintf('Building data set: %d of %d...', i, data_sets))
    out[[i]] = sapply(1:nr, function(z){pts[[z]][i,]}) %>% 
      t %>% 
      as.data.frame %>%
      unnest(., cols = c(X,Y))
  };rm(i)
  
  return(out)
}


# SAMPLE RANDOM DATA ----------------------------------------------------------
col_pts <- st_random(sf        = colombia, 
                     data_sets = 1000, 
                     replace   = FALSE,  # I want 1000 UNIQUE points 
                     full_sim  = 10000)

# SAVE ------------------------------------------------------------------------
save(col_pts, file = 'data/col_pts.rdata')
rm(list = ls())
