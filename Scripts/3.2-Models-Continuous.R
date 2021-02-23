#-----------------------------------------------------------------------------#
#                                                                             
# Author:        Logan Stundal                                                    
# Date:          May 23, 2020                                                 
# Purpose:       INLA SPDE geostatistical models                      
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
library(INLA)
library(sf)

#---------------------------#
# Set working directory     
#---------------------------#
setwd('C:/Users/logan/GoogleDrive/UMN/RESEARCH/RA_John/Event_Data_Project')

#---------------------------#
# Load data                 
#---------------------------#
load('data/Colombia.Rdata')
rm(list = setdiff(ls(),c('colombia', 'colombia0')))

#---------------------------#
# Load functions            
#---------------------------#


# TIDY DATA -------------------------------------------------------------------
# CONSTRUCT DISAGREEMENT VARIABLES
colombia <- colombia %>%
  mutate(icews_farc_bin    = as.integer(icews_farc > 0),
         ged_farc_bin      = as.integer(ged_farc > 0),
         cinep_bin         = as.integer(cinep_farc > 0)) %>%
  mutate(icews_cinep_under = case_when(icews_farc_bin == 0 & cinep_bin == 1 ~ 1, TRUE ~ 0),
         icews_cinep_over  = case_when(icews_farc_bin == 1 & cinep_bin == 0 ~ 1, TRUE ~ 0),
         icews_cinep_bias  = case_when(icews_farc_bin != cinep_bin ~ 1, TRUE ~ 0),
         
         ged_cinep_under   = case_when(ged_farc_bin == 0 & cinep_bin == 1 ~ 1, TRUE ~ 0),
         ged_cinep_over    = case_when(ged_farc_bin == 1 & cinep_bin == 0 ~ 1, TRUE ~ 0),
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


# INLA ANALYSIS ---------------------------------------------------------------
# Coordinates for mesh - key for prior construction in SPDE models.
colcoord <- cbind(colombia$centroid_mun_long, 
                  colombia$centroid_mun_lat)

border   <- inla.mesh.segment(colcoord)
mesh     <- inla.mesh.2d(loc.domain = border$loc, max.edge = c(1.6))
nv       <- mesh$n
A        <- inla.spde.make.A(mesh = mesh,
                             loc  = as.matrix(cbind(colombia$centroid_mun_long,
                                                    colombia$centroid_mun_lat)))

# SPDE ------------------------------ #
spde <- inla.spde2.matern(mesh, alpha = 2)

# MODEL SETUP ----------------------- #
formula <- y ~ -1 + intercept + dist + pop + tri + f(spatial.field, model=spde)

dvs <- c(
         'icews_farc_bin', 'ged_farc_bin', 'cinep_bin',
         'icews_cinep_under','ged_cinep_under',
         'icews_cinep_over', 'ged_cinep_over',
         'icews_cinep_bias', 'ged_cinep_bias'
         )

stacks <- lapply(dvs, function(x){
  inla.stack(data    = list(y = colombia[[x]]),
             A       = list(A,1,1,1), 
             effects = list(c(list(intercept = rep(1,nv)),
                              inla.spde.make.index("spatial.field", spde$n.spde)),
                            dist = colombia$distance_bogota_km_ln,
                            pop  = colombia$google_ee_pop_sum_ln,
                            tri  = colombia$google_terrain_ri_mean_m),
             tag='spde')
});names(stacks) <- dvs

# ESTIMATION ------------------------ #
inla_mods <- lapply(1:length(dvs), function(x){
  print(sprintf('Working on model DV: %s', dvs[x]))

  inla(formula           = formula, 
       data              = inla.stack.data(stacks[[x]], 
                                           spde = spde),
       family            = 'binomial',
       control.family    = list(link    = 'probit'),
       control.predictor = list(A       = inla.stack.A(stacks[[x]]), 
                                compute = TRUE,
                                link    = 1),
       control.compute   = list(waic    = TRUE, 
                                config  = TRUE),
       control.fixed     = list(correlation.matrix=TRUE))
})

names(inla_mods) <- dvs


# Quantities of interest ------------------------------------------------------

# Observed FARC event models
# Regression coefficients
inla_betas <- lapply(inla_mods[1:3], function(mod){
  round(mod$summary.fixed[,1:5],3)
})

inla_hyper <- lapply(inla_mods[1:3], function(mod, d = 3){
  spde_pars = inla.spde2.result(inla = mod, 
                                name = "spatial.field", 
                                spde,do.transform = TRUE)  
  
  # Kappa
  Kappa    = inla.emarginal(function(x) x, spde_pars$marginals.kappa[[1]])  # kappa (mean)
  Kappahpd = inla.hpdmarginal(0.95, spde_pars$marginals.kappa[[1]])         # kappa (hpd 95%)
  
  # Sigma
  Sigma    = inla.emarginal(function(x) x, spde_pars$marginals.variance.nominal[[1]]) # variance (mean)
  Sigmahpd = inla.hpdmarginal(0.95, spde_pars$marginals.variance.nominal[[1]])        # variance (hpd 95%)
  
  # Range
  Range       = inla.emarginal(function(x) x, spde_pars$marginals.range.nominal[[1]]) # range (mean)
  Rangehpd    = inla.hpdmarginal(0.95, spde_pars$marginals.range.nominal[[1]])        # range (hpd 95%)
  
  # Convert range to km (degrees = 2*pi*6371/360)
  Range    = Range * 2*pi*6371/360
  Rangehpd = Rangehpd * 2*pi*6371/360
  
  return(list('Kappa'     = round(Kappa, d),
              'Kappa-hpd' = round(Kappahpd, d),
              'Sigma'     = round(Sigma, d),
              'Sigma-hpd' = round(Sigmahpd, d),
              'Range'     = round(Range, d),
              'Rangehpd'  = round(Rangehpd, d)))
})

inla_lliks <- lapply(inla_mods[1:3], function(mod){
  mod$mlik[1]
})


# Observed Bias models
# Regression coefficients
inla_betas <- lapply(inla_mods[4:9], function(mod){
  round(mod$summary.fixed[,1:5],3)
})

inla_hyper <- lapply(inla_mods[4:9], function(mod, d = 3){
  spde_pars = inla.spde2.result(inla = mod, 
                                name = "spatial.field", 
                                spde,do.transform = TRUE)  
  
  # Kappa
  Kappa    = inla.emarginal(function(x) x, spde_pars$marginals.kappa[[1]])  # kappa (mean)
  Kappahpd = inla.hpdmarginal(0.95, spde_pars$marginals.kappa[[1]])         # kappa (hpd 95%)
  
  # Sigma
  Sigma    = inla.emarginal(function(x) x, spde_pars$marginals.variance.nominal[[1]]) # variance (mean)
  Sigmahpd = inla.hpdmarginal(0.95, spde_pars$marginals.variance.nominal[[1]])        # variance (hpd 95%)
  
  # Range
  Range       = inla.emarginal(function(x) x, spde_pars$marginals.range.nominal[[1]]) # range (mean)
  Rangehpd    = inla.hpdmarginal(0.95, spde_pars$marginals.range.nominal[[1]])        # range (hpd 95%)
  
  # Convert range to km (degrees = 2*pi*6371/360)
  Range    = Range * 2*pi*6371/360
  Rangehpd = Rangehpd * 2*pi*6371/360
  
  return(list('Kappa'     = round(Kappa, d),
              'Kappa-hpd' = round(Kappahpd, d),
              'Sigma'     = round(Sigma, d),
              'Sigma-hpd' = round(Sigmahpd, d),
              'Range'     = round(Range, d),
              'Rangehpd'  = round(Rangehpd, d)))
})

inla_lliks <- lapply(inla_mods[4:9], function(mod){
  mod$mlik[1]
})





# SAVE ------------------------------------------------------------------------
# save.image(file = 'Data/Models-Continuous.Rdata')
rm(list = ls())



