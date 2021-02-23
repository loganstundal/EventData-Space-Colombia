#-----------------------------------------------------------------------------#
#                                                                             
# Author:        Logan Stundal                                                    
# Date:          March 21, 2020                                                 
# Purpose:       Extimate spatial error probit models                      
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
library(ProbitSpatial)
library(spdep)

#---------------------------#
# Set working directory     
#---------------------------#
setwd('c:/users/logan/googledrive/umn/research/ra_john/event_data_project')

#---------------------------#
# Load data                 
#---------------------------#
load('data/Colombia.Rdata')
# rm(list = setdiff(ls(),'colombia'))

#---------------------------#
# Load functions            
#---------------------------#
source('scripts/Functions_EDP.R')


# MODEL SETUP -----------------------------------------------------------------

# model_formula <- 'distance_bogota_km + google_ee_pop_sum_ln + google_terrain_ri_mean_m'
model_formula <- 'log(distance_bogota_km) + google_ee_pop_sum_ln + google_terrain_ri_mean_m'

dvs_events    <- c('icews_farc_bin', 'ged_farc_bin', 'cinep_farc_bin')
dvs_under     <- c('icews_cinep_under','ged_cinep_under')

var_names <- list('(Intercept)'              = '$Intercept$',
                  # 'distance_bogota_km'       = '$Distance,\\ Bogota_{km}$',
                  'log(distance_bogota_km)'  = '$Distance,\\ Bogota^{\\dagger}_{km}$',
                  'google_ee_pop_sum_ln'     = '$Population^{\\dagger}$',
                  'google_terrain_ri_mean_m' = '$TRI$',
                  'rho'                      = '$\\lambda$')

# Robust ses?
robust   = T
hc       = 'HC1'

# Conditional or Full-likelihood approximation?
# sp_method = 'full-lik'       # 'conditional' or 'full-lik'
sp_method = 'conditional'
#-----------------------------------------------------------------------------#


# DISCRETE PROBIT MODELS: FARC EVENTS -----------------------------------------
# VANILLA PROBITS ---------------------
vanilla_probits_events <- lapply(c(dvs_events, dvs_under), function(x){
  glm(formula = as.formula(paste0(x, ' ~ ' ,model_formula)),
      data    = colombia,
      family  = binomial(link = 'probit'))
})

vanilla_probits_events.coefs <- lapply(1:5, function(x){
  coef(vanilla_probits_events[[x]])})
vanilla_probits_events.ses   <- lapply(1:5, function(x){
  sqrt(diag(sandwich::vcovHC(vanilla_probits_events[[x]], type = 'HC1')))})
vanilla_probits_events.pvals <- lapply(1:5, function(x){
  2 * (1 - pnorm(abs(coef(vanilla_probits_events[[x]]))/vanilla_probits_events.ses[[x]]))})
vanilla_probits_events.lliks <- lapply(1:5, function(x){
  logLik(vanilla_probits_events[[x]])})

vanilla_probits_events <- lapply(1:5, function(x){
  as.data.frame(cbind(vanilla_probits_events.coefs[[x]],
                      vanilla_probits_events.ses[[x]],
                      vanilla_probits_events.pvals[[x]]))
})
rm(vanilla_probits_events.coefs,
   vanilla_probits_events.ses,
   vanilla_probits_events.pvals)

var_names_vanilla <- list('(Intercept)'              = '$Intercept$',
                          'log(distance_bogota_km)'  = '$Distance,\\ Bogota^{\\dagger}_{km}$',
                          'google_ee_pop_sum_ln'     = '$Population^{\\dagger}$',
                          'google_terrain_ri_mean_m' = '$TRI$')

reg_table(models    = vanilla_probits_events,
          se.size   = 'footnotesize',
          mod_names = c('ICEWS','GED','CINEP','ICEWS','GED'),
          var_names = var_names_vanilla)


# SPATIAL ERROR PROBITS ---------------
se_probits_events <- lapply(dvs_events, function(x){
  print(sprintf('Working on variable: %s',x))
  SpatialProbitFit(formula = as.formula(paste0(x, ' ~ ' ,model_formula)),
                   data    = colombia,
                   W       = as(W_matrix.queen, 'dgCMatrix'),
                   DGP     = 'SEM',
                   method  = sp_method,
                   varcov  = 'varcov',
                   control = list(iW_CL = 6))
})

se_probits_events.coefs <- lapply(se_probits_events, function(x){c(x@beta, x@rho)})
se_probits_events.ses   <- lapply(1:length(dvs_events), function(x){
  print(sprintf('Working on variable: %s',x))
  se_robust(se_probits_events[[x]], robust = robust, hc = hc)})
se_probits_events.pvals <- lapply(1:length(dvs_events), function(x){
  2 * (1 - pnorm(abs(se_probits_events.coefs[[x]])/se_probits_events.ses[[x]]))})
se_probits_events.lliks <- lapply(se_probits_events, function(x){c(round(x@loglik,3))})

se_probits_events_df <- lapply(1:length(dvs_events), function(x){
  as.data.frame(cbind(se_probits_events.coefs[[x]], 
                      se_probits_events.ses[[x]], 
                      se_probits_events.pvals[[x]]))
})


# mods <- c(se_probits_events_df)
# for(i in 1:length(mods)){
#   colnames(mods[[i]]) = c('point','se','pval')
# };rm(i)
# 
# reg_table(models    = mods,
#           se.size   = 'footnotesize',
#           mod_names = rep(c('ICEWS','GED','CINEP')),
#           var_names = var_names)


# DISCRETE PROBIT MODELS: UNDERREPORTING --------------------------------------
# VANILLA PROBITS ---------------------
# vanilla_probits_under <- lapply(dvs_under, function(x){
#   glm(formula = as.formula(paste0(x, ' ~ ' ,model_formula)),
#       data    = colombia,
#       family  = binomial(link = 'probit'))
# })
# 
# vanilla_probits_under.coefs <- lapply(1:length(vanilla_probits_under), function(x){
#   coef(vanilla_probits_under[[x]])})
# vanilla_probits_under.ses   <- lapply(1:length(vanilla_probits_under), function(x){
#   sqrt(diag(sandwich::vcovHC(vanilla_probits_under[[x]], type = 'HC1')))})
# vanilla_probits_under.pvals <- lapply(1:length(vanilla_probits_under), function(x){
#   2 * (1 - pnorm(abs(coef(vanilla_probits_under[[x]]))/vanilla_probits_under.ses[[x]]))})
# vanilla_probits_under.lliks <- lapply(1:length(vanilla_probits_under), function(x){
#   logLik(vanilla_probits_under[[x]])})
# 
# vanilla_probits_under <- lapply(1:length(vanilla_probits_under), function(x){
#   as.data.frame(cbind(vanilla_probits_under.coefs[[x]], 
#                       vanilla_probits_under.ses[[x]], 
#                       vanilla_probits_under.pvals[[x]]))
# })
# rm(vanilla_probits_under.coefs,
#    vanilla_probits_under.ses,
#    vanilla_probits_under.pvals)


# SPATIAL ERROR PROBITS ---------------
se_probits_under <- lapply(dvs_under, function(x){
  print(sprintf('Working on variable: %s',x))
  SpatialProbitFit(formula = as.formula(paste0(x, ' ~ ' ,model_formula)),
                   data    = colombia,
                   W       = as(W_matrix.queen, 'dgCMatrix'),
                   DGP     = 'SEM',
                   method  = sp_method,
                   varcov  = 'varcov',
                   control = list(iW_CL = 6))
})

se_probits_under.coefs <- lapply(se_probits_under, function(x){c(x@beta, x@rho)})
se_probits_under.ses   <- lapply(1:length(dvs_under), function(x){
  print(sprintf('Working on variable: %s',x))
  se_robust(se_probits_under[[x]], robust = robust, hc = hc)})
se_probits_under.pvals <- lapply(1:length(dvs_under), function(x){
  2 * (1 - pnorm(abs(se_probits_under.coefs[[x]])/se_probits_under.ses[[x]]))})
se_probits_under.lliks <- lapply(se_probits_under, function(x){c(round(x@loglik,3))})

se_probits_under_df <- lapply(1:length(dvs_under), function(x){
  as.data.frame(cbind(se_probits_under.coefs[[x]], 
                      se_probits_under.ses[[x]], 
                      se_probits_under.pvals[[x]]))
})

# mods <- c(se_probits_under_df)
# for(i in 1:length(mods)){
#   colnames(mods[[i]]) = c('point','se','pval')
# };rm(i)
# 
# reg_table(models    = mods,
#           se.size   = 'footnotesize',
#           mod_names = rep(c('ICEWS','CINEP')),
#           var_names = var_names)

# ----------------------------------- #
# Mods Joint
mods <- c(se_probits_events_df, se_probits_under_df)
for(i in 1:length(mods)){
  colnames(mods[[i]]) = c('point','se','pval')
};rm(i)

reg_table(models    = mods,
          se.size   = 'footnotesize',
          mod_names = c('ICEWS','GED','CINEP','ICEWS','GED'),
          var_names = var_names)

#-----------------------------------------------------------------------------#
# SAVE ------------------------------------------------------------------------
# save.image("data/SPE-Discrete.Rdata")
rm(list = ls())
