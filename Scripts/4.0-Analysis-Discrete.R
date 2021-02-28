#-----------------------------------------------------------------------------#
#
# Author:        Logan Stundal
# Date:          February 27, 2021
# Purpose:       4.0: Analysis - Discrete
#
#
# Copyright (c): Logan Stundal, 2021
# Email:         stund005@umn.edu
#
#-----------------------------------------------------------------------------#
#
# Notes:
#
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
library(ProbitSpatial)

#---------------------------#
# Load data
#---------------------------#
load("data/data_variables.RData")
load("data/data_spatial.RData")

#---------------------------#
# Functions
#---------------------------#
source("Scripts/Functions/se_robust.R")



#-----------------------------------------------------------------------------#
# MODEL SETUP                                                             ----
#-----------------------------------------------------------------------------#

fn  <- paste0("distance_bogota_km_ln + I(distance_bogota_km_ln^2) + ",
              "pop_sum_ln + I(pop_sum_ln^2) +",
              "area_km2_ln + I(area_km2_ln^2) +" ,
              # "log(terrain_ri_mean_m) + ",          # better for observed mods
              "terrain_ri_mean_m + ",                 # better for under mods (nonlinear distance emerges for icews)
              "forest_per + nl_mean + bogota_dummy")
dvs <- c("icews_bin", "ged_bin", "cinep_bin",
         "icews_cinep_under","ged_cinep_under")

fs  <- lapply(paste(dvs, fn, sep = " ~ "), as.formula)
names(fs) <- dvs

rm(dvs, fn)
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# NON-SPATIAL PROBITS                                                     ----
#-----------------------------------------------------------------------------#

try(suppressWarnings({rm(ns_probits, ns_p_ses, ns_p_pvals)}))

# ----------------------------------- #
# Cross-section
# ----------------------------------- #
ns_probits <- lapply(fs, function(x){
  glm(formula = x,
      family  = binomial(link = "probit"),
      data    = colombia_cs)
})

ns_p_ses <- lapply(ns_probits, function(x){
  sqrt(diag(sandwich::vcovHC(x, type = "HC1")))
})

ns_p_pvals <- lapply(1:length(fs), function(x){
  2 * (1 - pnorm(abs(ns_probits[[x]]$coefficients)/ns_p_ses[[x]]))
})

texreg::htmlreg(l = ns_probits,
                override.se = ns_p_ses,
                override.pvalues = ns_p_pvals,
                file = "Documents/tst.html")
# ----------------------------------- #












# ----------------------------------- #
# Years-Grouped
# ----------------------------------- #
try(suppressWarnings({rm(vanilla, vses, vpvl)}))

yrs <- unique(colombia_yg$year_grouped)

vanilla <- lapply(yrs, function(y){
  tmp <- colombia_yg %>% filter(year_grouped == y)
  lapply(fs, function(x){
    glm(formula = x,
        family  = binomial(link = "probit"),
        data    = tmp)
  })
})

names(vanilla) <- paste0("Year_",yrs)

# vses <- lapply(vanilla, function(y){
#   lapply(y, function(x){
#     sqrt(diag(sandwich::vcovHC(x, type = "HC1")))
#   })
# })
#
# vpval <- lapply(vanilla, function(y){
#   lapply(1:length(fs), function(x){
#     2 * (1 - pnorm(abs(y[[x]]$coefficients)/vses[[x]][1]))
#   })
# })


cat("\14");texreg::screenreg(l = vanilla$`Year_2002-03`)
cat("\14");texreg::screenreg(l = vanilla$`Year_2004-05`)
cat("\14");texreg::screenreg(l = vanilla$`Year_2006-07`)
cat("\14");texreg::screenreg(l = vanilla$`Year_2008-09`)



rm(vanilla, vses, vpvl)

# ----------------------------------- #
















#-----------------------------------------------------------------------------#

basic <- formula(icews_bin ~ distance_bogota_km_ln + pop_sum_ln + terrain_ri_mean_m)
# Note - Bogota dummy is fine. Replace basic in m1 below to see
#        change in rho.

# This drives me crazy , M&G definitely swapped lambda and rho. rho is the spatial lag
# while lambda is spatial error param... typically.

# Then, once we filter out spatial error dependence using proper functional
# form, we can accurately estimate a spatial lag model.

m1  <- SpatialProbitFit(formula = fs$icews_bin,
                        data    = colombia_cs,
                        W       = nb.queen.mat,
                        DGP     = 'SAR',
                        method  = "conditional",
                        varcov  = 'varcov',
                        control = list(iW_CL = 6))

m2  <- SpatialProbitFit(formula = f2,
                        data    = colombia_cs,
                        W       = nb.queen.mat,
                        DGP     = 'SEM',
                        method  = "conditional",
                        varcov  = 'varcov',
                        control = list(iW_CL = 6))

m3  <- SpatialProbitFit(formula = f3,
                        data    = colombia_cs,
                        W       = nb.queen.mat,
                        DGP     = 'SEM',
                        method  = "conditional",
                        varcov  = 'varcov',
                        control = list(iW_CL = 6))



m4  <- SpatialProbitFit(formula = fs$icews_cinep_under,
                        data    = colombia_cs,
                        W       = nb.queen.mat,
                        DGP     = 'SEM',
                        method  = "conditional",
                        varcov  = 'varcov',
                        control = list(iW_CL = 6))

m5  <- SpatialProbitFit(formula = fs$ged_cinep_under,
                        data    = colombia_cs,
                        W       = nb.queen.mat,
                        DGP     = 'SEM',
                        method  = "conditional",
                        varcov  = 'varcov',
                        control = list(iW_CL = 6))










reported <- list("icews" = m1,
                 "ged"   = m2,
                 "cinep" = m3)
results  <- list()

results$coefs <- lapply(reported, function(x){c(x@beta, x@rho)})

results$ses   <- lapply(1:3, function(x){
  print(sprintf('Working on model: %s',x))
  se_robust(reported[[x]], robust = TRUE, hc = "HC1")})

results$pvals <- lapply(1:3, function(x){
  2 * (1 - pnorm(abs(results$coefs[[x]])/results$ses[[x]]))})

results$lliks <- lapply(reported, function(x){c(round(x@loglik,3))})

results$dfs <- lapply(1:3, function(x){
  data.frame("Coefs" = results$coefs[[x]],
             "SEs"   = results$ses[[x]],
             "Pvals" = results$pvals[[x]])
  })

results$dfs

v   <- c("ICEWS","GED","CINEP")
tst <- data.frame()

for(i in 1:3){
  tmp <- results$dfs[[i]] %>%
    rownames_to_column(., var = "variable") %>%
    dplyr::select(variable, everything())
  tmp$Model = v[i]

  tst <- bind_rows(tst, tmp)
}



#-----------------------------------------------------------------------------#






#-----------------------------------------------------------------------------#
# ANALYSIS                                                                ----
#-----------------------------------------------------------------------------#


#-----------------------------------------------------------------------------#
# SAVE                                                                    ----
#-----------------------------------------------------------------------------#
# save.image(file = "models_discrete.RData")
#rm(list = ls())
