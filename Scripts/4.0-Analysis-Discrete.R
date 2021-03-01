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
source("c:/users/logan/documents/r/r - simple-functions/reg_tbl.r")


#-----------------------------------------------------------------------------#
# MODEL SETUP                                                             ----
#-----------------------------------------------------------------------------#

fn  <- paste0("distance_bogota_km_ln + I(distance_bogota_km_ln^2) + ",
              "pop_sum_ln + I(pop_sum_ln^2) +",
              "area_km2_ln + I(area_km2_ln^2) +" ,
              # "bogota_dummy + "                     # This is producing some explosive standard errors. Assuming major optimizer problems for second deriv being masked with inclusion of this.
              # "log(terrain_ri_mean_m) + ",          # better for observed mods
              "terrain_ri_mean_m + ",                 # better for under mods (nonlinear distance emerges for icews)
              "forest_per + nl_mean")
dvs <- c("icews_bin", "ged_bin", "cinep_bin",
         "icews_cinep_under","ged_cinep_under")

fs  <- lapply(paste(dvs, fn, sep = " ~ "), as.formula)
names(fs) <- dvs


varnames <- str_split(fn, "\\+") %>% unlist
varnames <- lapply(varnames, function(x){
  tmp = str_trim(x)
  str_remove_all(tmp, pattern = "I|[()]")
}) %>% unlist
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

texreg::screenreg(ns_probits)

reg_tbl(model_list     = ns_probits,
        model_names    = dvs,
        variable_names = varnames)
# ----------------------------------- #


# ----------------------------------- #
# Years-Grouped
# ----------------------------------- #
      # try(suppressWarnings({rm(vanilla, vses, vpvl)}))
      #
      # yrs <- unique(colombia_yg$year_grouped)
      #
      # vanilla <- lapply(yrs, function(y){
      #   tmp <- colombia_yg %>% filter(year_grouped == y)
      #   lapply(fs, function(x){
      #     glm(formula = x,
      #         family  = binomial(link = "probit"),
      #         data    = tmp)
      #   })
      # })
      #
      # names(vanilla) <- paste0("Year_",yrs)
      #
      # # vses <- lapply(vanilla, function(y){
      # #   lapply(y, function(x){
      # #     sqrt(diag(sandwich::vcovHC(x, type = "HC1")))
      # #   })
      # # })
      # #
      # # vpval <- lapply(vanilla, function(y){
      # #   lapply(1:length(fs), function(x){
      # #     2 * (1 - pnorm(abs(y[[x]]$coefficients)/vses[[x]][1]))
      # #   })
      # # })
      #
      #
      # cat("\14");texreg::screenreg(l = vanilla$`Year_2002-03`)
      # cat("\14");texreg::screenreg(l = vanilla$`Year_2004-05`)
      # cat("\14");texreg::screenreg(l = vanilla$`Year_2006-07`)
      # cat("\14");texreg::screenreg(l = vanilla$`Year_2008-09`)
      #
      #
      #
      # rm(vanilla, vses, vpvl)
# ----------------------------------- #
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# SPATIAL PROBITS
#-----------------------------------------------------------------------------#

# ----------------------------------- #
# Spatial error probits (SEM)
# ----------------------------------- #
m1  <- SpatialProbitFit(formula = fs$icews_bin,
                        data    = colombia_cs,
                        W       = nb.queen.mat,
                        DGP     = 'SEM',
                        method  = "conditional",
                        varcov  = 'varcov',
                        control = list(iW_CL = 6))

m2  <- SpatialProbitFit(formula = fs$ged_bin,
                        data    = colombia_cs,
                        W       = nb.queen.mat,
                        DGP     = 'SEM',
                        method  = "conditional",
                        varcov  = 'varcov',
                        control = list(iW_CL = 6))

m3  <- SpatialProbitFit(formula = fs$cinep_bin,
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

# Organize models
sp_probits <- list(m1,m2,m3,m4,m5)
names(sp_probits) <- dvs

# Spatial probits - parameter extract
results       <- list()
results$coefs <- lapply(sp_probits, function(x){c(x@beta, x@rho)})

results$ses   <- lapply(1:length(dvs), function(x){
  print(sprintf('Working on model: %s',x))
  se_robust(sp_probits[[x]], robust = TRUE, hc = "HC1")})

# Organize values to markdown table (pandoc pipe format)
res <- list()
res[[1]] <- results$coefs
res[[2]] <- results$ses


reg_tbl(model_data     = res,
        model_names    = dvs,
        variable_names = c(varnames, "rho"))
# ----------------------------------- #










#-----------------------------------------------------------------------------#






#-----------------------------------------------------------------------------#
# ANALYSIS                                                                ----
#-----------------------------------------------------------------------------#


#-----------------------------------------------------------------------------#
# SAVE                                                                    ----
#-----------------------------------------------------------------------------#
# save.image(file = "data/models_discrete.RData")
#rm(list = ls())
