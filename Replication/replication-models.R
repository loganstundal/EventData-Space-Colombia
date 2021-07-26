#-----------------------------------------------------------------------------#
#
# Author:        Logan Stundal
# Date:          July 26, 2021
# Purpose:       Replication materials - manuscript main tables
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

#---------------------------#
# Load required packages
#---------------------------#
library(tidyverse)
library(magrittr)
library(sf)
library(INLA)
library(kableExtra)
#---------------------------#

#---------------------------#
# Set working directory
#---------------------------#
# setwd()
#---------------------------#

#---------------------------#
# Load data
#---------------------------#
load("replication/data.Rdata")
#---------------------------#

#---------------------------#
# Functions
#---------------------------#
qoi <- function(model_list, centrality = "mean"){
  # Takes an inla model list and returns a list containing quantities
  # of interest
  # To extract quantites of interest for one model, enter as a named list, e.g.;
  # result <- qoi(model_list = list("my_model" = estimated_model))

  # ----------------------------------- #
  # Extract partials from structural model
  # ----------------------------------- #
  inla_betas <- lapply(model_list, function(mod){
    tmp <- round(mod$summary.fixed[,c(ifelse(centrality == "mean", "mean", "0.5quant"),
                                      "0.025quant","0.975quant")],3) %>%
      as.data.frame()

    if(centrality == "mean"){
      tmp %<>% rename(mean = `mean`,
                      lb   = `0.025quant`,
                      ub   = `0.975quant`) %>%
        rownames_to_column(var = "variable")
    } else if(centrality == "median"){
      tmp %<>% rename(median = `0.5quant`,
                      lb     = `0.025quant`,
                      ub     = `0.975quant`) %>%
        rownames_to_column(var = "variable")
    } else{
      stop("Centrality parameter must be one of: 'median' or 'mean'.")
    }
  })
  # ----------------------------------- #

  # ----------------------------------- #
  # Extract hyper-parameters
  # ----------------------------------- #
  inla_hyper <- lapply(model_list, function(mod, round_digits = 3){
    spde_pars <- inla.spde2.result(inla = mod,
                                   name = "spatial.field",
                                   spde,do.transform = TRUE)
    # ----------------------------------- #

    # ----------------------------------- #
    # Tidy hyper-parameter centrality measures
    # ----------------------------------- #
    if(centrality == "median"){
      Kappa    <- inla.qmarginal(0.50, spde_pars$marginals.kappa[[1]])                     # kappa (median)
      Sigma    <- inla.qmarginal(0.50, spde_pars$marginals.variance.nominal[[1]])          # variance (median)
      Range    <- inla.qmarginal(0.50, spde_pars$marginals.range.nominal[[1]])             # range (median)
    } else if(centrality == "mean"){
      Kappa    <- inla.emarginal(function(x) x, spde_pars$marginals.kappa[[1]])            # kappa (mean)
      Sigma    <- inla.emarginal(function(x) x, spde_pars$marginals.variance.nominal[[1]]) # variance (mean)
      Range    <- inla.emarginal(function(x) x, spde_pars$marginals.range.nominal[[1]])    # range (mean)
    } else{
      stop("Centrality parameter must be one of: 'median' or 'mean'.")
    }
    # ----------------------------------- #

    # ----------------------------------- #
    # Extract HPDs
    # ----------------------------------- #
    Kappahpd <- inla.hpdmarginal(0.95, spde_pars$marginals.kappa[[1]])            # kappa (hpd 95%)
    Sigmahpd <- inla.hpdmarginal(0.95, spde_pars$marginals.variance.nominal[[1]]) # variance (hpd 95%)
    Rangehpd <- inla.hpdmarginal(0.95, spde_pars$marginals.range.nominal[[1]])    # range (hpd 95%)
    # ----------------------------------- #

    # ----------------------------------- #
    # Convert range to km (degrees = 2*pi*6371/360)
    # ----------------------------------- #
    Range    <- Range * 2*pi*6371/360
    Rangehpd <- Rangehpd * 2*pi*6371/360
    # ----------------------------------- #

    # ----------------------------------- #
    # Tidy up return object
    # ----------------------------------- #
    df <- rbind(cbind(Kappa, Kappahpd),
                cbind(Sigma, Sigmahpd),
                cbind(Range, Rangehpd)) %>%
      as.data.frame()

    colnames(df) <- c(centrality,"lb","ub")
    rownames(df) <- 1:nrow(df)
    df$variable  <- c("Kappa","Sigma","Range")
    df <- df[c("variable",centrality,"lb","ub")]
    # ----------------------------------- #
    return(df)
  })

  # ----------------------------------- #
  # Model log likelihoods
  # ----------------------------------- #
  inla_lliks <- lapply(model_list, function(mod){
    mod$mlik[1]
  })
  # ----------------------------------- #

  # ----------------------------------- #
  # Return statement
  # ----------------------------------- #
  return(list("betas" = inla_betas,
              "hyper" = inla_hyper,
              "lliks" = inla_lliks))
  # ----------------------------------- #
}
#---------------------------#
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# INLA SETUP
#-----------------------------------------------------------------------------#
# ----------------------------------- #
# Mesh setup - units are the same for all years so only need to create once
# ----------------------------------- #
tmp <- dat$`2002-2004`

# Coordinates for mesh - key for prior construction in SPDE models.
colcoord <- cbind(tmp$centroid_mun_long,
                  tmp$centroid_mun_lat)
border   <- inla.mesh.segment(colcoord)
mesh     <- inla.mesh.2d(loc.domain = border$loc, max.edge = c(1.6))
nv       <- mesh$n
A        <- inla.spde.make.A(mesh = mesh,
                             loc  = as.matrix(cbind(tmp$centroid_mun_long,
                                                    tmp$centroid_mun_lat)))
mesh_list <- list("mesh" = mesh,
                  "nv"   = mesh$n,
                  "A"    = A)
rm(mesh, nv, A, border, colcoord, tmp)
# ----------------------------------- #


# ----------------------------------- #
# SPDE specification
# ----------------------------------- #
spde <- inla.spde2.matern(mesh_list$mesh, alpha = 2)
# ----------------------------------- #


# ----------------------------------- #
# MODEL setup
# ----------------------------------- #
# Formula
formula <- y ~ -1 + intercept + dist + pop + tri + f(spatial.field, model=spde)

# DVs
dvs <- c(
  # Observed FARC Events
  "icews_bin", "ged_bin", "cinep_bin",

  # Under-reporting DVs
  "icews_cinep_under","ged_cinep_under"
)

# INLA Stacks (INLA model take data in a stack format)
stacks <- sapply(yr_grp, function(x){

  tmp <- dat[[x]]

  tmp_stack <- sapply(dvs, function(x){
    inla.stack(data    = list(y = tmp[[x]]),
               A       = list(mesh_list$A,1,1,1),
               effects = list(c(list(intercept = rep(1,mesh_list$nv)),
                                inla.spde.make.index("spatial.field", spde$n.spde)),
                              dist = tmp$distance_bogota_km_ln,
                              pop  = tmp$pop_sum_ln,
                              tri  = tmp$terrain_ri_mean_m),
               tag='spde')
  }, simplify = F)
}, simplify = F)

# For additional help on this model, see:
# m <- inla.models()
# m$latent$spde2$doc
# m$latent$spde2$pdf
# inla.doc("matern2d")
# ----------------------------------- #
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# INLA MODEL ESTIMATION                                                   ----
#-----------------------------------------------------------------------------#
# ----------------------------------- #
# Notes
# ----------------------------------- #
# - The following block iterates through the year groups and estimates
#   all the INLA models storing them in a list. This is for convenience.
#   Below, commented out, there is code illustrating how to estimate a single
#   model.
# - These models replicate Figures 3 and 4 in the main article and associated
#   Tables A.5 - A.8 in the supplemental appendix.
# - summary(model) prints coefficient estimates using posterior means. We present
#   posterior medians in the manuscript.
# ----------------------------------- #


# ----------------------------------- #
# Estimate all INLA models
# ----------------------------------- #
inla_mods <- sapply(yr_grp, function(yr){
  cat("\14")
  sapply(dvs, function(dv){
    cat(sprintf('Working on model %s-%s\n', yr, dv))

    inla(formula           = formula,
         data              = inla.stack.data(stacks[[yr]][[dv]],
                                             spde = spde),
         family            = 'binomial',
         control.family    = list(link    = 'probit'),
         control.predictor = list(A       = inla.stack.A(stacks[[yr]][[dv]]),
                                  compute = TRUE,
                                  link    = 1),
         control.compute   = list(waic    = TRUE,
                                  config  = TRUE),
         control.fixed     = list(correlation.matrix=TRUE))
  }, simplify = F)
}, simplify = F)
# ----------------------------------- #


# ----------------------------------- #
# One INLA model example
# ----------------------------------- #
# One model [ICEWS, 2002-2009 cross-section]
# one_model <- inla(formula           = formula,
#                   data              = inla.stack.data(stacks$`2002-2009`$icews_bin,
#                                                       spde = spde),
#                   family            = 'binomial',
#                   control.family    = list(link    = 'probit'),
#                   control.predictor = list(A       = inla.stack.A(stacks$`2002-2009`$icews_bin),
#                                            compute = TRUE,
#                                            link    = 1),
#                   control.compute   = list(waic    = TRUE,
#                                            config  = TRUE),
#                   control.fixed     = list(correlation.matrix=TRUE))
# summary(one_model) # Note - presents posterior density means
#
# qoi takes a list, so the following returns reported results for the
# cross-section 2002-2009 ICEWS model
# qoi(model_list = list("ICEWS" = one_model), centrality = "median")
# ----------------------------------- #
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# Extract quantities of interest for tables and figures
#-----------------------------------------------------------------------------#
# ----------------------------------- #
# Extract quantities of interest
# ----------------------------------- #
results_vals <- sapply(yr_grp, function(x){
  qoi(inla_mods[[x]], centrality = "median")
}, simplify = F)
# ----------------------------------- #


# ----------------------------------- #
# Save results for table formatting script
# ----------------------------------- #
# save(results_vals, file = "results-tables.Rdata")
# ----------------------------------- #
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# Extract model quantities to map posterior Gaussian fields
#-----------------------------------------------------------------------------#

# These estimates are used to construct posterior field maps and spatial
# decay plots in Appendix figures A.8 and A.9 respectively.

# ----------------------------------- #
# Save
# ----------------------------------- #
save(results_fields, file = "results-fields.Rdata")
# ----------------------------------- #
#-----------------------------------------------------------------------------#


#-----------------------------------------------------------------------------#
# COLOR VECTOR LIST FOR CONSISTENT PLOT COLORING                          ----
#-----------------------------------------------------------------------------#
scales::show_col(viridis::viridis(n = 5)[c(1:3)])
model_colors <- viridis::viridis(n = 5)[c(1:3)]
names(model_colors) <- c("CINEP","ICEWS","GED")
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# SAVE                                                                    ----
#-----------------------------------------------------------------------------#
        # save(inla_mods, dat, spde, dvs, yr_grp, model_colors,
        #      file = "Replication/inla-mods.RData")
        # rm(list=ls())
#-----------------------------------------------------------------------------#


