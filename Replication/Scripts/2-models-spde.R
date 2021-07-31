#-----------------------------------------------------------------------------#
#
# Author:        Logan Stundal
# Date:          July 26, 2021
# Purpose:       Replicates main SPDE models.
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
library(dplyr)
library(tibble)
library(INLA)

# To install inla version used in analysis:
# install.packages("https://inla.r-inla-download.org/R/stable/src/contrib/INLA_21.02.23.tar.gz",
#                  repos  = NULL,
#                  method = "libcurl")
#---------------------------#


#---------------------------#
# Load data
#---------------------------#
load("Data/farc_events.Rdata")
#---------------------------#

#---------------------------#
# Functions
#---------------------------#
qoi <- function(model_list, centrality = "mean"){
  # ----------------------------------- #
  # function description
  # ----------------------------------- #
  # qoi() [quantity of interest] takes an INLA model list and returns a list
  # containing quantities of interest: posterior parameter estimates as well as
  # credibility intervals on included regressors and parameters for the GMRF
  # (range, kappa, and sigma).
  # To extract these quantities for one model, input your model as a named list:
  # result <- qoi(model_list = list("my_model" = estimated_model))
  # ----------------------------------- #


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


matern <- function(lambda, kappa, dist){
  # Matern covariance function, used with matern_data()
  2^(1-lambda)/gamma(lambda) * (kappa*dist)^lambda* besselK(x=dist*kappa, nu=lambda)
}


matern_data <- function(model,
                        max_distance = NULL){
  # ----------------------------------- #
  # function description
  # ----------------------------------- #
  # matern_data takes an inla model where random effects estimated in a gaussian field are assumed
  # to follow a continuous decay process fit with a stochastic partial differential equation with a
  # matern covariance solution. This function extract model parameters to construct decay estimates
  # up to a max_distance argument (km) set by the user.
  # ----------------------------------- #


  # ----------------------------------- #
  # Extract spatial field and parameters from model input
  # ----------------------------------- #
  # Spatial parameters with nominal scale (time is aggregated if present)
  spde.resfinal <- inla.spde2.result(inla = model,
                                     name = "spatial.field",
                                     spde,do.transform = TRUE) # do.transform put in correct scale

  # Kappa / computed using Blangiardo & Cameletti (2015)
  Kappa    <-inla.emarginal(function(x) x, spde.resfinal$marginals.kappa[[1]]) # kappa (mean)
  Kappahpd <-inla.hpdmarginal(0.95, spde.resfinal$marginals.kappa[[1]])        # kappa (hpd 95%)

  # Variance of the random field
  variance    <- inla.emarginal(function(x) x, spde.resfinal$marginals.variance.nominal[[1]]) # variance (mean)
  variancehpd <- inla.hpdmarginal(0.95, spde.resfinal$marginals.variance.nominal[[1]])        # variance (hpd 95%)

  # Range + degree conversion
  range    <- inla.emarginal(function(x) x, spde.resfinal$marginals.range.nominal[[1]]) # range (mean)
  rangehpd <- inla.hpdmarginal(0.95, spde.resfinal$marginals.range.nominal[[1]])        # range (hpd 95%)
  deg      <- 2*pi*6371/360
  # ----------------------------------- #


  # ----------------------------------- #
  # Build plot data frame
  # ----------------------------------- #
  # Additional parameters:
  lambda <- 1

  if(is.null(max_distance)){
    dist.x <- seq(from = 0.01, to = rangehpd[,"high"], length.out = 100)
  } else{
    # Convert user-input kilometers "max_distance" to decimal degrees for
    # appropriate mapping to estimation space.
    max_distance <- max_distance / deg

    dist.x <- seq(from = 0.01, to = max_distance, length.out = 100)
  }

  plt_dat <- data.frame(
    "x"  = dist.x,
    "lb" = matern(lambda = lambda,
                  kappa  = Kappahpd[,"low"],
                  dist   = dist.x),
    "y"  = matern(lambda = lambda,
                  kappa  = Kappa,
                  dist   = dist.x),
    "ub" = matern(lambda = lambda,
                  kappa  = Kappahpd[,"high"],
                  dist   = dist.x))
  # ----------------------------------- #


  # ----------------------------------- #
  # Return statement
  # ----------------------------------- #
  res <- list("df"    = plt_dat,
              "range" = range)

  return(res)
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

# Construct a mesh projector object to use with projecting field estimates:
mesh_projector <- inla.mesh.projector(mesh, projection = "longlat",
                                      dims = c(180,180))

# Collect mesh elements in a list
mesh_list <- list("mesh"      = mesh,
                  "nv"        = mesh$n,
                  "A"         = A,
                  "projector" = mesh_projector)
rm(mesh, nv, A, border, colcoord, tmp, mesh_projector)
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

# INLA Stacks (INLA models take data in a stack format)
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
# Extract quantities of interest for tables and figures 3 and 4
#-----------------------------------------------------------------------------#
# ----------------------------------- #
# Extract quantities of interest
# ----------------------------------- #
parameter_data <- sapply(yr_grp, function(x){
  qoi(inla_mods[[x]], centrality = "median")
}, simplify = F)

# Replicates estimates used to produce main article Figures 3 and 4 and
# associated tables A.5-A.8 located in Appendix
# e.g.,
# Appendix table A.5:
# parameter_data$`2002-2009`
# ----------------------------------- #
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# Extract model quantities to map posterior Gaussian fields and range
#-----------------------------------------------------------------------------#
# ----------------------------------- #
# Gaussian Random Field Estimates
# ----------------------------------- #
# The following collects data to construct posterior field maps in
# replication-a8.R corresponding to Appendix Figure A8

# Note - The appendix provides maps for full cross-section (2002-2009) results
# only. Change `2002-2009` on first line for other years
# nb : [1-3] corresponds to observed FARC event, not underreporting models
field_data <- sapply(inla_mods$`2002-2009`[1:3], function(mod){
  sapply(c("mean", "sd"), function(x){
    inla.mesh.project(projector = mesh_list$projector,
                      field     = mod$summary.random$spatial.field[[x]])
  }, simplify = FALSE)
}, simplify = FALSE)

names(field_data) <- c("ICEWS","GED","CINEP")
# ----------------------------------- #


# ----------------------------------- #
# Spatial Error Range Estimates
# ----------------------------------- #
# The following collects data to construct posterior range estimates
# replication-a9.R corresponding to Appendix Figure A9

# Note - The appendix provides spatial error correlation decay estimates for
# full cross-section (2002-2009) results only. Change `2002-2009` on first
# line for other years
# nb : [1-3] corresponds to observed FARC event, not underreporting models
range_data <- sapply(inla_mods$`2002-2009`[1:3], function(mod){
  matern_data(model = mod, max_distance = 500)
  # max distance 500km
}, simplify = FALSE)

names(range_data) <- c("ICEWS","GED","CINEP")
# ----------------------------------- #


# ----------------------------------- #
# Predicted outctomes - for ROCs
# ----------------------------------- #
# Extract fit predicted values for observed FARC and Underreporting
# ICEWS and GED models
pred_data <- sapply(inla_mods, function(yr){
  sapply(yr[c(1,2,4,5)], function(mod){
    pnorm(mod$summary.linear.predictor[1:1116, "mean"])
  }, simplify = FALSE)
}, simplify = FALSE)
# ----------------------------------- #
#-----------------------------------------------------------------------------#


#-----------------------------------------------------------------------------#
# COLOR VECTOR LIST FOR CONSISTENT PLOT COLORING                          ----
#-----------------------------------------------------------------------------#
# scales::show_col(c("#7CAE00","#F8766D","#00BFC4"))
model_colors        <- c("#7CAE00", "#F8766D", "#00BFC4")
names(model_colors) <- c("CINEP","ICEWS","GED")
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# SAVE                                                                    ----
#-----------------------------------------------------------------------------#
# INLA-SPDE model parameter data for:
#   - table formatting script - `6-tables.R` and
#   - figures (3 & 4) script  - `4-figures-coefplots.R`
save(parameter_data, yr_grp, dvs, model_colors,
     file = "Results/Replication-Estimates/parameter-data.Rdata")

# Field data for:
#   - Gaussian field mapping script - `7-figures-field_range-estimates.R`
save(field_data, mesh_list, model_colors,
     file = "Results/Replication-Estimates/field-data.Rdata")

# Range data for:
#   - SPDE error correlation range - `7-figures-field_range-estimates.R`
save(range_data, model_colors,
     file = "Results/Replication-Estimates/range-data.Rdata")

# Predicted outcome data for:
#   - ROCs - `5-figures-ROCs.R`
save(pred_data, model_colors,
     file = "Results/Replication-Estimates/pred-data.Rdata")



# Published models [folder further compressed after save]
save(inla_mods, spde, mesh_list, stacks, dat, qoi,
     compress = "xz",
     file     = "Results/Published-Models/published-models-spde.Rdata")
#-----------------------------------------------------------------------------#

rm(list=ls())

