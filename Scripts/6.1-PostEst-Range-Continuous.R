#-----------------------------------------------------------------------------#
#
# Author:        Logan Stundal
# Date:          April 25, 2021
# Purpose:       Note
#
#
# Copyright (c): Logan Stundal, 2021
# Email:         stund005@umn.edu
#
#-----------------------------------------------------------------------------#
#
# Notes:
#  Code amended from Python's Boko Haram paper in 2017. Replication files at:
#  C:/Users/logan/GoogleDrive/UMN/RESEARCH/DATA/REPLICATION FILES - OTHERS/
#   Python_2017_BokoHaram/STbin_total_graphics.R
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
library(magrittr)
# library(sf)
library(INLA)

#---------------------------#
# Load data
#---------------------------#
load("Results/inla-mods.Rdata")
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# LOCAL FUNCTIONS                                                         ----
#-----------------------------------------------------------------------------#
matern <- function(lambda, kappa, dist){
  2^(1-lambda)/gamma(lambda) * (kappa*dist)^lambda* besselK(x=dist*kappa, nu=lambda)
}

matern_data <- function(model,
                        max_distance = NULL){

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


matern_plot <- function(data,
                        range,
                        breaks = NULL){

  if(is.null(breaks)){
    deg    <- 2*pi*6371/360
    breaks <- c(0, 10, 50, 100, 150) / deg
  }

  ggplot(data = data, aes(x=x)) +
    geom_ribbon(aes(ymin = lb, ymax = ub, fill = "ICEWS"), alpha = 0.3) +
    geom_line(aes(y = y)) +

    geom_vline(aes(xintercept = range), linetype = "dashed", size = 0.1, color = "gray50") +
    geom_hline(aes(yintercept = 0.1),       linetype = "dashed", size = 0.1, color = "gray50") +

    scale_x_continuous(name   = "Distance [km]",
                       limits = c(0,max(data$x)),
                       breaks = breaks,
                       labels = function(x){x * deg},
                       expand = c(0,0)) +

    scale_y_continuous(name   = "Matern covariance function",
                       limits = c(0,1),
                       breaks = c(seq(0, 1, 0.25), 0.1)) +

    theme_minimal() +
    theme(legend.position  = "bottom",
          legend.direction = "horizontal",
          legend.title     = element_blank(),
          panel.grid       = element_blank(),
          panel.background = element_rect(fill = NA, color = "black", size = 0.1))
}
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# PLOTS SEPARATE                                                          ----
#-----------------------------------------------------------------------------#
icews     <- matern_data(model = inla_mods$`2002-2009`$icews_bin)
icews_plt <- matern_plot(data = icews$df, range = icews$range)

ged       <- matern_data(model = inla_mods$`2002-2009`$ged_bin)
ged_plt   <- matern_plot(data = ged$df, range = ged$range)

cinep     <- matern_data(model = inla_mods$`2002-2009`$cinep_bin)
cinep_plt <- matern_plot(data = cinep$df, range = cinep$range)
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# PLOTS COMBINED                                                          ----
#-----------------------------------------------------------------------------#
icews     <- matern_data(model = inla_mods$`2002-2009`$icews_bin, max_distance = 500/deg)
ged       <- matern_data(model = inla_mods$`2002-2009`$ged_bin,   max_distance = 500/deg)
cinep     <- matern_data(model = inla_mods$`2002-2009`$cinep_bin, max_distance = 500/deg)

plt_dat <- bind_rows(icews$df %>% mutate(model = "ICEWS", range = icews$range),
                     ged$df   %>% mutate(model = "GED",   range = ged$range),
                     cinep$df %>% mutate(model = "CINEP", range = cinep$range))

deg    <- 2*pi*6371/360
range(plt_dat$x * deg)

breaks <- c(0, 50, 100, 200, 300, 400, 500) / deg

final <- ggplot(data = plt_dat, aes(x=x)) +
  geom_ribbon(aes(ymin = lb, ymax = ub, fill = model), alpha = 0.3) +
  geom_line(aes(y = y), size = 0.25) +

  geom_vline(aes(xintercept = range), linetype = "dashed", size = 0.2, color = "gray50") +
  geom_hline(aes(yintercept = 0.1),   linetype = "dashed", size = 0.2, color = "gray50") +

  scale_x_continuous(name   = "Distance [km]",
                     limits = c(0,max(breaks)),
                     breaks = breaks,
                     labels = function(x){x * deg},
                     expand = c(0,0)) +

  scale_y_continuous(name   = "Matern covariance function",
                     limits = c(0,1),
                     breaks = c(seq(0, 1, 0.25), 0.1)) +

  # scale_fill_viridis_d() +
  scale_fill_manual(values = viridis::viridis(n = 8)[c(1,3,5)]) +

  theme_minimal() +
  theme(legend.position  = "bottom",
        legend.direction = "horizontal",
        legend.title     = element_blank(),
        panel.grid       = element_blank(),
        panel.background = element_rect(fill = NA, color = "black", size = 0.1),
        strip.background = element_rect(fill = "gray95", color = "black", size = 0.1),
        plot.margin      = unit(c(1,3,1,1), "mm")) +
  facet_wrap(~model, nrow = 3) +
  labs(title    = "Spatial correlation decay",
       subtitle = "SPDE - Observed FARC Events")
# final

ggsave(plot     = final,
       filename = "Results/Plots/matern.png",
       width    = 4.5,
       height   = 8.0,
       units    = "in",
       dpi      = 350)
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# SAVE                                                                    ----
#-----------------------------------------------------------------------------#
#save.image()
#rm(list = ls())
#-----------------------------------------------------------------------------#
