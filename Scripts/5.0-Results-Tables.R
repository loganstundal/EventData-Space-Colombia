#-----------------------------------------------------------------------------#
#
# Author:        Logan Stundal
# Date:          April 09, 2021
# Purpose:       5.0: Table Construction
#
#
# Copyright (c): Logan Stundal, 2021
# Email:         stund005@umn.edu
#
#-----------------------------------------------------------------------------#
#
# Notes:
#  Create tables for SPDE model results
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
library(kableExtra)

#---------------------------#
# Load data
#---------------------------#
load("Results/inla-mods.Rdata")

#---------------------------#
# Functions
#---------------------------#
qoi <- function(mod_list){
  # Takes an inla model list and returns a list containing quantities
  # of interest
  inla_betas <- lapply(mod_list, function(mod){
    round(mod$summary.fixed[,c("0.5quant","0.025quant","0.975quant")],3) %>%
      as.data.frame() %>%
      rename(median = `0.5quant`,
             lb     = `0.025quant`,
             ub     = `0.975quant`) %>%
      rownames_to_column(var = "variable")
  })

  inla_hyper <- lapply(mod_list, function(mod, round_digits = 3){
    spde_pars <- inla.spde2.result(inla = mod,
                                   name = "spatial.field",
                                   spde,do.transform = TRUE)

    # Kappa
    # Kappa    <- inla.emarginal(function(x) x, spde_pars$marginals.kappa[[1]])  # kappa (mean)
    Kappa    <- inla.qmarginal(0.50, spde_pars$marginals.kappa[[1]])         # kappa (median)
    Kappahpd <- inla.hpdmarginal(0.95, spde_pars$marginals.kappa[[1]])         # kappa (hpd 95%)

    # Sigma
    # Sigma    <- inla.emarginal(function(x) x, spde_pars$marginals.variance.nominal[[1]]) # variance (mean)
    Sigma    <- inla.qmarginal(0.50, spde_pars$marginals.variance.nominal[[1]])        # variance (median)
    Sigmahpd <- inla.hpdmarginal(0.95, spde_pars$marginals.variance.nominal[[1]])        # variance (hpd 95%)

    # Range
    # Range    <- inla.emarginal(function(x) x, spde_pars$marginals.range.nominal[[1]]) # range (mean)
    Range    <- inla.qmarginal(0.50, spde_pars$marginals.range.nominal[[1]])        # range (median)
    Rangehpd <- inla.hpdmarginal(0.95, spde_pars$marginals.range.nominal[[1]])        # range (hpd 95%)

    # Convert range to km (degrees = 2*pi*6371/360)
    Range    <- Range * 2*pi*6371/360
    Rangehpd <- Rangehpd * 2*pi*6371/360

    df <- rbind(cbind(Kappa, Kappahpd),
                cbind(Sigma, Sigmahpd),
                cbind(Range, Rangehpd)) %>%
      as.data.frame()

    colnames(df) <- c("median","lb","ub")
    rownames(df) <- 1:nrow(df)
    df$variable  <- c("Kappa","Sigma","Range")

    return(df)
  })

  inla_lliks <- lapply(mod_list, function(mod){
    mod$mlik[1]
  })

  return(list("betas" = inla_betas,
              "hyper" = inla_hyper,
              "lliks" = inla_lliks))
}
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# QOI                                                                     ----
#-----------------------------------------------------------------------------#
# Extract quantities-of-interest: regression coefficients and hyperparameters
res_vals <- sapply(yr_grp, function(x){
  qoi(inla_mods[[x]])
}, simplify = F)
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# TIDY PARAMS                                                             ----
#-----------------------------------------------------------------------------#
tidy_vals <- list()
for(yr in yr_grp){
  for(dv in dvs){
    bs <- res_vals[[yr]][["betas"]][[dv]]
    hy <- res_vals[[yr]][["hyper"]][[dv]]

    llik <- res_vals[[yr]][["lliks"]][[dv]]

    vl <- bind_rows(bs, hy)
    vl <- bind_cols(vl,
                    "model" = dv,
                    "years" = yr,
                    "lliks" = llik,
                    "n"     = "1116")

    id <- paste(dv, yr, sep = "_._")
    tidy_vals[[id]] <- vl
  }
};rm(yr, dv, bs, hy, llik, vl, id)

# Bind parameters to DF
tidy_vals <- bind_rows(tidy_vals)
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# MODEL TIDY PARAMS                                                       ----
#-----------------------------------------------------------------------------#
tab_vals <- tidy_vals %>%
  mutate(across(c(median, lb, ub, lliks),
                ~format(round(.x, 3), nsmall = 3))) %>%
  mutate(hpd = sprintf("[%s, %s]", lb, ub)) %>%
  select(variable, median, hpd, model, years) %>%
  pivot_longer(.,
               cols     = c(median, hpd),
               names_to = "type") %>%
  pivot_wider(.,
              id_cols = c(variable, type, years),
              names_from = model,
              values_from = value)

lliks_n <- tidy_vals %>%
  group_by(model, years) %>%
  summarize(lliks = lliks[1],
            n     = n[1],
            .groups = "keep") %>%
  ungroup() %>%
  mutate(across(c(lliks),
                ~format(round(.x, 3), nsmall = 3))) %>%
  select(lliks, n, model, years) %>%
  pivot_longer(.,
               cols = c(lliks, n),
               names_to = "variable") %>%
  pivot_wider(.,
              id_cols = c(variable, years),
              names_from = model,
              values_from = value) %>%
  mutate(type = NA)


tab_vals <- bind_rows(tab_vals, lliks_n) %>%
  dplyr::select(-type) %>%
  mutate(variable = case_when(variable == "intercept" ~ "Intercept",
                              variable == "dist"      ~ "Dist. Bogota, km (log)",
                              variable == "pop"       ~ "Population (log)",
                              variable == "tri"       ~ "TRI",
                              variable == "lliks"     ~ "LogLik",
                              variable == "n"         ~ "N",
                              TRUE ~ variable))

rm(lliks_n)
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# SAVE                                                                    ----
#-----------------------------------------------------------------------------#
save(tab_vals, tidy_vals, file = "Results/Tables/tidy-mods.Rdata")
#rm(list = ls())
#-----------------------------------------------------------------------------#






