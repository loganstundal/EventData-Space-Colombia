#-----------------------------------------------------------------------------#
#
# Author:        Logan Stundal
# Date:          February 27, 2021
# Purpose:       9.0: Analysis - Discrete
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
library(magrittr)
library(sf)
library(ProbitSpatial)
library(sandwich)

#---------------------------#
# Load data
#---------------------------#
load("data/data_variables.RData")
rm(colombia_yg, colombia_pn)

#---------------------------#
# Functions
#---------------------------#
source("Scripts/Functions/se_robust.R")

local_table <- function(coefs   = NULL,
                        st_errs = NULL,
                        lliks   = NULL){

  # ----------------------------------- #
  # Setup
  var_names <- names(coefs[[1]])
  dvs       <- names(coefs)
  # ----------------------------------- #


  tidy_vals <- sapply(dvs, function(dv){

    res <- coefs[[dv]]
    ses <- st_errs[[dv]]

    lb <- res - (qnorm(0.975) * ses)
    ub <- res + (qnorm(0.975) * ses)

    res <- data.frame("var" = var_names,
                      "est" = res,
                      "lb"  = lb,
                      "ub"  = ub,
                      "model" = dv) %>%
      mutate(across(c(est, lb, ub), ~format(round(.x, 3), nsmall = 3))) %>%
      mutate(hpd = sprintf("[%s, %s]", lb, ub))

    res %<>% dplyr::select(var, est, hpd, model)

    post <- data.frame("var" = c("loglik", "n"),
                       "est" = c(format(round(lliks[[dv]], 3), nsmall = 3),
                                 "1116"),
                       "model" = rep(dv, 2))

    res <- bind_rows(res, post)
  }, simplify = F)


  tidy_vals %<>%
    bind_rows() %>%
    pivot_longer(cols = c(est, hpd),
                 values_to = "vals",
                 names_to  = "ids") %>%
    pivot_wider(id_cols = c(var, ids),
                names_from = model,
                values_from = vals) %>%
    drop_na()

  return(tidy_vals)
}
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# GROUPED YEAR DATA TIDY                                                  ----
#-----------------------------------------------------------------------------#

# ----------------------------------- #
# Organize data model estimation
# ----------------------------------- #
colombia <- colombia_cs %>%
  st_drop_geometry() %>%
  mutate(icews_cinep_under = case_when(icews_bin == 0 & cinep_bin == 1 ~ 1, TRUE ~ 0),
         icews_cinep_over  = case_when(icews_bin == 1 & cinep_bin == 0 ~ 1, TRUE ~ 0),
         icews_cinep_bias  = case_when(icews_bin != cinep_bin ~ 1, TRUE ~ 0),

         ged_cinep_under   = case_when(ged_bin == 0 & cinep_bin == 1 ~ 1, TRUE ~ 0),
         ged_cinep_over    = case_when(ged_bin == 1 & cinep_bin == 0 ~ 1, TRUE ~ 0),
         ged_cinep_bias    = case_when(ged_bin != cinep_bin ~ 1, TRUE ~ 0)) %>%
  mutate(yr_grp = as_factor("2002-2009")) %>%
  dplyr::select(Department, Municipality, yr_grp, ID_Mun, cinep, icews, ged,
                distance_bogota_km_ln, terrain_ri_mean_m, centroid_mun_long,
                centroid_mun_lat, pop_sum_ln, cinep_bin, icews_bin, ged_bin,
                icews_cinep_under, icews_cinep_over, icews_cinep_bias,
                ged_cinep_under, ged_cinep_over, ged_cinep_bias)
# ----------------------------------- #
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# PROBIT MODELS SETUP                                                     ----
#-----------------------------------------------------------------------------#

# ----------------------------------- #
# Spatial Weights - Row-Standardized
# ----------------------------------- #
# Create a spatial polygons dataframe to pass to 'poly2nb':
dat_shp <- colombia_cs %>%
  dplyr::select(ID_Mun) %>%
  as_Spatial(., IDs = ID_Mun)

# QUEEN - Spatial neighbors, matrix and lists:
nb.r.queen   <- spdep::poly2nb(pl        = dat_shp,
                               row.names = dat_shp$ID_Mun,
                               queen     = TRUE)
nb.queen.mat <- spdep::nb2mat(neighbours = nb.r.queen,
                              style      = "W")
colnames(nb.queen.mat) <- rownames(nb.queen.mat)

rm(colombia_cs, dat_shp, nb.r.queen)
# ----------------------------------- #



# ----------------------------------- #
# MODEL setup
# ----------------------------------- #
# Formula
formula <- . ~ dist + pop + tri

# DVs
dvs <- c(
  # Observed FARC Events
  "icews_bin", "ged_bin", "cinep_bin",

  # Under-reporting DVs
  "icews_cinep_under","ged_cinep_under"
)
# ----------------------------------- #
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# VANILLA PROBITS                                                         ----
#-----------------------------------------------------------------------------#
# Estimate models
probit_ns <- sapply(dvs, function(dv){
  cat(sprintf('Working on model %s\n', dv))

  dat_tmp <- colombia %>%
    rename(dist = distance_bogota_km_ln,
           pop  = pop_sum_ln,
           tri  = terrain_ri_mean_m)

  mod <- glm(formula = update(formula, paste(dv, " ~ . ")),
             data    = dat_tmp,
             family  = binomial(link = "probit"))
  return(mod)
}, simplify = F)

# Extract model elements
coefs   <- lapply(probit_ns, function(x){coef(x)})
st_errs <- lapply(probit_ns, function(x){
  vcovHC(x = x, type = "HC1") %>% diag %>% sqrt %>% unlist %>% as.vector
})
lliks   <- lapply(probit_ns, function(x){logLik(x)})

# Tidy results to clean table format
probit_ns <- local_table(coefs   = coefs,
                         st_errs = st_errs,
                         lliks   = lliks)

rm(coefs, st_errs, lliks)

# Consistent variable naming:
probit_ns %<>% mutate(var = case_when(var == "(Intercept)" ~ "Intercept",
                                      var == "dist" ~ "Dist. Bogota, km (log)",
                                      var == "pop" ~ "Population (log)",
                                      var == "tri" ~ "TRI",
                                      var == "loglik" ~ "LogLik",
                                      var == "n" ~ "N"))
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# SPATIAL PROBITS                                                         ----
#-----------------------------------------------------------------------------#
# Estimate models
probit_sp <- sapply(dvs, function(dv){
  cat(sprintf('Working on model %s\n', dv))

  dat_tmp <- colombia %>%
    rename(dist = distance_bogota_km_ln,
           pop  = pop_sum_ln,
           tri  = terrain_ri_mean_m)

  mod <- SpatialProbitFit(formula = update(formula, paste(dv, " ~ . ")),
                          data    = dat_tmp,
                          W       = nb.queen.mat,
                          DGP     = 'SEM',
                          method  = "conditional",
                          varcov  = 'varcov',
                          control = list(iW_CL = 6))
  return(mod)
}, simplify = F)

# Extract model elements
coefs   <- lapply(probit_sp, function(x){coef(x)})
st_errs <- sapply(names(probit_sp), function(x){
  cat(sprintf("Working on: %s", x))

  mod <- probit_sp[[x]]
  ses <- se_robust(mod)
  return(ses)
},simplify = F)
lliks   <- lapply(probit_sp, function(x){x@loglik})

# Save spems for post-estimation qois
spem <- probit_sp
save(spem, colombia, file = "Results/discrete-spem-models.Rdata")


# Tidy results to clean table format
probit_sp <- local_table(coefs   = coefs,
                         st_errs = st_errs,
                         lliks   = lliks)

rm(coefs, st_errs, lliks)

# Consistent variable naming:
probit_sp %<>% mutate(var = case_when(var == "(Intercept)" ~ "Intercept",
                                      var == "dist" ~ "Dist. Bogota, km (log)",
                                      var == "pop" ~ "Population (log)",
                                      var == "tri" ~ "TRI",
                                      var == "rho" ~ "$\\lambda$",
                                      var == "loglik" ~ "LogLik",
                                      var == "n" ~ "N"))
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# CLEAN-UP                                                                ----
#-----------------------------------------------------------------------------#
rm(nb.queen.mat, dvs, formula, local_table, se_robust, colombia)
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# SAVE MODELS                                                             ----
#-----------------------------------------------------------------------------#
save.image("Results/discrete-mods.RData")
#-----------------------------------------------------------------------------#

