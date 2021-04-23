#-----------------------------------------------------------------------------#
#
# Author:        Logan Stundal
# Date:          April 08, 2021
# Purpose:       4.1: SPDE - Continuous models
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
library(INLA)

#---------------------------#
# Load data
#---------------------------#
load("data/data_variables.RData")
rm(colombia_yg)

#---------------------------#
# Functions
#---------------------------#
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# GROUPED YEAR DATA TIDY                                                  ----
#-----------------------------------------------------------------------------#

# ----------------------------------- #
# Model data tidy
# ----------------------------------- #
colombia <- colombia_pn %>%
  as.data.frame() %>%
  mutate(yr_grp = case_when(year %in% 2002:2004 ~ "2002-2004",
                            year %in% 2005:2007 ~ "2005-2007",
                            year %in% 2008:2009 ~ "2008-2009")) %>%
  group_by(Department, Municipality, yr_grp) %>%
  summarize(across(c(cinep, icews, ged), sum),
            across(c(distance_bogota_km_ln, terrain_ri_mean_m,
                     centroid_mun_long, centroid_mun_lat), ~.x[1]),
            pop_sum_ln = mean(pop_sum_ln),
            .groups = "keep") %>%
  ungroup() %>%
  mutate(across(c(cinep, icews, ged), ~case_when(.x > 0 ~ 1, TRUE ~ 0), .names = "{col}_bin")) %>%
  mutate(icews_cinep_under = case_when(icews_bin == 0 & cinep_bin == 1 ~ 1, TRUE ~ 0),
         icews_cinep_bias  = case_when(icews_bin != cinep_bin ~ 1, TRUE ~ 0),

         ged_cinep_under   = case_when(ged_bin == 0 & cinep_bin == 1 ~ 1, TRUE ~ 0),
         ged_cinep_bias    = case_when(ged_bin != cinep_bin ~ 1, TRUE ~ 0))

rm(colombia_pn)
# ----------------------------------- #


# ----------------------------------- #
# Bind cross section to "colombia" object for tidy model estimation
# ----------------------------------- #
colombia <- colombia_cs %>%
  st_drop_geometry() %>%
  mutate(yr_grp = "2002-2009") %>%
  select(names(colombia)) %>%
  bind_rows(colombia, .)

rm(colombia_cs)
# ----------------------------------- #


# ----------------------------------- #
# Tidy yr_grp variable and create ID
# ----------------------------------- #
# Tidy variable
colombia %<>% mutate(yr_grp = factor(yr_grp,
                                     levels = c("2002-2009",
                                                "2002-2004",
                                                "2005-2007",
                                                "2008-2009")))

# ID To use in many apply function calls below:
yr_grp <- as.character(unique(colombia$yr_grp))
# ----------------------------------- #


# ----------------------------------- #
# Arrange data for lat-long alignment
# ----------------------------------- #
dat <- sapply(yr_grp, function(x){
  colombia %>% filter(yr_grp == x) %>%
    arrange(Department, Municipality, yr_grp)
}, simplify = FALSE)
# ----------------------------------- #
rm(colombia)
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# INLA SETUP
#-----------------------------------------------------------------------------#

# ----------------------------------- #
# Mesh setup - same for all years
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
# SPDE setup - same for all years
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
  'icews_bin', 'ged_bin', 'cinep_bin',
  'icews_cinep_under','ged_cinep_under'
)

# INLA Stacks
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
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# INLA MODEL ESTIMATION                                                   ----
#-----------------------------------------------------------------------------#
inla_mods <- sapply(yr_grp, function(yr){
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
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# SAVE MODELS                                                             ----
#-----------------------------------------------------------------------------#
save(inla_mods, dat, spde, dvs, yr_grp, file = "Results/inla-mods.RData")
rm(list=ls())
#-----------------------------------------------------------------------------#







