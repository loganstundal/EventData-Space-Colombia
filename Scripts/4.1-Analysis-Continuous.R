#-----------------------------------------------------------------------------#
#
# Author:        Logan Stundal
# Date:          April 08, 2021
# Purpose:       SPDE - Continuous models
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
library(INLA)
library(kableExtra)


#---------------------------#
# Load data
#---------------------------#
load("data/data_variables.RData")
rm(colombia_cs, colombia_yg)

#---------------------------#
# Functions
#---------------------------#

qoi <- function(mod_list){
  # Takes an inla model list and returns a list containing quantities
  # of interest
  inla_betas <- lapply(mod_list, function(mod){
    round(mod$summary.fixed[,c("mean","0.025quant","0.975quant")],3) %>%
      as.data.frame() %>%
      rownames_to_column(var = "variable")
  })

  inla_hyper <- lapply(mod_list, function(mod, round_digits = 3){
    spde_pars <- inla.spde2.result(inla = mod,
                                   name = "spatial.field",
                                   spde,do.transform = TRUE)

    # Kappa
    Kappa    <- inla.emarginal(function(x) x, spde_pars$marginals.kappa[[1]])  # kappa (mean)
    Kappahpd <- inla.hpdmarginal(0.95, spde_pars$marginals.kappa[[1]])         # kappa (hpd 95%)

    # Sigma
    Sigma    <- inla.emarginal(function(x) x, spde_pars$marginals.variance.nominal[[1]]) # variance (mean)
    Sigmahpd <- inla.hpdmarginal(0.95, spde_pars$marginals.variance.nominal[[1]])        # variance (hpd 95%)

    # Range
    Range    <- inla.emarginal(function(x) x, spde_pars$marginals.range.nominal[[1]]) # range (mean)
    Rangehpd <- inla.hpdmarginal(0.95, spde_pars$marginals.range.nominal[[1]])        # range (hpd 95%)

    # Convert range to km (degrees = 2*pi*6371/360)
    Range    <- Range * 2*pi*6371/360
    Rangehpd <- Rangehpd * 2*pi*6371/360

    df <- rbind(cbind(Kappa, Kappahpd),
                cbind(Sigma, Sigmahpd),
                cbind(Range, Rangehpd)) %>%
      as.data.frame()

    colnames(df) <- c("mean","0.025quant","0.975quant")
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
# GROUPED YEAR DATA TIDY                                                  ----
#-----------------------------------------------------------------------------#
colombia <- colombia_pn %>%
  as.data.frame() %>%
  mutate(yr_grp = case_when(year %in% 2002:2004 ~ "2002-2004",
                            year %in% 2005:2007 ~ "2005-2007",
                            year %in% 2008:2009 ~ "2008-2009")) %>%
  group_by(Department, Municipality, yr_grp) %>%
  summarize(across(c(cinep, icews, ged), sum),
            across(c(distance_bogota_km_ln, terrain_ri_mean_m,
                     centroid_mun_long, centroid_mun_lat,
                     geometry), ~.x[1]),
            pop_sum_ln = mean(pop_sum_ln),
            .groups = "keep") %>%
  ungroup() %>%
  mutate(across(c(cinep, icews, ged), ~case_when(.x > 0 ~ 1, TRUE ~ 0), .names = "{col}_bin")) %>%
  mutate(icews_cinep_under = case_when(icews_bin == 0 & cinep_bin == 1 ~ 1, TRUE ~ 0),
         icews_cinep_bias  = case_when(icews_bin != cinep_bin ~ 1, TRUE ~ 0),

         ged_cinep_under   = case_when(ged_bin == 0 & cinep_bin == 1 ~ 1, TRUE ~ 0),
         ged_cinep_bias    = case_when(ged_bin != cinep_bin ~ 1, TRUE ~ 0)) %>%
  st_set_geometry(., "geometry")

rm(colombia_pn)


# ----------------------------------- #
# yr_grp ID
# ----------------------------------- #
# To use in many apply function calls below:
yr_grp <- unique(colombia$yr_grp)
# ----------------------------------- #


#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# INLA SETUP
#-----------------------------------------------------------------------------#

# ----------------------------------- #
# Mesh setup - same for all years
# ----------------------------------- #
tmp <- colombia %>%  filter(yr_grp == "2002-2004")

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

  tmp <- colombia %>%  filter(yr_grp == x)

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


# ----------------------------------- #
# Save inla mods
# ----------------------------------- #
# Since they are so large (1.5+ gb), will save
# to desktop until I decide if worth keeping or not:
# save(inla_mods, file = "c:/users/logan/desktop/colombia-inla-mods.RData")

# never mind. Figure this out later.
#... Rdata object is only 11.8 mb... must be some crazy compression going on.
# loljk 2gb.
# ----------------------------------- #
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# QOI                                                                     ----
#-----------------------------------------------------------------------------#
# Observed FARC event models
# Regression coefficients
res_vals <- sapply(yr_grp, function(x){
  qoi(inla_mods[[x]])
}, simplify = F)
#-----------------------------------------------------------------------------#


res <- sapply(res_vals, function(x){
  sapply(dvs, function(dv){
    bs <- x[["betas"]][[dv]]
    hy <- x[["hyper"]][[dv]]

    vl <- bind_rows(bs, hy)
    vl <- bind_cols(vl, "model" = dv)
  }, simplify = F)
}, simplify = F)


#-----------------------------------------------------------------------------#
# MODEL TIDY PARAMS                                                       ----
#-----------------------------------------------------------------------------#
# res <- lapply(dvs, function(x){
#   bs <- inla_betas[[x]]
#   hy <- inla_hyper[[x]]
#
#   vl <- bind_rows(bs,hy)
#   vl <- bind_cols(vl, "model" = x)
# })

res <- sapply(res_vals, function(x){
  sapply(dvs, function(dv){
    bs <- x[["betas"]][[dv]]
    hy <- x[["hyper"]][[dv]]

    vl <- bind_rows(bs, hy)
    vl <- bind_cols(vl, "model" = dv)
  }, simplify = F)
}, simplify = F)



res2 <- sapply(yr_grp, function(yg){

  tmp_global <- res[[yg]]

  sapply(tmp_global, function(x){

    x <- x %>%
      mutate(across(c("mean", "0.025quant", "0.975quant"), ~format(round(.x, 3), nsmall = 3))) %>%
      mutate(hpd = sprintf("[%s, %s]", `0.025quant`, `0.975quant`),
             yr_grp = yg) %>%
      dplyr::select(variable, mean, hpd, model, yr_grp)

  }, simplify = F)
}, simplify = F)


mods <- lapply(res2, function(x){
  bind_rows(x) %>%
  pivot_longer(.,
               cols = c("mean", "hpd"),
               names_to = "type") %>%
  pivot_wider(.,
              id_cols     = c(variable, type),
              names_from  = model,
              values_from = value)
})



# res <- lapply(res, function(x){
#   x <- x %>%
#     mutate(across(c("mean", "0.025quant", "0.975quant"), ~format(round(.x, 3), nsmall = 3))) %>%
#     mutate(hpd = sprintf("[%s, %s]", `0.025quant`, `0.975quant`)) %>%
#     dplyr::select(variable, mean, hpd, model)
# }) %>%
#   bind_rows %>%
#   pivot_longer(.,
#                cols = c("mean", "hpd"),
#                names_to = "type") %>%
#   pivot_wider(.,
#               id_cols     = c(variable, type),
#               names_from  = model,
#               values_from = value)

# mod_lliks <- bind_cols("variable" = "LogLik",
#                        "type"     = NA,
#                        bind_rows(inla_lliks)) %>%
#   mutate(across(all_of(dvs), ~format(round(.x, 3), nsmall = 3)))

# mod_ns <- rep("1116", length(dvs))
# names(mod_ns) <- dvs
#
# mod_ns <- bind_cols("variable" = "N",
#                     "type"     = NA,
#                     bind_rows(mod_ns))
#
# res <- bind_rows(res, mod_lliks, mod_ns)

mods2 <- sapply(1:length(mods), function(x){

  tmp <- bind_cols("variable" = "LogLik",
                   "type"     = NA,
                   bind_rows(res_vals[[x]]$lliks)) %>%
    mutate(across(all_of(dvs), ~format(round(.x, 3), nsmall = 3)))


  ns <- rep("1116", length(dvs))
  names(ns) <- dvs

  tmp2 <- bind_cols("variable" = "N",
                   "type"     = NA,
                   bind_rows(ns))

  bind_rows(mods[[x]],
            tmp,
            tmp2)

}, simplify = FALSE)

names(mods2) <- yr_grp

# Tidy dfs
mods2 <- sapply(mods2, function(x){
  x %>%
    dplyr::select(-type) %>%
    mutate(variable = case_when(variable == "intercept" ~ "Intercept",
                                variable == "dist"      ~ "Dist. Bogota, km (log)",
                                variable == "pop"       ~ "Population (log)",
                                variable == "tri"       ~ "TRI",
                                TRUE ~ variable))
}, simplify = F)
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# TABLES                                                                  ----
#-----------------------------------------------------------------------------#
mods_tex <- sapply(1:length(mods2), function(x){
  md <- mods2[[x]]
  md %>%
    kbl(caption   = yr_grp[x],
        col.names = c("", "ICEWS", "GED", "CINEP", "ICEWS-Underreporing","GED-Underreporting"),
        align     = c("l","c","c","c","c","c"),
        format    = "latex",
        booktabs  = TRUE)
  # %>%
  #   column_spec(column = 1,
  #               width  = "2in", ) %>%
  #   collapse_rows(columns = 1,
  #                 valign  = "middle")
}, simplify = FALSE)

names(mods_tex) <- yr_grp
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# SAVE                                                                    ----
#-----------------------------------------------------------------------------#
save(mods_tex, file = "Results/Tables/INLA_MODS.Rdata")
#rm(list = ls())
#-----------------------------------------------------------------------------#




