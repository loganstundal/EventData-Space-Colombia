#-----------------------------------------------------------------------------#
#
# Author:        Logan Stundal
# Date:          July 29, 2021
# Purpose:       Estimates Spatial Probit Error Model (discrete space)
#                presented in Appendix.
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
library(tidyr)
library(tibble)
library(ggplot2)
library(stringr)
library(purrr)
library(forcats)
library(sf)
library(ProbitSpatial)
library(sandwich)
library(spdep)
library(kableExtra)
#---------------------------#

#---------------------------#
# Load data
#---------------------------#
load("Data/farc_events.Rdata")
load("Data/colombia2.Rdata")
#---------------------------#

#---------------------------#
# Functions
#---------------------------#
tidy_model <- function(model){
  # ----------------------------------- #
  # Description
  # ----------------------------------- #
  # Takes a glm or SpatialProbit class and returns a list object
  # containing tidied model results to use in tables or plots
  # ----------------------------------- #
  if(any(class(model) == "SpatialProbit")){
    model_coefs <- model@coeff
    model_ses   <- spem_ses(model)
    model_llik  <- model@loglik
    model_lb    <- model_coefs - 1.96 * model_ses
    model_ub    <- model_coefs + 1.96 * model_ses
    preds       <- pnorm(as.vector(model@X %*% model@beta))
  } else{
    model_coefs <- coef(model)
    model_ses   <- vcovHC(x = model, type = "HC1") %>%
      diag %>% sqrt %>% as.vector
    model_llik  <- as.numeric(logLik(model))
    model_lb    <- model_coefs - 1.96 * model_ses
    model_ub    <- model_coefs + 1.96 * model_ses
    preds       <- model$fitted.values
  }

  model_results <- list(
    "model" = model,
    "coefs" = model_coefs,
    "ses"   = model_ses,
    "lliks" = model_llik,
    "lb"    = model_lb,
    "ub"    = model_ub,
    "preds" = preds
  )
  return(model_results)
}


spem_cut <- function(x){
  # ----------------------------------- #
  # Description
  # ----------------------------------- #
  # Translates spem predicted probabilities into categories for mapping
  # ----------------------------------- #
  cut(x,
      breaks         = c(0, 0.05, 0.1, 0.2, 0.4, 0.6, Inf),
      include.lowest = TRUE,
      dig.lab        = 2) %>%
    fct_recode(., "(0.6 +)" = "(0.6,Inf]")
}


spem_ses <- function(object){
  # ----------------------------------- #
  # Description
  # ----------------------------------- #
  # Computes standard errors for SpatialProbit class models and returns as
  # a vector
  # ----------------------------------- #
  mycoef    <- object@coeff
  mod_covar <- ifelse(object@varcov == "varcov", "UC", "UP")

  # Estimate variance covariance matrix
  lik     <- function (th, env){.Call(paste('lik',object@DGP,mod_covar, sep='_'),
                                      th, env, PACKAGE = "ProbitSpatial")}
  H          <- numDeriv::hessian(lik, x = mycoef, env = object@env)
  spem_vcov  <- abs(solve(H))
  se         <- sqrt(diag(spem_vcov))
  return(se)
}


local_table <- function(models, caption, model_names = NULL){
  # ----------------------------------- #
  # Description
  # ----------------------------------- #
  # Takes a probit or spatial probit tidied list (from tidy_model() defined
  # nearby) and tidies coefficient and credibility interval estimates for a
  # kable table which is produced and returned
  # ----------------------------------- #
  res <- sapply(names(models), function(x){
    model <- models[[x]]
    res <- data.frame(
      "est"   = model[["coefs"]],
      "ses"   = model[["ses"]],
      "lb"    = model[["lb"]],
      "ub"    = model[["ub"]],
      "model" = x
    ) %>%
      rownames_to_column(var = "var") %>%
      mutate(across(c(est, lb, ub), ~format(round(.x, 3), nsmall = 3))) %>%
      mutate(hpd = sprintf("[%s, %s]", lb, ub)) %>%
      dplyr::select(var, est, hpd, model) %>%
      bind_rows(.,
                data.frame("var" = c("loglik","n"),
                           "est" = c(format(round(as.numeric(model["lliks"]), 3),
                                            nsmall = 3),
                                     "1116"),
                           "model" = rep(x, 2)))
  }, simplify = FALSE)

  tidy_vals <- res %>%
    bind_rows() %>%
    pivot_longer(cols = c(est, hpd),
                 values_to = "vals",
                 names_to  = "ids") %>%
    pivot_wider(id_cols = c(var, ids),
                names_from = model,
                values_from = vals) %>%
    drop_na() %>%
    dplyr::select(-ids)

  if(is.null(model_names)){
    model_names <- c("", "ICEWS", "GED", "CINEP",
                     "ICEWS - Underreporing","GED - Underreporting")
  }

  kbl_table <-  kbl(x         = tidy_vals,
                    caption   = caption,
                    format    = "pipe",
                    escape    = FALSE,
                    col.names = model_names,
                    align     = c("l","c","c","c","c","c"))
  return(kbl_table)
}
#---------------------------#
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# PROBIT MODEL SETUP                                                      ----
#-----------------------------------------------------------------------------#
# ----------------------------------- #
# Data
# ----------------------------------- #
# Note - Appendix only presents SPEM for full cross-section (2002-2009)
dat <- dat$`2002-2009` %>%
  rename(dist = distance_bogota_km_ln,
         pop  = pop_sum_ln,
         tri  = terrain_ri_mean_m) %>%
  mutate(id = paste(department, municipality, sep = "_")) %>%
  arrange(id)
# ----------------------------------- #


# ----------------------------------- #
# Spatial Weights - Row-Standardized
# ----------------------------------- #
# Create a spatial polygons dataframe to pass to 'poly2nb':
colombia2$id <- paste(colombia2$department, colombia2$municipality, sep = "_")
colombia2    <- as.data.frame(colombia2) %>%
  arrange(id) %>%
  st_set_geometry("geometry")
rownames(colombia2) <- colombia2$id

# QUEEN - Spatial neighbors, matrix and lists:
w <- poly2nb(pl = colombia2, queen = TRUE) %>%
  nb2mat(., style = "W")
colnames(w) <- rownames(w)
# ----------------------------------- #


# ----------------------------------- #
# Check W and data in same spatial order
# ----------------------------------- #
# data.frame("head_w" = head(rownames(w)),
#            "head_d" = head(dat$id),
#            "tail_w" = tail(rownames(w)),
#            "tail_d" = tail(dat$id))
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
# FIT MODELS                                                              ----
#-----------------------------------------------------------------------------#
# ----------------------------------- #
# Non-spatial Probits
# ----------------------------------- #
# Estimate models
probit_ns <- sapply(dvs, function(dv){
  cat(sprintf('Working on model %s\n', dv))

  mod <- glm(formula = update(formula, paste(dv, " ~ . ")),
             data    = dat,
             family  = binomial(link = "probit"))

  # Return tidied model results
  mod <- tidy_model(model = mod)
  return(mod)
}, simplify = F)
# ----------------------------------- #


# ----------------------------------- #
# Spatial Probits
# ----------------------------------- #
probit_sp <- sapply(dvs, function(dv){
  cat(sprintf('Working on model %s\n', dv))
  mod <- SpatialProbitFit(formula = update(formula, paste(dv, " ~ . ")),
                          data    = dat,
                          W       = w,
                          DGP     = 'SEM',
                          method  = "conditional",
                          varcov  = 'varcov',
                          control = list(iW_CL = 6))

  # Return tidied model results
  mod <- tidy_model(model = mod)
  return(mod)
}, simplify = F)
# ----------------------------------- #
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# Figure A4 - SPEM: Predicted Probability Map                             ----
#-----------------------------------------------------------------------------#
# ----------------------------------- #
# Tidy map data
# ----------------------------------- #
spem_probs <- sapply(probit_sp[1:3] %>% map("model"), function(x){
  pnorm(predict(x, X = x@X))
}, simplify = FALSE)
names(spem_probs) <- paste(names(spem_probs), 'SEM.prob',sep = '.')

# Turn spem_probs into a data frame and join department and municipality vars
spem_probs <- as.data.frame(spem_probs) %>%
  bind_cols(spem_probs, dat[,c("department", "municipality")]) %>%
  rename("ICEWS" = 1, "GED" = 2, "CINEP" = 3)

# Join predicted probabilities to spatial data and expand longer for
# facet plot
map_data <- colombia2 %>%
  left_join(., spem_probs, by = c("department", "municipality")) %>%
  mutate(across(.cols = ICEWS:CINEP,
                .fns  = ~(spem_cut(.x)))) %>%
  pivot_longer(.,
               cols = ICEWS:CINEP,
               values_to = "val",
               names_to  = "group") %>%
  mutate(group = str_to_upper(group)) %>%
  st_set_geometry(., "geometry")
# ----------------------------------- #


# ----------------------------------- #
# Produce map
# ----------------------------------- #
figureA4 <- ggplot(data = map_data, aes(fill = val)) +
  geom_sf(color = "gray50", size = 0) +
  facet_wrap(~group, ncol = 3) +
  scale_fill_manual(name   = "",
                    values = viridis::magma(n = 12)[seq(2,12,2)]) +
  theme_minimal() +
  theme(legend.position  = "bottom",
        legend.direction = "horizontal",
        legend.title     = element_blank(),
        legend.key.size  = unit(3, "mm"),
        axis.text        = element_blank(),
        panel.grid       = element_blank(),
        plot.title       = element_text(hjust = 0),
        strip.background = element_rect(fill = "gray90", linetype = "solid"),
        panel.background = element_rect(fill = NA, linetype = "solid", color = "black")) +
  guides(fill = guide_legend(nrow = 2)) +
  labs(title = "SPEM: Predicted Probabilities of FARC Events")
# ----------------------------------- #


# ----------------------------------- #
# Save map
# ----------------------------------- #
ggsave(filename = "Results/Replication-Figures/figure_appendix_4.png",
       plot     = figureA4,
       width    = 6.5,
       height   = 4.0,
       units    = "in",
       dpi      = 350)
# ----------------------------------- #
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# TABLES
#-----------------------------------------------------------------------------#
local_table(models  = probit_ns,
            caption = "Table A.3: Probits: 2002-2009") %>%
  save_kable("Results/Replication-Tables/table_appendix_3.txt")


local_table(models  = probit_sp,
            caption = "Table A.4: SPEM Models: 2002-2009") %>%
  save_kable("Results/Replication-Tables/table_appendix_4.txt")
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# SAVE                                                                    ----
#-----------------------------------------------------------------------------#
# Save non-spatial AND spatial probits
if(!dir.exists("Results/Published-Models"){
  dir.create("Results/Published-Models")})

save(probit_sp, probit_ns, w,
     file = "Results/Published-Models/published-models-spem.Rdata")
#-----------------------------------------------------------------------------#

rm(list=ls())
