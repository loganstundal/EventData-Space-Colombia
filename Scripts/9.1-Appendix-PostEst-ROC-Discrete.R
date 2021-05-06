#-----------------------------------------------------------------------------#
#
# Author:        Logan Stundal
# Date:          May 02, 2021
# Purpose:       9.1 - SPEM ROC
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
library(pROC)

#---------------------------#
# Load data
#---------------------------#
load("Results/discrete-spem-models.RData")
load("data/data_variables.RData")
rm(colombia_yg, colombia_pn)

#---------------------------#
# Local functions
#---------------------------#
local_theme <- function(title.size, text.size){
  theme_minimal() +
    theme(panel.grid       = element_blank(),
          legend.position  = "bottom",
          legend.direction = "horizontal",
          legend.title     = element_blank(),
          legend.key.size  = unit(3, "mm"),
          plot.title       = element_text(size  = ({{title.size}} / .pt)),
          text             = element_text(size  = ({{text.size}}  / .pt)),
          panel.background = element_rect(fill  = NA,
                                          color = "black",
                                          size  = 0.5,
                                          linetype = "solid"))
}

local_roc <- function(dv,
                      pred,
                      data = colombia,
                      sims = 1000,
                      cis  = 0.95){

  # ----------------------------------- #
  # Description
  # ----------------------------------- #
  # This function partially automates ROC construction by returning a year-dv specific
  # data frame tidied for plotting.
  # It defaults to data from "colombia" df object and subsets based on a yr (year) and
  # dv (dependent variable) string input (ideally passed from sapply())
  # ----------------------------------- #


  # ----------------------------------- #
  # Tidy data
  # ----------------------------------- #
  y    <- data %>% pull({{dv}})
  pred <- data %>% pull({{pred}})
  # ----------------------------------- #


  # ----------------------------------- #
  # Estimate ROC data
  # ----------------------------------- #
  roc_est <- roc(response  = y,
                 predictor = pred,
                 auc = T,
                 ci  = T)
  roc_auc <- round(as.numeric(roc_est$ci) * 100,2)
  roc_cis <- ci.thresholds(roc        = roc_est,
                           thresholds = seq(0,1,0.01),
                           boot.n     = sims,
                           conf.level = cis)
  # ----------------------------------- #


  # ----------------------------------- #
  # Tidy return objects
  # ----------------------------------- #
  roc_dat <- data.frame('FPR'     = 1 - roc_cis$specificity[,2],
                        'TPR_MED' = roc_cis$sensitivity[,2],
                        'TPR_LCI' = roc_cis$sensitivity[,1],
                        'TPR_UCI' = roc_cis$sensitivity[,3],
                        'GROUP'   = sprintf("%s",dv),
                        stringsAsFactors = F)


  dat <- list("roc_dat" = roc_dat,
              "roc_est" = roc_est,
              "roc_auc" = roc_auc,
              "roc_cis" = roc_cis)

  return(dat)
  # ----------------------------------- #
}

local_roc_plot <- function(roc_list,
                           title   = NULL,
                           auc_lab = TRUE,
                           title.size = 14,
                           text.size  = 12,
                           auc.size   = 08){
  if(is.null(names(roc_list))){
    stop("Function requires a named list for plot formatting.")
  }

  # ----------------------------------- #
  # Description
  # ----------------------------------- #
  # This function takes a list of roc objects returned from local_roc()
  # and returns a roc plot.
  # ----------------------------------- #

  # ----------------------------------- #
  # Tidy plot data
  # ----------------------------------- #
  plt_dat  <- roc_list %>% map("roc_dat") %>% bind_rows(.id = "Model")
  auc_vals <- roc_list %>% map("roc_auc") %>% bind_cols() %>%
    mutate(across(everything(), ~format(round(.x, 2), nsmall = 2)),
           "stat" = c("mean","lb","ub")) %>%
    pivot_longer(.,
                 cols      = !matches("stat"),
                 names_to  = "model",
                 values_to = "vals") %>%
    pivot_wider(.,
                id_cols     = model,
                values_from = vals,
                names_from  = stat)

  # Number of vars indicator for appropriate plot spcaing corrections:
  n <- length(roc_list)
  # ----------------------------------- #


  # ----------------------------------- #
  # Produce plot
  # ----------------------------------- #
  plt <- ggplot(data = plt_dat, aes(x = FPR)) +
    geom_ribbon(aes(ymin  = TPR_LCI,
                    ymax  = TPR_UCI,
                    fill  = Model),
                alpha = 0.5) +
    geom_line(aes(y     = TPR_MED,
                  color = Model),
              size = 0.25) +
    geom_abline(intercept = 0, slope = 1, linetype = 'dashed', colour = 'gray80') +
    scale_color_manual("Model", values = rep('black',n)) +
    scale_fill_manual(values = {{model_colors}}) +
    scale_y_continuous("True Positive Rate",
                       labels = scales::percent_format(),
                       expand = c(0.005,0.005)) +
    scale_x_continuous("False Positive Rate",
                       labels = scales::percent_format(),
                       expand = c(0.005,0.005)) +
    local_theme(title.size, text.size) +
    labs(title = title)


  if(auc_lab){
    lab <- apply(auc_vals,1, function(x){
      sprintf("%s: %s [95%% CI: %s - %s]\n",
              x["model"],
              x["lb"],
              x["mean"],
              x["ub"])
    }) %>%
      c("AUC:\n",.) %>%
      str_c(., collapse = "")

    plt <- plt +
      annotate(geom  = "text",
               x     = 1.00,
               y     = (0.02 * n),
               size  = (auc.size / .pt),
               label = lab,
               hjust = 1)
  }
  # ----------------------------------- #


  # ----------------------------------- #
  # Return statement
  # ----------------------------------- #
  return(plt)
  # ----------------------------------- #
}

#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# TIDY DATA                                                               ----
#-----------------------------------------------------------------------------#
se_events_probs <- sapply(spem, function(x){
  pnorm(predict(x, X = x@X))
}, simplify = FALSE)
names(se_events_probs) <- paste(names(se_events_probs), 'SEM.prob',sep = '.')
se_events_probs <- as.data.frame(se_events_probs)


colombia %<>%
  bind_cols(., se_events_probs) %>%
  left_join(., colombia_cs[, c("geometry", "ID_Mun")], by = "ID_Mun")

rm(colombia_cs)

# ----------------------------------- #
# COLOR VECTOR LIST FOR CONSISTENT PLOT COLORING
# ----------------------------------- #
model_colors <- viridis::viridis(n = 5)[c(1:3)]
names(model_colors) <- c("CINEP","ICEWS","GED")
#-----------------------------------------------------------------------------#


#-----------------------------------------------------------------------------#
# Predicted Probability Map
#-----------------------------------------------------------------------------#

local_cols <- viridis::magma(n = 12)[c(2,4,6,8,10,12)]

local_cut <- function(x){
  cut(x,
      breaks         = c(0, 0.05, 0.1, 0.2, 0.4, 0.6, Inf),
      include.lowest = TRUE,
      dig.lab        = 2) %>%
    fct_recode(., "(0.6 +)" = "(0.6,Inf]")
}

mp <- colombia %>%
  mutate(icews = local_cut(icews_bin.SEM.prob),
         ged   = local_cut(ged_bin.SEM.prob),
         cinep = local_cut(cinep_bin.SEM.prob)) %>%
  dplyr::select(icews,ged,cinep, geometry) %>%
  pivot_longer(.,
               cols = icews:cinep,
               values_to = "val",
               names_to  = "group") %>%
  mutate(group = str_to_upper(group)) %>%
  st_set_geometry(., "geometry")

spem_mp <- ggplot(data = mp, aes(fill = val)) +
  geom_sf(color = "gray50", size = 0) +
  facet_wrap(~group, ncol = 3) +
  scale_fill_manual(name   = "",
                    values = local_cols) +
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

ggsave(filename = "Results/Plots/Map-spem-probs.png",
       plot     = spem_mp,
       width    = 6.5,
       height   = 4.0,
       units    = "in",
       dpi      = 350)

#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# ANALYSIS                                                                ----
#-----------------------------------------------------------------------------#
icews_obs <- local_roc(dv = "cinep_bin", pred = "icews_bin.SEM.prob")
ged_obs   <- local_roc(dv = "cinep_bin", pred = "ged_bin.SEM.prob")

icews_und <- local_roc(dv = "cinep_bin", pred = "icews_cinep_under.SEM.prob")
ged_und   <- local_roc(dv = "cinep_bin", pred = "ged_cinep_under.SEM.prob")
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# CREATE ROC PLOTS                                                        ----
#-----------------------------------------------------------------------------#

# ----------------------------------- #
# Text sizes
# ----------------------------------- #
txt.size <- 20
tit.size <- 30
auc.size <- 07
# ----------------------------------- #

roc_event_0209 <- local_roc_plot(roc_list = list("ICEWS" = icews_obs,
                                                 "GED"   = ged_obs),
                                 title      = "ROC - Event: 2002-2009",
                                 title.size = tit.size,
                                 text.size  = txt.size,
                                 auc.size   = auc.size)


roc_under_0209 <- local_roc_plot(roc_list = list("ICEWS" = icews_und,
                                                 "GED"   = ged_und),
                                 title      = "ROC - Under: 2002-2009",
                                 title.size = tit.size,
                                 text.size  = txt.size,
                                 auc.size   = auc.size)

#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# SAVE                                                                    ----
#-----------------------------------------------------------------------------#
# Stand-alone cross sections
ggsave(filename = "Results/Plots/roc-spem-0209-event.png",
       plot     = roc_event_0209,
       width    = 3.0,
       height   = 4.5,
       dpi      = 350)

ggsave(filename = "Results/Plots/roc-spem-0209-under.png",
       plot     = roc_under_0209,
       width    = 3.0,
       height   = 4.5,
       dpi      = 350)
rm(list = ls())
#-----------------------------------------------------------------------------#
