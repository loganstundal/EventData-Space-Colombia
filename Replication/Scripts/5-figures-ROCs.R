#-----------------------------------------------------------------------------#
#
# Author:        Logan Stundal
# Date:          July 29, 2021
# Purpose:       Produces all ROC curves and AUC estimates.
#
#
# Copyright (c): Logan Stundal, 2021
# Email:         stund005@umn.edu
#
#-----------------------------------------------------------------------------#
#
# Notes:
# Figure 5,
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
library(ggplot2)
library(stringr)
library(purrr)
library(pROC)
library(cowplot)
#---------------------------#

#---------------------------#
# Load data
#---------------------------#
load("Data/farc_events.Rdata")
load("Results/Replication-Estimates/pred-data.Rdata")
load("Results/Published-Models/published-models-spem.Rdata")
#---------------------------#

#---------------------------#
# Functions
#---------------------------#
local_roc <- function(y,
                      dv_name,
                      yr,
                      pred,
                      sims = 1000,
                      cis  = 0.95){
  # ----------------------------------- #
  # Description
  # ----------------------------------- #
  # This function partially automates ROC construction by returning a
  # year-dv specific data frame tidied for plotting.
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
                        'GROUP'   = sprintf("%s_%s",dv_name, yr),
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
    geom_abline(intercept = 0, slope = 1, linetype = 'dashed',
                colour = 'gray80') +
    scale_color_manual("Model", values = rep('black',n)) +
    scale_fill_manual(values = {{model_colors}}) +
    scale_y_continuous("True Positive Rate",
                       labels = scales::percent_format(),
                       expand = c(0.005,0.005)) +
    scale_x_continuous("False Positive Rate",
                       labels = scales::percent_format(),
                       expand = c(0.005,0.005)) +
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
                                          linetype = "solid")) +
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
#---------------------------#
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# ESTIMATE ROC DATA                                                       ----
#-----------------------------------------------------------------------------#

# Expand dvs and years to calculate each ROC
dv_yr <- expand.grid(names(pred_data$`2002-2004`), names(pred_data)) %>%
  mutate(x = paste(Var1, Var2, sep = ".")) %>%
  pull(x)
# 16 = 2 (ICEWS, GED) * 4 (year groups) * 2 (observed, underreporting)


# INLA GMRF ROCs
rocs <- sapply(dv_yr, function(x){
  tmp    <- str_split(x, "[.]")
  tmp_dv <- tmp[[1]][1]
  tmp_yr <- tmp[[1]][2]

  dv_name <- str_split(tmp_dv, "_",n = 2)[[1]][1] %>% str_to_upper()
  pr <- pred_data[[tmp_yr]][[tmp_dv]]

  y <- dat[[tmp_yr]] %>% pull(cinep_bin)

  return(local_roc(y  = y,
                   dv = dv_name,
                   yr = tmp_yr,
                   pred = pr))
}, simplify = F)



# SPEM ROCs
roc_spem <- sapply(c(1,2,4,5), function(x){
  tmp_dv  <- names(probit_sp)[x]
  dv_name <- str_split(tmp_dv, "_",n = 2)[[1]][1] %>% str_to_upper()

  local_roc(y = dat$`2002-2009`[["cinep_bin"]],
            dv = dv_name,
            yr = "2002-2009",
            pred = probit_sp[[tmp_dv]][["preds"]])
}, simplify = FALSE)
names(roc_spem) <- names(probit_sp)[c(1,2,4,5)]
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

# ----------------------------------- #
# Observed event models
# ----------------------------------- #
roc_event_0209 <- local_roc_plot(roc_list = list("ICEWS" = rocs$`icews_bin.2002-2009`,
                                                 "GED"   = rocs$`ged_bin.2002-2009`),
                                 title      = "ROC - Event: 2002-2009",
                                 title.size = tit.size,
                                 text.size  = txt.size,
                                 auc.size   = auc.size)

roc_event_0204 <- local_roc_plot(roc_list = list("ICEWS" = rocs$`icews_bin.2002-2004`,
                                                 "GED"   = rocs$`ged_bin.2002-2004`),
                                 title      = "ROC - Event: 2002-2004",
                                 title.size = tit.size,
                                 text.size  = txt.size,
                                 auc.size   = auc.size)

roc_event_0507 <- local_roc_plot(roc_list = list("ICEWS" = rocs$`icews_bin.2005-2007`,
                                                 "GED"   = rocs$`ged_bin.2005-2007`),
                                 title      = "ROC - Event: 2005-2007",
                                 title.size = tit.size,
                                 text.size  = txt.size,
                                 auc.size   = auc.size)

roc_event_0809 <- local_roc_plot(roc_list = list("ICEWS" = rocs$`icews_bin.2008-2009`,
                                                 "GED"   = rocs$`ged_bin.2008-2009`),
                                 title      = "ROC - Event: 2008-2009",
                                 title.size = tit.size,
                                 text.size  = txt.size,
                                 auc.size   = auc.size)
# ----------------------------------- #


# ----------------------------------- #
# Under-reporting models
# ----------------------------------- #
roc_under_0209 <- local_roc_plot(roc_list = list("ICEWS" = rocs$`icews_cinep_under.2002-2004`,
                                                 "GED"   = rocs$`ged_cinep_under.2002-2009`),
                                 title      = "ROC - Under: 2002-2009",
                                 title.size = tit.size,
                                 text.size  = txt.size,
                                 auc.size   = auc.size)

roc_under_0204 <- local_roc_plot(roc_list = list("ICEWS" = rocs$`icews_cinep_under.2002-2004`,
                                                 "GED"   = rocs$`ged_cinep_under.2002-2004`),
                                 title      = "ROC - Under: 2002-2004",
                                 title.size = tit.size,
                                 text.size  = txt.size,
                                 auc.size   = auc.size)

roc_under_0507 <- local_roc_plot(roc_list = list("ICEWS" = rocs$`icews_cinep_under.2005-2007`,
                                                 "GED"   = rocs$`ged_cinep_under.2005-2007`),
                                 title      = "ROC - Under: 2005-2007",
                                 title.size = tit.size,
                                 text.size  = txt.size,
                                 auc.size   = auc.size)

roc_under_0809 <- local_roc_plot(roc_list = list("ICEWS" = rocs$`icews_cinep_under.2008-2009`,
                                                 "GED"   = rocs$`ged_cinep_under.2008-2009`),
                                 title      = "ROC - Under: 2008-2009",
                                 title.size = tit.size,
                                 text.size  = txt.size,
                                 auc.size   = auc.size)
# ----------------------------------- #


# ----------------------------------- #
# SPEM (Discrete Spatial Probit)
# ----------------------------------- #
figureA3 <- local_roc_plot(roc_list = list("ICEWS" = roc_spem$icews_bin,
                                           "GED"   = roc_spem$ged_bin),
                           title      = "ROC - Event: 2002-2009",
                           title.size = tit.size,
                           text.size  = txt.size,
                           auc.size   = auc.size)

figureA5 <- local_roc_plot(roc_list = list("ICEWS" = roc_spem$icews_cinep_under,
                                           "GED"   = roc_spem$ged_cinep_under),
                           title      = "ROC - Under: 2002-2009",
                           title.size = tit.size,
                           text.size  = txt.size,
                           auc.size   = auc.size)
# ----------------------------------- #


# ----------------------------------- #
# Create combined plots
# ----------------------------------- #
figure5 <- plot_grid(roc_event_0209, roc_event_0204, roc_event_0507,
                     ncol = 3)

figureA6 <- plot_grid(roc_under_0209, roc_under_0204, roc_under_0507,
                      ncol = 3)

figureA7 <- plot_grid(roc_event_0809, roc_under_0809,
                      ncol = 2)
# ----------------------------------- #
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# SAVE PLOTS                                                              ----
#-----------------------------------------------------------------------------#
ggsave(filename = "Results/Replication-Figures/figure_main_5.png",
       plot     = figure5,
       width    = 9.0,
       height   = 4.5,
       dpi      = 350)

ggsave(filename = "Results/Replication-Figures/figure_appendix_3.png",
       plot     = figureA3,
       width    = 3.0,
       height   = 4.5,
       dpi      = 350)

ggsave(filename = "Results/Replication-Figures/figure_appendix_5.png",
       plot     = figureA5,
       width    = 3.0,
       height   = 4.5,
       dpi      = 350)

ggsave(filename = "Results/Replication-Figures/figure_appendix_6.png",
       plot     = figureA6,
       width    = 9.0,
       height   = 4.5,
       dpi      = 350)

ggsave(filename = "Results/Replication-Figures/figure_appendix_7.png",
       plot     = figureA7,
       width    = 6.0,
       height   = 4.5,
       dpi      = 350)
# ----------------------------------- #

rm(list = ls())

#-----------------------------------------------------------------------------#



