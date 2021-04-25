#-----------------------------------------------------------------------------#
#
# Author:        Logan Stundal
# Date:          April 14, 2021
# Purpose:       6.0 Post-Estimation Plots - ROC
#
#
# Copyright (c): Logan Stundal, 2021
# Email:         stund005@umn.edu
#
#-----------------------------------------------------------------------------#
#
# Notes:
#   Produces receiver-operator-curvers for INLA models
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
library(INLA)
library(pROC)
library(cowplot)

#---------------------------#
# Load data
#---------------------------#
load("results/inla-mods.Rdata")

#---------------------------#
# Local functions
#---------------------------#
local_theme <- function(title.size, text.size){
  theme_minimal() +
  theme(panel.grid       = element_blank(),
        legend.position  = "bottom",
        legend.direction = "horizontal",
        legend.title     = element_blank(),
        legend.key.size  = unit(2, "mm"),
        plot.title       = element_text(size  = ({{title.size}} / .pt)),
        text             = element_text(size  = ({{text.size}}  / .pt)),
        panel.background = element_rect(fill  = NA,
                                        color = "black",
                                        size  = 0.5,
                                        linetype = "solid"))
}

local_roc <- function(dv,
                      yr,
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
  y <- data %>% filter(yr_grp == {{yr}}) %>% pull({{dv}})
  # ----------------------------------- #


  # ----------------------------------- #
  # Estimate ROC data
  # ----------------------------------- #
  roc_est <- roc(response  = y,
                 predictor = pnorm(inla_mods[[yr]][[dv]]$summary.linear.predictor[1:1116,'mean']),
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
                        'GROUP'   = sprintf("%s_%s",dv, yr),
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
              x["mean"],
              x["lb"],
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

# local_prc_plot <- function(roc_list,
#                            title      = NULL,
#                            title.size = 14,
#                            text.size  = 12){
#
#   if(is.null(names(roc_list))){
#     stop("Function requires a named list for plot formatting.")
#   }
#
#   # ----------------------------------- #
#   # Description
#   # ----------------------------------- #
#   # This function takes a list of roc objects returned from local_roc()
#   # and returns a precision-recall plot. Note, I don't know how to est.
#   # standard errors for these stats yet.
#   # ----------------------------------- #
#
#   # ----------------------------------- #
#   # Tidy data
#   # ----------------------------------- #
#   plt_dat  <- roc_list %>% map("roc_est")
#
#   plt_dat <- sapply(plt_dat, function(x){
#     x %<>% coords(ret = "all", transpose = FALSE) %>%
#       dplyr::select(precision, recall)
#   }, simplify = FALSE) %>%
#     bind_rows(.id = "Model")
#   # ----------------------------------- #
#
#   # ----------------------------------- #
#   # Create plot
#   # ----------------------------------- #
#   prc <- ggplot(data = plt_dat,
#                 aes(y = precision, x = recall, color = Model)) +
#     geom_line() +
#     scale_y_continuous("Precision",
#                        limits = c(0,1),
#                        labels = scales::percent) +
#     scale_x_continuous("Recall",
#                        limits = c(0,1),
#                        labels = scales::percent) +
#     local_theme(title.size, text.size) +
#     labs(title = title)
#   # ----------------------------------- #
#
#   # ----------------------------------- #
#   # Return statement
#   # ----------------------------------- #
#   return(prc)
#   # ----------------------------------- #
# }

prc_dat <- function(model_list, true_values){
  # ----------------------------------- #
  # Setup
  # ----------------------------------- #
  thresh  <- seq(0,1,length.out = 200)
  n       <- length(true_values)
  plt_dat <- data.frame()
  # ----------------------------------- #


  # ----------------------------------- #
  # Estimate values
  # ----------------------------------- #
  for(g in c("icews", "ged")){
    dv = sprintf("%s_bin", g)

    model_linear_predictor <- pnorm(model_list[[dv]]$summary.linear.predictor[1:1116,'mean'])

    for (i in 1:length(thresh)) {

      z = ifelse(model_linear_predictor >= thresh[i], 1, 0)
      x = table(z,true_values)

      tn = tryCatch(
        expr  = {x['0','0']},
        error = function(e){0}
      )

      tp = tryCatch(
        expr  = {x['1','1']},
        error = function(e){0}
      )

      fp = tryCatch(
        expr  = {x['1','0']},
        error = function(e){0}
      )

      fn = tryCatch(
        expr  = {x['0','1']},
        error = function(e){0}
      )

      prec = tp / (tp + fp)
      prec_se = sqrt((prec * (1 - prec))/ (n + 4))
      prec_lb = prec - 1.96 * prec_se
      prec_ub = prec + 1.96 * prec_se

      recc = tp / (tp + fn)

      tmp_dat <- data.frame(
        "Re"    = recc,
        "Pr"    = prec,
        "Pr_lb" = prec_lb,
        "Pr_ub" = prec_ub,
        "Group" = stringr::str_to_upper(g)
      )
      plt_dat <- bind_rows(plt_dat, tmp_dat)
    }
  }
  # ----------------------------------- #


  # ----------------------------------- #
  # Tidy results
  # ----------------------------------- #
  tmp<-plt_dat %>%
    mutate(Pr_lb = case_when(is.nan(Pr_lb) & Pr == 1 ~ 1,
                             is.nan(Pr_lb) & Pr == 0 ~ 0,
                             TRUE ~ Pr_lb),
           Pr_ub = case_when(is.nan(Pr_ub) & Pr == 1 ~ 1,
                             is.nan(Pr_ub) & Pr == 0 ~ 0,
                             TRUE ~ Pr_ub))
  # ----------------------------------- #


  # ----------------------------------- #
  # Return statement
  # ----------------------------------- #
  return(plt_dat)
  # ----------------------------------- #
}
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# ESTIMATE ROC DATA                                                       ----
#-----------------------------------------------------------------------------#
dv_yr <- expand.grid(dvs, yr_grp) %>%
  mutate(x = paste(Var1, Var2, sep = ".")) %>%
  pull(x)

# Reconstruct a full panel.
colombia <- bind_rows(dat)

# Calculate rocs
rocs <- sapply(dv_yr, function(x){
  tmp    <- str_split(x, "[.]")
  tmp_dv <- tmp[[1]][1]
  tmp_yr <- tmp[[1]][2]

  return(local_roc(dv = tmp_dv, yr = tmp_yr))
}, simplify = F)
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
# ----------------------------------- #


# ----------------------------------- #
# Create combined plot
# ----------------------------------- #
roc_event <- plot_grid(roc_event_0209, roc_event_0204, roc_event_0507,
                   ncol = 3)
roc_under <- plot_grid(roc_under_0209, roc_under_0204, roc_under_0507,
                   ncol = 3)

ggsave(filename = "Results/Plots/roc-event.png",
       plot     = roc_event,
       width    = 9.0,
       height   = 4.5,
       dpi      = 350)

ggsave(filename = "Results/Plots/roc-under.png",
       plot     = roc_under,
       width    = 9.0,
       height   = 4.5,
       dpi      = 350)
# ----------------------------------- #
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# PRECISION RECALL CURVES                                                 ----
#-----------------------------------------------------------------------------#


tst <- prc_dat(model_list  = inla_mods$`2002-2009`,
               true_values = dat$`2002-2009`$cinep_bin)




ggplot(data = tst) +
  geom_ribbon(aes(x = Re, ymin = Pr_lb, ymax = Pr_ub, fill = Group), alpha = .5) +
  geom_line(aes(x = Re, y = Pr, color = Group), size = 0.25) +
  labs(title = "SPDE Models, Precision-Recall Curves",
       x     = "Recall",
       y     = "Precision") +
  scale_color_manual("Group", values = c('black','black')) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_continuous(labels = scales::percent_format()) +
  local_theme(title.size, text.size)

# ----------------------------------- #
# Observed event models
# ----------------------------------- #
prc_event_0209 <- local_prc_plot(roc_list = list("ICEWS" = rocs$`icews_bin.2002-2009`,
                                                 "GED"   = rocs$`ged_bin.2002-2009`),
                                 title      = "PRC - Event: 2002-2009",
                                 title.size = tit.size,
                                 text.size  = txt.size)

prc_event_0204 <- local_prc_plot(roc_list = list("ICEWS" = rocs$`icews_bin.2002-2004`,
                                                 "GED"   = rocs$`ged_bin.2002-2004`),
                                 title      = "PRC - Event: 2002-2004",
                                 title.size = tit.size,
                                 text.size  = txt.size)

prc_event_0507 <- local_prc_plot(roc_list = list("ICEWS" = rocs$`icews_bin.2005-2007`,
                                                 "GED"   = rocs$`ged_bin.2005-2007`),
                                 title      = "PRC - Event: 2005-2007",
                                 title.size = tit.size,
                                 text.size  = txt.size)
# ----------------------------------- #


# ----------------------------------- #
# Under-reporting models
# ----------------------------------- #
prc_under_0209 <- local_prc_plot(roc_list = list("ICEWS" = rocs$`icews_cinep_under.2002-2004`,
                                                 "GED"   = rocs$`ged_cinep_under.2002-2009`),
                                 title      = "PRC - Under: 2002-2009",
                                 title.size = tit.size,
                                 text.size  = txt.size)

prc_under_0204 <- local_prc_plot(roc_list = list("ICEWS" = rocs$`icews_cinep_under.2002-2004`,
                                                 "GED"   = rocs$`ged_cinep_under.2002-2004`),
                                 title      = "PRC - Under: 2002-2004",
                                 title.size = tit.size,
                                 text.size  = txt.size)

prc_under_0507 <- local_prc_plot(roc_list = list("ICEWS" = rocs$`icews_cinep_under.2005-2007`,
                                                 "GED"   = rocs$`ged_cinep_under.2005-2007`),
                                 title      = "PRC - Under: 2005-2007",
                                 title.size = tit.size,
                                 text.size  = txt.size)
# ----------------------------------- #


# ----------------------------------- #
# Create combined plot
# ----------------------------------- #
prc_event <- plot_grid(prc_event_0209, prc_event_0204, prc_event_0507,
                   ncol = 3)
prc_under <- plot_grid(prc_under_0209, prc_under_0204, prc_under_0507,
                   ncol = 3)

ggsave(filename = "Results/Plots/prc-event.png",
       plot     = prc_event,
       width    = 9.0,
       height   = 4.5,
       dpi      = 350)

ggsave(filename = "Results/Plots/prc-under.png",
       plot     = prc_under,
       width    = 9.0,
       height   = 4.5,
       dpi      = 350)
# ----------------------------------- #
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# SAVE                                                                    ----
#-----------------------------------------------------------------------------#
#save.image()
rm(list = ls())
#-----------------------------------------------------------------------------#

