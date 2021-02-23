#-----------------------------------------------------------------------------#
#                                                                             
# Author:        Logan Stundal                                                    
# Date:          March 25, 2020                                                 
# Purpose:       Discrete spatial error model plots: Predicted probability, ROCs                      
#                                                                             
#
# Copyright (c): Logan Stundal, 2020                      
# Email:         stund005@umn.edu
#
#-----------------------------------------------------------------------------# 
#
# Notes:                                                                    
#         20200524 - updated predicted probability maps to identical theme as spde maps. Did not change bias maps, noted below                                                                    
#                                                                             
#-----------------------------------------------------------------------------#



# ADMINISTRATIVE --------------------------------------------------------------

#---------------------------#
# Clear working directory   
#---------------------------#
rm(list = ls())

#---------------------------#
# Load required libraries   
#---------------------------#
library(tidyverse)
library(sf)
library(ProbitSpatial)
# library(plotROC)
library(pROC)
library(precrec)
library(cowplot)

#---------------------------#
# Set working directory     
#---------------------------#
setwd('c:/users/logan/googledrive/umn/research/ra_john/event_data_project')

#---------------------------#
# Load data                 
#---------------------------#
load('data/Models-Discrete.rdata')

#---------------------------#
# Load functions            
#---------------------------#
# source('scripts/Functions_EDP.R')

my_theme <- {
  theme_minimal() +
    theme(panel.grid.minor   = element_blank(),
          panel.grid.major.x = element_blank(),
          plot.title         = element_text(size = 12),
          plot.subtitle      = element_text(size = 10),
          plot.caption       = element_text(size = 10),
          axis.title         = element_text(size = 10),
          axis.text          = element_text(size = 10),
          legend.position    = 'bottom',
          legend.direction   = 'horizontal',
          legend.title       = element_blank())
}


# Predicted probability maps --------------------------------------------------
{bogota <- data.frame('x' = -74.064559, 
                      'y' = 4.709534)
bogota <- st_as_sf(bogota, coords = c('x','y'), crs = '+proj=longlat +datum=WGS84')
bogota <- st_transform(bogota, crs = st_crs(colombia))}

# Messing around with CIs for ROC curve confidence intervals.
# Calculate predicted probabilities for DVs
 # se_events_probs_ci <- lapply(1:length(dvs_events), function(x){
 #  # pnorm(predict(se_probits_events[[x]], X = se_probits_events[[x]]@X))
 #  var = x
 # 
 #  crit = abs(qnorm((1-0.95)/2))
 # 
 #  X = se_probits_events[[x]]@X
 #  b = se_probits_events[[x]]@beta
 # 
 #  nvars = length(b)
 # 
 #  ses = se_probits_events.ses[[x]][1:nvars]
 # 
 #  b.lci = b - crit * ses
 #  b.uci = b + crit * ses
 # 
 #  d = data.frame('point' = pnorm(X %*% b),
 #                 'lci'   = pnorm(X %*% b.lci),
 #                 'uci'   = pnorm(X %*% b.uci))
 # });names(se_events_probs) <- paste(dvs_events, 'SEM.prob',sep = '.')

 
se_events_probs <- lapply(1:length(dvs_events), function(x){
  pnorm(predict(se_probits_events[[x]], X = se_probits_events[[x]]@X))
});names(se_events_probs) <- paste(dvs_events, 'SEM.prob',sep = '.') 
se_events_probs <- as.data.frame(se_events_probs)

se_under_probs <- lapply(1:length(dvs_under), function(x){
  pnorm(predict(se_probits_under[[x]], X = se_probits_under[[x]]@X))
});names(se_under_probs) <- paste(dvs_under, 'SEM.prob',sep = '.')
se_under_probs <- as.data.frame(se_under_probs)

colombia <- colombia %>%
  bind_cols(., se_events_probs) %>%
  bind_cols(., se_under_probs)

probs <- as.data.frame(c(se_events_probs, se_under_probs))

min(probs)
max(probs)

# ----------------------------------- #
# Discretized probs:

# bob <- lapply(se_events_probs, 
#               function(x){
#                 cut(x      = x,
#                     breaks = seq(0,1,0.1),
#                     labels = c("(0,  10]",
#                                "(10, 20]",
#                                "(20, 30]",
#                                "(30, 40]",
#                                "(40, 50]",
#                                "(50, 60]",
#                                "(60, 70]",
#                                "(70, 80]",
#                                "(80, 90]",
#                                "(90, 100]"))
#               }) %>% as.data.frame()

# colombia <- colombia %>%
#   mutate(CUTS = cut(x      = icews_farc_bin.SEM.prob,
#                     breaks = seq(0,1,0.1),
#                     labels = c("(0,  10]",
#                                "(10, 20]",
#                                "(20, 30]",
#                                "(30, 40]",
#                                "(40, 50]",
#                                "(50, 60]",
#                                "(60, 70]",
#                                "(70, 80]",
#                                "(80, 90]",
#                                "(90, 100]"))) 
# 
# ggplot() +
#   geom_sf(data = colombia, aes(fill = CUTS), color = 'transparent') +
#   scale_fill_viridis_d(option = "magma") +
#   theme_minimal() +
#   my_theme +
#   labs(title = "ICEWS: Predicted FARC Event")


# ----------------------------------- #
# Produce maps
cols <- viridis::magma(n = 8)

# OBSERVED FARC EVENTS
prob_maps_events <- lapply(paste(c(dvs_events), 'SEM.prob',sep ='.'), function(x){
  var = x

  ggplot(data = colombia) +
    geom_sf(aes(fill = colombia[[var]]), alpha = 0.9, colour = 'transparent') +
    geom_sf(data = bogota, shape = 24, fill = 'black', colour = 'white', size = 2) +
    scale_fill_gradientn(name    = '',
                         limits  = c(0, 1),
                         colours = cols[1:8],
                         breaks  = seq(0,1,0.2),
                         labels  = scales::percent) +
    theme_minimal() +
    theme(axis.text  = element_blank(),
          axis.title = element_blank(),
          panel.grid = element_blank(),
          plot.title = element_text(hjust = 0.5),
          plot.margin=unit(c(t = 0, l = 0, b = -1, r = 0),"cm"))
})

prob_maps_events[[1]] = prob_maps_events[[1]] + theme(legend.position = 'none')
prob_maps_events[[2]] = prob_maps_events[[2]] + theme(legend.position = 'none')
legend                = get_legend(prob_maps_events[[3]])
prob_maps_events[[3]] = prob_maps_events[[3]] + theme(legend.position = 'none')

map_probs  <- plot_grid(plotlist   = list(prob_maps_events[[1]],
                                       prob_maps_events[[2]],
                                       prob_maps_events[[3]],
                                       legend),
                        ncol       = 4, 
                        rel_widths = c(.3, .3, .3,0.1)) +
  draw_label(label = expression(paste('Predicted probability, ', pi)),
             x = 0.05, y = 0.88,
             hjust = 0, vjust = 0,
             lineheight = 1.0,
             size = 14) + 
  draw_text(text = c('ICEWS', 'GED', 'CINEP'),
            x    = c(.15,.45,.75), y = rep(0.85,3),
            size = 12)

# ----------------------------------- #
# Original file name: 'Plots/Maps-Discrete/Map-spe-Probs.png'

# Changed on 2020-08-07
# ----------------------------------- #
ggsave(filename = "Paper Drafts/Draft_20200807/Map-spe-Probs.png",
       plot     = map_probs,
       width    = 6.5,
       height   = 3.0,
       dpi      = 320)





# OLD CODE BELOW DIFFERS FROM PREDICTED PROBABILITY MAP ABOVE.





# UNDERREPORTING BIAS
prob_maps_under <- lapply(paste(c(dvs_under), 'SEM.prob',sep ='.'), function(x){
  var = x
  # print(var)
  
  ggplot(data = colombia) +
    geom_sf(aes(fill = colombia[[var]]), size = 0.01, alpha = 0.9) +
    geom_sf(data = bogota, aes(shape = 'Bogota'), size = 3, fill = cols[9],
            show.legend = 'point') +
    scale_fill_gradientn(name    = '',
                         limits  = c(0, 1),
                         colours = cols[1:8],
                         breaks  = seq(0,1,0.2),
                         labels  = scales::percent) +
    scale_shape_manual(name  = '',
                       values = 24) +
    map_theme + 
    labs(title    = sprintf('%s: Predicted probability of Underreporting Bias',
                            str_to_upper(str_extract(var,  "[^_]+"))),
         subtitle = 'Estimates from Discrete Spatial Error Model')
})
prob_maps_under_all <- do.call(gridExtra::grid.arrange, 
                                c(prob_maps_under, ncol = length(prob_maps_under)))
ggsave(filename = 'paper drafts/memo_20200325/plots_20200325/Discrete-SPE_Predprobs_Under.png',
       plot     = prob_maps_under_all,
       width    = 14,
       height   = 8,
       dpi      = 320)



# ROC PLOTS ------------------------------------------------------------------
# Code added on: 2020-08-07
roc_icews <- roc(colombia$cinep_farc_bin,
                 se_events_probs$icews_farc_bin.SEM.prob,
                 auc = T,
                 ci  = T)
roc_icews_auc <- round(as.numeric(roc_icews$ci) * 100,2)
ci_icews  <- ci.thresholds(roc_icews, thresholds = seq(0,1,0.01))

roc_ged   <- roc(colombia$cinep_farc_bin,
                 se_events_probs$ged_farc_bin.SEM.prob,
                 auc = T,
                 ci  = T)
roc_ged_auc <- round(as.numeric(roc_ged$ci) * 100,2)
ci_ged    <- ci.thresholds(roc_ged, thresholds = seq(0,1,0.01))


# Organize data
roc_icews_dat   <- data.frame('FPR'     = 1 - ci_icews$specificity[,2],
                              'TPR_MED' = ci_icews$sensitivity[,2],
                              'TPR_LCI' = ci_icews$sensitivity[,1],
                              'TPR_UCI' = ci_icews$sensitivity[,3],
                              'GROUP'   = 'ICEWS',
                              stringsAsFactors = F)

roc_ged_dat     <- data.frame('FPR'     = 1 - ci_ged$specificity[,2],
                              'TPR_MED' = ci_ged$sensitivity[,2],
                              'TPR_LCI' = ci_ged$sensitivity[,1],
                              'TPR_UCI' = ci_ged$sensitivity[,3],
                              'GROUP'   = 'GED',
                              stringsAsFactors = F)
plt_dat = bind_rows(roc_icews_dat,
                    roc_ged_dat)


# Plot
roc_plot <- ggplot(data = plt_dat, aes(x = FPR)) + 
  geom_ribbon(aes(ymin  = TPR_LCI,
                  ymax  = TPR_UCI,
                  fill  = GROUP),
              alpha = 0.5) +
  geom_line(aes(y     = TPR_MED,
                color = GROUP),
            size = 0.25) +
  geom_abline(intercept = 0, slope = 1, linetype = 'dashed', colour = 'gray80') +
  scale_color_manual("GROUP", values = c('black','black')) +
  scale_y_continuous("True Positive Rate", 
                     labels = scales::percent_format()) + 
  scale_x_continuous("False Positive Rate", 
                     labels = scales::percent_format()) + 
  annotate(geom  = "text",
           x     = 0.45,
           y     = 0.33, 
           label = sprintf("AUC - ICEWS: %s [95%% CI: %s - %s]\nAUC - GED:\ \ \   %s [95%% CI: %s - %s]",
                           roc_icews_auc[2],
                           roc_icews_auc[1],
                           roc_icews_auc[3],
                           roc_ged_auc[2],
                           roc_ged_auc[1],
                           format(roc_ged_auc[3], nsmall = 2)),
           hjust = 0) +
  my_theme


# ----------------------------------- #
# Nb - 2020-08-07 on precision-recall curves, if needed in the future:
prc_icews <- roc(colombia$cinep_farc_bin,
           se_events_probs$icews_farc_bin.SEM.prob) %>%
  coords(ret = "all", transpose = FALSE) %>%
  dplyr::select(precision, recall) %>%
  mutate(GROUP = 'ICEWS')
prc_ged <- roc(colombia$cinep_farc_bin,
               se_events_probs$ged_farc_bin.SEM.prob) %>%
  coords(ret = "all", transpose = FALSE) %>%
  dplyr::select(precision, recall) %>%
  mutate(GROUP = 'GED')

plt_dat = bind_rows(prc_icews, prc_ged)

prc <- ggplot(data = plt_dat, 
       aes(y = precision, x = recall, color = GROUP)) + 
  geom_line() + 
  scale_y_continuous("Precision",
                     limits = c(0.25,1),
                     labels = scales::percent) +
  scale_x_continuous("Recall",
                     limits = c(0,1),
                     labels = scales::percent) +
  my_theme + 
  labs(title = "Spatial Probit Error Models, Precision-Recall Curves")

ggsave(filename = "Paper Drafts/Draft_20200807/PRC-Discrete.png",
       plot     = prc,
       width    = 8,
       height   = 6,
       dpi      = 320)



# ----------------------------------- #
# Use a sample dataset created by the create_sim_samples function
samps4 <- create_sim_samples(2, 10, 10, "random")

smdat <- mmdata(samps4[["scores"]], samps4[["labels"]], dsids = samps4[["dsids"]])
smdat <- mmdata(se_events_probs$icews_farc_bin.SEM.prob, colombia$cinep_farc_bin, dsids = 1)
smcur <- evalmod(smdat, raw_curves = T)
autoplot(smcur, "PRC", show_cb = TRUE)
autoplot(smcur, "PRC")

smmdat2 <- mmdata(samps3[["scores"]], samps3[["labels"]], dsids = samps3[["dsids"]])
smcurves <- evalmod(smmdat2, raw_curves = TRUE)
autoplot(smcurves, "PRC", show_cb = TRUE)


#-----------------------------------------------------------------------------#

##################################################
### Single model & multiple test datasets
###
samps <- create_sim_samples(2, 100, 100, "good_er")
mdat <- mmdata(samps[["scores"]], samps[["labels"]],
               modnames = samps[["modnames"]],
               dsids = samps[["dsids"]])

## Generate an smcurve object that contains ROC and Precision-Recall curves
curves <- evalmod(mdat, raw_curves = TRUE)

curves <- evalmod(scores = se_events_probs[,1],
                  labels = colombia$cinep_farc_bin)

autoplot(curves, "PRC", show_cb = T)

## Average ROC and Precision-Recall curves
autoplot(smcurves, raw_curves = FALSE)




# ----------------------------------- #



# Not fucking working...
mmdat1 <- mmdata(scores = join_scores(se_events_probs[,c(1,2)]),
                 labels = join_labels(colombia$cinep_farc_bin),
                 modnames = c("b"),
                 dsids = c(1,2)
)
mmcurves <- evalmod(mmdat1,cb_alpha = 0.05)
head(as.data.frame(mmcurves))
x = as.data.frame(mmcurves)
table(x$modname)
autoplot(mmcurves, "PRC", curvetype = 'prc')












head(se_events_probs[,1:2])


thresh = seq(0,1,length.out = 200)
tr     = colombia$cinep_farc_bin
n      = length(tr)
pr = data.frame()
re = data.frame()

for (i in 1:length(thresh)) {
  # print(i)  
  z      = ifelse(se_events_probs$icews_farc_bin.SEM.prob >= thresh[i], 1, 0)
  x = table(z,tr)
  
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
  
  
  pr[i,1] = prec
  pr[i,2] = prec_lb
  pr[i,3] = prec_ub
  
  re[i,1] = recc
  
  # ----------------------------------- #
  # phat = tp / n
  # zscore = abs(qnorm(phat))
  # varz  = phat * (1-phat) / n
  # std   = sqrt(varz)
  # 
  # tp_lb = phat - zscore * std
  # tp_ub = phat + zscore * std
  # 
  # # ----------------------------------- #
  # 
  # phat = fp / n
  # zscore = abs(qnorm(phat))
  # varz  = phat * (1-phat) / n
  # std   = sqrt(varz)
  # 
  # fp_lb = phat - zscore * std
  # fp_ub = phat + zscore * std
  # 
  # # ----------------------------------- #
  # 
  # phat = tn / n
  # zscore = abs(qnorm(phat))
  # varz  = phat * (1-phat) / n
  # std   = sqrt(varz)
  # 
  # tn_lb = phat - zscore * std
  # tn_ub = phat + zscore * std
  # 
  # # ----------------------------------- #
  # 
  # phat = fn / n
  # zscore = abs(qnorm(phat))
  # varz  = phat * (1-phat) / n
  # std   = sqrt(varz)
  # 
  # fn_lb = phat - zscore * std
  # fn_ub = phat + zscore * std
  # 
  # #-----------------------------------------------------------------------------#
  # 
  # prec_lb = tp_lb / (tp_lb + fp_lb)
  # recc_lb = tp_lb / (tp_lb + fn_lb)
  # 
  # pr[i,2] = prec_lb
  # re[i,2] = recc_lb
  # 
  # prec_ub = tp_ub / (tp_ub + fp_ub)
  # recc_ub = tp_ub / (tp_ub + fn_ub)
  # 
  # pr[i,3] = prec_ub
  # re[i,3] = recc_ub
  
  
};rm(i)
colnames(pr) <- c("Pr", "Pr_lb", "Pr_ub")
pr <- pr %>% mutate_all(., list(function(x){case_when(is.nan(x) & .$Pr == 1 ~ 1,
                                                      is.nan(x) & .$Pr == 0 ~ 0, 
                                                      TRUE ~ x)}))
colnames(re) <- c("Re")
# re <- re %>% mutate_all(., list(function(x){case_when(is.nan(x) & .$Re == 1 ~ 1,
#                                                       is.nan(x) & .$Re == 0 ~ 0, 
#                                                       TRUE ~ x)}))

pltdat = cbind(re,pr) %>% mutate("GROUP" = "ICEWS")
prc = ggplot(data = pltdat) +
  geom_ribbon(aes(x = Re, ymin = Pr_lb, ymax = Pr_ub, fill = GROUP), alpha = .5) +
  geom_line(aes(x = Re, y = Pr, color = GROUP), size = 0.25) +
  labs(title = "Spatial Probit Error Models, Precision-Recall Curves",
       x     = "Recall",
       y     = "Precision") + 
  scale_color_manual("GROUP", values = c('black','black')) +
  scale_y_continuous(labels = scales::percent_format()) + 
  scale_x_continuous(labels = scales::percent_format()) + 
  my_theme 

# https://stats.stackexchange.com/questions/363382/confidence-interval-of-precision-recall-and-f1-score

# plot(x = unlist(re), y = unlist(pr), type = 'l', xlim = c(0,1), ylim = c(0,1))

ggsave(filename = "Paper Drafts/Draft_20200819/PRC-Discrete.png",
       plot     = prc,
       width    = 8,
       height   = 6,
       dpi      = 320)



















#-----------------------------------------------------------------------------#
# ----------------------------------- #

#-----------------------------------------------------------------------------#
# OBSERVED FARC EVENTS
# 
# roc_dat <- data.frame('obs'        = colombia$cinep_farc_bin,
#                       'pred.icews' = se_events_probs$icews_farc_bin.SEM.prob,
#                       'pred.ged'   = se_events_probs$ged_farc_bin.SEM.prob)
# 
# roc_plot <- ggplot(data = roc_dat) + 
#   geom_roc(aes(d = obs, m = pred.icews, color = 'ICEWS'),  labels = FALSE, size = 1.25) + 
#   geom_roc(aes(d = obs, m = pred.ged,   color = 'GED'),  labels = FALSE, size = 1.25) + 
#   scale_x_continuous(name   = 'False Positive Rate',
#                      labels = scales::percent) + 
#   scale_y_continuous(name   = 'True Positive Rate',
#                      labels = scales::percent) + 
#   geom_abline(intercept = 0, slope = 1, linetype = 'dashed', colour = 'gray80') +
#   my_theme + 
#   labs(title    = 'Receiver Operator Curves: ICEWS and GED',
#        subtitle = 'Treating CINEP observed events as true values against respective SPE model predicted values')
# 
# roc_plot
#-----------------------------------------------------------------------------#

# Original filename: 'Plots/ROC-Discrete.png'

ggsave(filename = "Paper Drafts/Draft_20200807/ROC-Discrete.png",
       plot     = roc_plot,
       width    = 8,
       height   = 6,
       dpi      = 320)


# Area under the curve calculation for these data:
icews_auc <- auc(roc(roc_dat$obs, roc_dat$pred.icews))
ged_auc   <- auc(roc(roc_dat$obs, roc_dat$pred.ged))

icews_auc;ci(icews_auc)
ged_auc;ci(ged_auc)







roc_plot_ci <- ggplot(data = data.frame('obs'          = colombia$cinep_farc_bin,
                                     'pred.icews'   = se_events_probs$icews_farc_bin.SEM.prob$point,
                                     'pred.icews.l' = se_events_probs$icews_farc_bin.SEM.prob$lci,
                                     'pred.icews.u' = se_events_probs$icews_farc_bin.SEM.prob$uci,
                                     'pred.ged'     = se_events_probs$ged_farc_bin.SEM.prob$point,
                                     'pred.ged.l'   = se_events_probs$ged_farc_bin.SEM.prob$lci,
                                     'pred.ged.u'   = se_events_probs$ged_farc_bin.SEM.prob$uci)) + 
  geom_roc(aes(d = obs, m = pred.icews, color = 'ICEWS'),  labels = FALSE, size = 1.25) + 
  geom_roc(aes(d = obs, m = pred.icews.l, color = 'ICEWS - LCI'), labels = FALSE, size = 1.25, linetype = 'dashed')
  geom_roc(aes(d = obs, m = pred.ged,   color = 'GED'),  labels = FALSE, size = 1.25) + 
  scale_x_continuous(name   = 'False Positive Rate',
                     labels = scales::percent) + 
  scale_y_continuous(name   = 'True Positive Rate',
                     labels = scales::percent) + 
  logan_theme + 
  labs(title    = 'Receiver Operator Curves: ICEWS and GED',
       subtitle = 'Treating CINEP observed events as true values against respective SPE model predicted values')

# ----------------------------------- #



# SAVE ------------------------------------------------------------------------
# save()
# rm(list = ls())

