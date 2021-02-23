#-----------------------------------------------------------------------------#
#                                                                             
# Author:        Logan Stundal                                                    
# Date:          May 24, 2020                                                 
# Purpose:       INLA - Continuous model maps                      
#                                                                             
#
# Copyright (c): Logan Stundal, 2020                      
# Email:         stund005@umn.edu
#
#-----------------------------------------------------------------------------# 
#
# Notes:                                                                    
#                                                                             
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
library(INLA)
library(tidyverse)
library(sf)
library(cowplot)
# library(plotROC)
library(pROC)

#---------------------------#
# Set working directory     
#---------------------------#
setwd('C:/Users/logan/GoogleDrive/UMN/RESEARCH/RA_John/Event_Data_Project')

#---------------------------#
# Load data                 
#---------------------------#
load('Data/Models-Continuous.Rdata')
non_maps <- ls() # To delete model data from resulting map object file at end of script.

#---------------------------#
# Functions            
#---------------------------#
map_prep <- function(mesh, boundary, res = 100){
  meshvert       = rbind(c(mesh$loc[,1],mesh$loc[,2]))
  proj           = inla.mesh.projector(mesh, projection = "longlat", 
                                       dims = c(res,res))
  
  boundary  = st_transform(boundary, crs = "+proj=longlat +datum=WGS84")
  boundary  = as_Spatial(from = boundary, 
                         IDs  = 1:nrow(boundary))
  
  e              = expand.grid(proj$x,proj$y)
  coordinates(e) = c("Var1","Var2")
  proj4string(e) = CRS(proj4string(boundary))
  e              = e[boundary]
  ins            = as.matrix(e@coords)
  
  return(list('proj'     = proj,
              'ins'      = ins,
              'boundary' = boundary))
}

post_surf <- function(model, 
                      map_prep, 
                      type  = NULL,
                      probs = FALSE){
  require(raster)
  proj = map_prep$proj
  
  # Resolve mean predictions with mesh vertices
  xmean       = inla.mesh.project(projector = proj,
                                  field     = model$summary.random$spatial.field[[type]])
  xmean[!map_prep$ins] = NA
  
  # Extract values for the probability of FARC attack
  if(probs){
    surf  =  binomial(link='probit')$linkinv(xmean)
  } else{
    surf  =  xmean
  }
  
  # Convert probability surfaces to raster objects
  colnames(surf) = proj$x
  rownames(surf) = proj$y
  surf           = raster(x   = surf, 
                          xmn = min(proj$x),
                          xmx = max(proj$x),
                          ymn = min(proj$y),
                          ymx = max(proj$y),
                          crs = CRS(proj4string(map_prep$boundary)))
  surf = mask(surf, map_prep$boundary)
  surf = as.data.frame(surf, xy = TRUE) %>% rename(z = layer)
  
  return(surf)
}

custom_scale <- function(surfaces, 
                         n_breaks){
  the_breaks = extended_breaks(only.loose = T)(surfaces)
  the_limits = c(min(the_breaks),max(the_breaks))
  
  return(list('breaks' = the_breaks,
              'limits' = the_limits))
}

post_map <- function(map_data,
                     scale_vals,
                     boundary  = model_map_prep$boundary,
                     map_title = NULL,
                     probs     = FALSE,
                     scale     = TRUE){
  ggplot() +
    geom_raster(data     = map_data,
                aes(x    = x,
                    y    = y,
                    fill = z),
                alpha = 0.85) +
    
    # To plot Bogota:
    geom_point(aes(x = -74.06456, y = 4.709534), shape = 24, fill = 'black',color = 'white',
               size = 2) +

    labs(title = map_title) +
    scale_fill_gradientn(name     = '',
                         colours  = viridis::magma(n = 8),
                         na.value = 'transparent',
                         breaks   = if(!probs){scale_vals$breaks}else{seq(0,1,0.2)},
                         labels   = if(!probs){
                           formatC(scale_vals$breaks, digits = 2, format = 'f')
                         }else{scales::percent},
                         limits   = if(!probs){scale_vals$limits}else{c(0,1)}) +
    geom_sf(data = st_as_sf(boundary), fill = 'transparent', colour = 'transparent') +
    theme_minimal() +
    theme(axis.text  = element_blank(),
          axis.title = element_blank(),
          panel.grid = element_blank(),
          plot.title = element_text(hjust = 0.5),
          plot.margin=unit(c(t = 0, l = 0, b = -1, r = 0),"cm"),
          legend.position = if(scale == FALSE){"none"})
}

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


# Maps ------------------------------------------------------------------------

# Construct common data for all maps
model_map_prep <- map_prep(mesh = mesh, boundary = colombia0, res = 300)


# Posterior predicted probabilities
probs <- lapply(inla_mods[1:3], function(x){
  post_surf(model    = x, 
            map_prep = model_map_prep,
            type     = 'mean',
            probs    = TRUE)
})

icews_prob <- post_map(probs$icews_farc_bin, 
                       # map_title = 'ICEWS', 
                       probs     = TRUE, 
                       scale     = FALSE)
ged_prob   <- post_map(probs$ged_farc_bin, 
                       # map_title = 'GED', 
                       probs     = TRUE, 
                       scale     = FALSE)
cinep_prob <- post_map(probs$cinep_bin, 
                       # map_title = 'CINEP', 
                       probs     = TRUE, 
                       scale     = TRUE)

legend     <- get_legend(cinep_prob)
cinep_prob <- cinep_prob + theme(legend.position = 'none')

map_probs  <- plot_grid(plotlist   = list(icews_prob, ged_prob, cinep_prob, legend),
                        ncol       = 4, 
                        rel_widths = c(.3, .3, .3,0.1)) +
  draw_label(label = expression(paste('Posterior probability, ', pi['s'])),
             x = 0.05, y = 0.88,
             hjust = 0, vjust = 0,
             lineheight = 1.0,
             size = 14) + 
  draw_text(text = c('ICEWS', 'GED', 'CINEP'),
            x    = c(.15,.45,.75), y = rep(0.85,3),
            size = 12)

ggsave(plot  = map_probs, 
       file  = 'Plots/Maps-Continuous/Map-spde-Probs.png',
       dpi   = 320,
       width = 6.5,
       height= 3.0,
       units = 'in')


# ----------------------------------- #
# Posterior means
means <- lapply(inla_mods[1:3], function(x){
  post_surf(model    = x, 
            map_prep = model_map_prep,
            type     = 'mean',
            probs    = FALSE)
})

mean_scale_vals = custom_scale(as.numeric(unlist(sapply(means, '[','z'))), 8)


icews_mean <- post_map(means$icews_farc_bin, 
                       # map_title = 'ICEWS', 
                       scale_vals = mean_scale_vals,
                       scale      = FALSE)
ged_mean   <- post_map(means$ged_farc_bin, 
                       # map_title = 'GED', 
                       scale_vals = mean_scale_vals,
                       scale     = FALSE)
cinep_mean <- post_map(means$cinep_bin, 
                       # map_title = 'CINEP', 
                       scale_vals = mean_scale_vals,
                       scale     = TRUE)

legend     <- get_legend(cinep_mean)
cinep_mean <- cinep_mean + theme(legend.position = 'none')

map_means  <- plot_grid(plotlist   = list(icews_mean, ged_mean, cinep_mean, legend), 
                        ncol       = 4, 
                        rel_widths = c(.3, .3, .3,0.1)) +
  draw_label(label = expression(paste('Posterior mean ', xi['s'])),
             x = 0.05, y = 0.88,
             hjust = 0, vjust = 0,
             lineheight = 1.0,
             size = 14)

ggsave(plot  = map_means, 
       file  = 'Plots/Maps-Continuous/Map-spde-Means.png',
       dpi   = 320,
       width = 6.5,
       height= 3.0,
       units = 'in')

# ----------------------------------- #
# Posterior standard deviations
sds <- lapply(inla_mods[1:3], function(x){
  post_surf(model    = x, 
            map_prep = model_map_prep,
            type     = 'sd',
            probs    = FALSE)
})

sd_scale_vals = custom_scale(as.numeric(unlist(sapply(sds, '[','z'))), 8)

icews_sd <- post_map(sds$icews_farc_bin, 
                     # map_title = 'ICEWS', 
                     scale_vals = sd_scale_vals,
                     scale      = FALSE)
ged_sd   <- post_map(sds$ged_farc_bin, 
                     # map_title = 'GED', 
                     scale_vals = sd_scale_vals,
                     scale      = FALSE)
cinep_sd <- post_map(sds$cinep_bin, 
                     # map_title = 'CINEP', 
                     scale_vals = sd_scale_vals,
                     scale     = TRUE)

legend   <- get_legend(cinep_sd)
cinep_sd <- cinep_sd + theme(legend.position = 'none')

map_sds  <- plot_grid(plotlist   = list(icews_sd, ged_sd, cinep_sd, legend),
                      ncol       = 4, 
                      rel_widths = c(.3, .3, .3,0.1)) +
  draw_label(label = expression(paste('Posterior variance ', sigma[xi['s']]^2)),
             x = 0.05, y = 0.88,
             hjust = 0, vjust = 0,
             lineheight = 1.0,
             size = 14)

ggsave(plot  = map_sds, 
       file  = 'Plots/Maps-Continuous/Map-spde-SDs.png',
       dpi   = 320,
       width = 6.5,
       height= 3.0,
       units = 'in')



# ROC Plots -------------------------------------------------------------------

# Code added on: 2020-08-07
roc_icews <- roc(colombia$cinep_farc_bin,
                 pnorm(inla_mods$icews_farc_bin$summary.linear.predictor[1:nrow(colombia),'mean']),
                 auc = T,
                 ci  = T)
roc_icews_auc <- round(as.numeric(roc_icews$ci) * 100,2)
ci_icews  <- ci.thresholds(roc_icews, thresholds = seq(0,1,0.01))

roc_ged   <- roc(colombia$cinep_farc_bin,
                 pnorm(inla_mods$ged_farc_bin$summary.linear.predictor[1:nrow(colombia),'mean']),
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
                           roc_ged_auc[3]),
           hjust = 0) +
  my_theme


# ----------------------------------- #
# Nb - 2020-08-07 on precision-recall curves, if needed in the future:
prc_icews <- roc(colombia$cinep_farc_bin,
                 pnorm(inla_mods$icews_farc_bin$summary.linear.predictor[1:nrow(colombia),'mean'])) %>%
  coords(ret = "all", transpose = FALSE) %>%
  dplyr::select(precision, recall) %>%
  mutate(GROUP = 'ICEWS')
prc_ged <- roc(colombia$cinep_farc_bin,
               pnorm(inla_mods$ged_farc_bin$summary.linear.predictor[1:nrow(colombia),'mean'])) %>%
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
    labs(title = "SPDE Models, Precision-Recall Curves")

ggsave(filename = "Paper Drafts/Draft_20200807/PRC-Continuous.png",
       plot     = prc,
       width    = 8,
       height   = 6,
       dpi      = 320)
# ----------------------------------- #
# ----------------------------------- #

#-----------------------------------------------------------------------------#
# Old way of doing this - overwritten with above code
# roc_dat <- data.frame('obs'        = colombia$cinep_farc_bin,
#                       'pred.icews' = pnorm(inla_mods$icews_farc_bin$summary.linear.predictor[1:nrow(colombia),'mean']),
#                       'pred.ged'   = pnorm(inla_mods$ged_farc_bin$summary.linear.predictor[1:nrow(colombia),'mean']))
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
#        subtitle = 'Treating CINEP observed events as true values against respective SPDE model predicted values')
# roc_plot
#-----------------------------------------------------------------------------#

# original file-name 'Plots/ROC-Continuous.png'

ggsave(filename = "Paper Drafts/Draft_20200807/ROC-Continuous.png",
       plot     = roc_plot,
       width    = 8,
       height   = 6,
       dpi      = 320)


# Area under the curve calculation for these data:
icews_auc <- auc(roc(roc_dat$obs, roc_dat$pred.icews))
ged_auc   <- auc(roc(roc_dat$obs, roc_dat$pred.ged))

icews_auc;ci(icews_auc)
ged_auc;ci(ged_auc)


#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

# PRECISION RECALL CURVES

thresh = seq(0,1,length.out = 200)
tr     = colombia$cinep_farc_bin
n      = length(tr)
pr = data.frame()
# re = data.frame()


# Initialize row counter to store estimates - super ugly fix
r = 0

for(g in c("icews", "ged")){
  var = sprintf("%s_farc_bin", g)
  for (i in 1:length(thresh)) {
    
    r = r + 1
    
    z      = ifelse(pnorm(inla_mods[[var]]$summary.linear.predictor[1:nrow(colombia),'mean']) >= thresh[i], 1, 0)
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
    
    
    pr[r,1] = recc
    pr[r,2] = prec
    pr[r,3] = prec_lb
    pr[r,4] = prec_ub
    pr[r,5] = stringr::str_to_upper(g)
    
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
    
    
  }
};rm(i)



colnames(pr) <- c("Re", "Pr", "Pr_lb", "Pr_ub", "GROUP")
pr <- pr %>% mutate_at(c("Pr_lb", "Pr_ub"), list(function(x){case_when(is.nan(x) & .$Pr == 1 ~ 1,
                                                             is.nan(x) & .$Pr == 0 ~ 0, 
                                                             TRUE ~ x)}))
pltdat = pr


prc = ggplot(data = pltdat) +
  geom_ribbon(aes(x = Re, ymin = Pr_lb, ymax = Pr_ub, fill = GROUP), alpha = .5) +
  geom_line(aes(x = Re, y = Pr, color = GROUP), size = 0.25) +
  labs(title = "SPDE Models, Precision-Recall Curves",
       x     = "Recall",
       y     = "Precision") + 
  scale_color_manual("GROUP", values = c('black','black')) +
  scale_y_continuous(labels = scales::percent_format()) + 
  scale_x_continuous(labels = scales::percent_format()) + 
  my_theme 

# https://stats.stackexchange.com/questions/363382/confidence-interval-of-precision-recall-and-f1-score

# plot(x = unlist(re), y = unlist(pr), type = 'l', xlim = c(0,1), ylim = c(0,1))

ggsave(filename = "Paper Drafts/Draft_20200819/PRC-Continuous.png",
       plot     = prc,
       width    = 8,
       height   = 6,
       dpi      = 320)




#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#





# SAVE ------------------------------------------------------------------------
# save()
# rm(list = ls())



