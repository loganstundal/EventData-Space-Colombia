#-----------------------------------------------------------------------------#
#                                                                             
# Author:        Logan Stundal                                                    
# Date:          March 25, 2020                                                 
# Purpose:       EDA for Event Data Project, raw data plots                      
#                                                                             
#
# Copyright (c): Logan Stundal, 2020                      
# Email:         stund005@umn.edu
#
#-----------------------------------------------------------------------------# 
#
# Notes:                                                                    
#    20200525 - Updated script to produce raw data maps consistent with SPE and SPDE predicted
#               probability maps
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

#---------------------------#
# Set working directory     
#---------------------------#
setwd('c:/users/logan/googledrive/umn/research/ra_john/event_data_project')

#---------------------------#
# Load data                 
#---------------------------#
load('data/colombia.rdata')
rm(list = setdiff(ls(), 'colombia'))

#---------------------------#
# Load functions            
#---------------------------#
source('scripts/0.0-Functions_EDP.R')


# RAW DATA MAPS ---------------------------------------------------------------
# cols = c(viridis::viridis(9)[1],viridis::viridis(9)[5],viridis::viridis(9)[9])
# cols = c('gray95','gray50')
cols = c("gray90",viridis::magma(n = 8)[2])

{bogota <- data.frame('x' = -74.064559, 
                     'y' = 4.709534)
bogota <- st_as_sf(bogota, coords = c('x','y'), crs = '+proj=longlat +datum=WGS84')
bogota <- st_transform(bogota, crs = st_crs(colombia))}

raw_dvs <- list('ICEWS' = 'icews_farc_bin','GED' = 'ged_farc_bin','CINEP' = 'cinep_farc_bin')

# With Bogota marked (nb., john email March 23)
raw_maps <- lapply(1:length(raw_dvs), function(x){
  var = raw_dvs[[x]]
  
  ggplot(data = colombia %>% mutate(tmp = as_factor(colombia[[var]]))) +
    geom_sf(aes(fill = tmp), size = 0.01, alpha = 0.9, color = 'transparent') +
    geom_sf(data = bogota, aes(shape = 'Bogota'), size = 2, fill = 'black',colour = 'white',
            show.legend = 'point') +
    scale_fill_manual(name   = '',
                      values = cols,
                      guide  = guide_legend(override.aes = list(shape = NA),
                                            keyheight    = unit(0.15, 'in'),
                                            keywidth     = unit(0.15, 'in')),
                      labels = c('No Event','Event')) +
    scale_shape_manual(name  = '',
                       values = 24) +
    theme_minimal() +
    theme(axis.text  = element_blank(),
          axis.title = element_blank(),
          panel.grid = element_blank(),
          plot.title = element_text(hjust = 0.5),
          plot.margin=unit(c(t = 0, l = 0, b = -1, r = 0),"cm"),
          legend.position  = 'right',
          legend.direction = 'vertical',
          legend.margin = margin(c(-15, 5, 0, 10)),
          legend.text = element_text(margin = margin(r = 5, unit = "pt"),
                                     size = 10)) 
})

raw_maps[[1]] = raw_maps[[1]] + theme(legend.position = 'none')
raw_maps[[2]] = raw_maps[[2]] + theme(legend.position = 'none')
legend        = get_legend(raw_maps[[3]])
raw_maps[[3]] = raw_maps[[3]] + theme(legend.position = 'none')

observed_maps  <- plot_grid(plotlist = list(raw_maps[[1]],
                                            raw_maps[[2]],
                                            raw_maps[[3]],
                                            legend),
                        ncol       = 4, 
                        rel_widths = c(.27, .27, .27, 0.19)) +
  draw_label(label = paste('Reported FARC events by source'),
             x = 0.05, y = 0.90,
             hjust = 0, vjust = 0,
             lineheight = 1.0,
             size = 14) + 
  draw_text(text = c('ICEWS', 'GED', 'CINEP'),
            x    = c(.15,.45,.75), y = rep(0.85,3),
            size = 12)

observed_maps

# ----------------------------------- #
# Original file name: 'Plots/Maps-Discrete/Map-Observed.png'

# Changed on 2020-08-07

ggsave(filename = "Paper Drafts/Draft_20200807/Map-Observed.png",
       plot     = observed_maps,
       width    = 6.5,
       height   = 3.0,
       dpi      = 320)

# ----------------------------------- #






raw_maps <- do.call(gridExtra::grid.arrange, c(raw_maps, ncol = length(raw_dvs)))
ggsave(filename = 'paper drafts/memo_20200325/plots_20200325/RawData_ObservedEvents.png',
       plot     = raw_maps,
       width    = 14,
       height   = 8,
       dpi      = 320)

# UNDERREPORTING MAPS ---------------------------------------------------------
under_dvs <- list('ICEWS' = 'icews_cinep_under','GED' = 'ged_cinep_under')

# With Bogota marked (nb., john email March 23)
under_maps <- lapply(1:length(under_dvs), function(x){
  var = under_dvs[[x]]
  
  ggplot(data = colombia %>% mutate(tmp = as_factor(colombia[[var]]))) +
    geom_sf(aes(fill = tmp), size = 0.01, alpha = 0.9) +
    geom_sf(data = bogota, aes(shape = 'Bogota'), size = 3, fill = cols[3],
            show.legend = 'point') +
    scale_fill_manual(name   = '',
                      values = cols,
                      guide  = guide_legend(override.aes = list(linetype = "blank", shape = NA)),
                      labels = c('Agreement','Underreporting')) +
    scale_shape_manual(name  = '',
                       values = 24) +
    map_theme + 
    labs(title = sprintf('%s: Underreporting', names(under_dvs)[x])
         # caption = sprintf('Plot date: %s.', format(Sys.Date(), "%B %d, %Y"))
         )
})
under_maps <- do.call(gridExtra::grid.arrange, c(under_maps, ncol = length(under_dvs)))
ggsave(filename = 'paper drafts/memo_20200325/plots_20200325/RawData_Underreporting.png',
       plot     = under_maps,
       width    = 14,
       height   = 8,
       dpi      = 320)


# CONFUSION MATRICES ----------------------------------------------------------
# Kappa statistics
d <- sapply(c('icews_farc_bin','ged_farc_bin'), function(x){
  kappa_cm(observed = colombia$cinep_farc_bin, 
           p_bin    = colombia[[x]])
}) %>% t %>% apply(., 2, as.numeric) %>% as.data.frame


kappa_plot <- {ggplot(data = d, aes(x = c(0,0))) + 
  geom_point(aes(y = Kappa, colour = c('ICEWS','GED'))) +
  geom_errorbar(aes(ymin = Kappa_lci,
                    ymax = Kappa_uci,
                    colour = c('ICEWS','GED')),
                width = 0.10) + 
  geom_hline(aes(yintercept = 0), linetype = 'solid', size = 1, color = 'gray70') +
  scale_x_continuous(name   = '',
                     limits = c(-0.25, 0.25)) + 
  scale_y_continuous(name   = '',
                     limits = c(-0.01, 0.5)) +
  # scale_color_manual(values = c('ICEWS' = cols[1],
  #                               'GED'   = cols[2])) +
  # coord_equal(expand = FALSE) +
  theme_minimal() + 
  theme(panel.grid.minor   = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.title       = element_blank(),
        axis.ticks.x       = element_blank(),
        axis.text.x        = element_blank(),
        axis.title.x       = element_blank(),
        legend.position    = 'bottom',
        legend.direction   = 'horizontal') +
  labs(title    = 'Kappa values and 95% confidence intervals',
       subtitle = 'ICEWS and GED reliabilty compared to CINEP',
       caption  = sprintf('Plot date: %s.', format(Sys.Date(), "%B %d, %Y")))
}
ggsave(filename = 'paper drafts/memo_20200325/plots_20200325/RawData_Kappa.png',
       plot     = kappa_plot,
       width    = 5,
       height   = 7,
       dpi      = 320)

# ----------------------------------- #

# Confusion Matrices
lapply(c('icews_farc_bin','ged_farc_bin'), function(x){
  caret::confusionMatrix(as_factor(colombia$cinep_farc_bin), as_factor(colombia[[x]]))
})


# SAVE ------------------------------------------------------------------------
# save()
# rm(list = ls())