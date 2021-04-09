#-----------------------------------------------------------------------------#
#
# Author:        Logan Stundal
# Date:          April 09, 2021
# Purpose:       5.1-Results-Coef-Plots.R
#
#
# Copyright (c): Logan Stundal, 2021
# Email:         stund005@umn.edu
#
#-----------------------------------------------------------------------------#
#
# Notes:
#  Coefficient plots using INLA spde models and tidied results from
#  5.0-Results-Tables.R
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
library(cowplot)

#---------------------------#
# Load data
#---------------------------#
load("Results/Tables/tidy-mods.Rdata");rm(tab_vals)

#---------------------------#
# Local functions
#---------------------------#
custom_theme <-
  theme_minimal() +
  theme(axis.title       = element_blank(),
        panel.border     = element_rect(colour = "black", fill = NA, size = 0.3),
        panel.grid       = element_blank(),
        panel.grid.major.x = element_line(linetype = "dotted", color = "gray70", size = 0.2),
        strip.background = element_rect(fill = NA, linetype = "solid", size = 0.3),
        strip.text       = element_text(size = 10),
        legend.position  = "none")

custom_plot <- function(data){
  ggplot(data = data,
         aes(y = y)) +
    geom_point(aes(x = median, color = model), size = 0.5) +
    geom_errorbar(aes(xmin = lb, xmax = ub, color = model), width = 0.1, size = 0.2) +
    facet_wrap(~years, ncol = 3) +
    # geom_segment(aes(x = 0, y = 0.7, xend = 0, yend = 3.3),
    #              linetype = "dashed", color = "gray50", size = 0.4) +
    custom_theme
}

#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# COEFFICIENT PLOTS CONSTRUCTION                                          ----
#-----------------------------------------------------------------------------#

# ----------------------------------- #
# Tidy data for plotting
# ----------------------------------- #
g <- tidy_vals %>% filter(!str_detect(model, pattern = "_under"),
                          variable != "intercept") %>%
  dplyr::select(variable:years) %>%
  mutate(type = case_when(variable %in% c("Kappa","Sigma","Range") ~ variable,
                          TRUE ~ "Structural"),
         y = case_when(variable == "dist"      ~ 1,
                       variable == "pop"       ~ 2,
                       variable == "tri"       ~ 3,
                       # variable == "intercept" ~ 1,
                       TRUE                    ~ 1)) %>%
  mutate(type = factor(type, levels = c("Structural","Kappa","Sigma","Range")),
         y = case_when(model == "icews_bin" ~ y - 0.2,
                       model == "ged_bin"   ~ y + 0.2,
                       TRUE ~ y),
         model = case_when(str_detect(model, "cinep") ~ "CINEP",
                           str_detect(model, "icews") ~ "ICEWS",
                           str_detect(model, "ged")   ~ "GED")) %>%
  mutate(model = factor(model, levels = c("CINEP","ICEWS","GED")))
# ----------------------------------- #


# ----------------------------------- #
# Structural parameters
# ----------------------------------- #
structural <- custom_plot(data = g %>% filter(type == "Structural")) +
  scale_y_continuous(breaks = 1:3,
                     labels = c("Dist. Bogota, km (log)", "Population (log)", "TRI"))
# ----------------------------------- #


# ----------------------------------- #
# SPDE parameters
# ----------------------------------- #
# SIGMA
spde_sigma <- custom_plot(data = g %>% filter(variable == "Sigma")) +
  scale_y_continuous(breaks = 1, labels = "Sigma") +
  theme(strip.background = element_blank(),
        strip.text.x     = element_blank())

# KAPPA
spde_kappa <- custom_plot(data = g %>% filter(variable == "Kappa")) +
  scale_y_continuous(breaks = 1, labels = "Kappa") +
  theme(strip.background = element_blank(),
        strip.text.x     = element_blank())


# RANGE
spde_range <- custom_plot(data = g %>% filter(variable == "Range")) +
  scale_y_continuous(breaks = 1, labels = "Range") +
  theme(strip.background = element_blank(),
        strip.text.x     = element_blank(),
        legend.position  = "bottom",
        legend.direction = "horizontal",
        legend.title     = element_blank())


# LEGEND (extract from Range and update range to no legend)
leg        <- get_legend(spde_range)
spde_range <- spde_range + theme(legend.position = "none")
# ----------------------------------- #
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# CONSTRUCT FULL PLOT                                                     ----
#-----------------------------------------------------------------------------#

final_plot <- plot_grid(structural, spde_sigma, spde_kappa, spde_range,leg,
                        rel_heights = c(3, 1, 1, 1, .3),
                        nrow  = 5,
                        align = "v",
                        axis  = "l")
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# SAVE                                                                    ----
#-----------------------------------------------------------------------------#
# ggsave(filename = "Results/Plots/coef-plot.png",
#        width    = 6.5,
#        height   = 6.5,
#        units    = "in",
#        dpi      = 380)
#
# save(final_plot, file = "Results/Plots/coef-plot.Rdata")
#
# rm(list = ls())
#-----------------------------------------------------------------------------#























