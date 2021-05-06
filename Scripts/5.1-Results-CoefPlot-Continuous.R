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
load("Results/Tables/tidy-mods.Rdata")

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
    geom_point(aes(x = x, color = model), size = 0.5) +
    geom_errorbar(aes(xmin = lb, xmax = ub, color = model), width = 0.1, size = 0.2) +
    scale_color_manual(values = {{model_colors}}) +
    facet_wrap(~years, ncol = 3) +
    # geom_segment(aes(x = 0, y = 0.7, xend = 0, yend = 3.3),
    #              linetype = "dashed", color = "gray50", size = 0.4) +
    custom_theme
}
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# COEFFICIENT PLOTS DATA                                                  ----
#-----------------------------------------------------------------------------#

# ----------------------------------- #
# Tidy data for plotting
# ----------------------------------- #
g <- tidy_vals %>%
  filter(variable != "intercept") %>%
  dplyr::select(variable:years) %>%
  mutate(type_var = case_when(variable %in% c("Kappa","Sigma","Range") ~ variable,
                              TRUE                                     ~ "Structural"),
         type_mod = case_when(str_detect(model, "_bin") ~ "Event",
                              TRUE                      ~ "Under"),
         model = case_when(str_starts(model, "cinep") ~ "CINEP",
                           str_starts(model, "icews") ~ "ICEWS",
                           str_starts(model, "ged")   ~ "GED"),
         y = case_when(variable == "dist"      ~ 1,
                       variable == "pop"       ~ 2,
                       variable == "tri"       ~ 3,
                       TRUE                    ~ 1)) %>%
  mutate(type_var = factor(type_var, levels = c("Structural","Kappa","Sigma","Range")),
         y = case_when(str_detect(model, "ICEWS") ~ y - 0.2,
                       str_detect(model, "GED")   ~ y + 0.2,
                       TRUE ~ y),
         model = factor(model, levels = c("CINEP","ICEWS","GED")),
         years = factor(years, levels = c("2002-2009", "2002-2004", "2005-2007", "2008-2009")))
# ----------------------------------- #


# ----------------------------------- #
# TIDY g
# ----------------------------------- #
g %<>% filter(years != "2008-2009") %>% mutate(years = fct_drop(years))
# ----------------------------------- #


# ----------------------------------- #
# Set "x" - to centrality measure (cent) for plotting
# ----------------------------------- #
g %<>% mutate(x = !!sym(cent))
# ----------------------------------- #


#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# CONSTRUCT PLOTS - OBSERVED EVENTS                                       ----
#-----------------------------------------------------------------------------#
# ----------------------------------- #
# Structural parameters
# ----------------------------------- #
event_structural <- custom_plot(data = g %>% filter(type_var == "Structural",
                                                    type_mod == "Event")) +
  scale_y_continuous(breaks = 1:3,
                     labels = c("Dist. Bogota, km (log)", "Population (log)", "TRI"))
# ----------------------------------- #


# ----------------------------------- #
# SPDE parameters
# ----------------------------------- #
# KAPPA
event_spde_kappa <- custom_plot(data = g %>% filter(type_var == "Kappa",
                                                    type_mod == "Event")) +
  scale_y_continuous(breaks = 1, labels = "Kappa") +
  theme(strip.background = element_blank(),
        strip.text.x     = element_blank())

# SIGMA
event_spde_sigma <- custom_plot(data = g %>% filter(type_var == "Sigma",
                                                    type_mod == "Event")) +
  scale_y_continuous(breaks = 1, labels = "Sigma") +
  theme(strip.background = element_blank(),
        strip.text.x     = element_blank())

# RANGE
event_spde_range <- custom_plot(data = g %>% filter(type_var == "Range",
                                                    type_mod == "Event")) +
  scale_y_continuous(breaks = 1, labels = "Range") +
  theme(strip.background = element_blank(),
        strip.text.x     = element_blank(),
        legend.position  = "bottom",
        legend.direction = "horizontal",
        legend.title     = element_blank())

# LEGEND (extract from Range and update range to no legend)
leg              <- get_legend(event_spde_range)
event_spde_range <- event_spde_range + theme(legend.position = "none")
# ----------------------------------- #


# ----------------------------------- #
# FULL PLOT
# ----------------------------------- #
event_final_plot <- plot_grid(event_structural,
                              event_spde_kappa,
                              event_spde_sigma,
                              event_spde_range,leg,
                              rel_heights = c(3, 1, 1, 1, .3),
                              nrow  = 5,
                              align = "v",
                              axis  = "l")
# ----------------------------------- #
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# CONSTRUCT PLOTS - UNDERREPORTING EVENTS                                 ----
#-----------------------------------------------------------------------------#
# ----------------------------------- #
# Structural parameters
# ----------------------------------- #
under_structural <- custom_plot(data = g %>% filter(type_var == "Structural",
                                                    type_mod == "Under")) +
  scale_y_continuous(breaks = 1:3,
                     labels = c("Dist. Bogota, km (log)", "Population (log)", "TRI"))
# ----------------------------------- #


# ----------------------------------- #
# SPDE parameters
# ----------------------------------- #
# KAPPA
under_spde_kappa <- custom_plot(data = g %>% filter(type_var == "Kappa",
                                                    type_mod == "Under")) +
  scale_y_continuous(breaks = 1, labels = "Kappa") +
  theme(strip.background = element_blank(),
        strip.text.x     = element_blank())

# SIGMA
under_spde_sigma <- custom_plot(data = g %>% filter(type_var == "Sigma",
                                                    type_mod == "Under")) +
  scale_y_continuous(breaks = 1, labels = "Sigma") +
  theme(strip.background = element_blank(),
        strip.text.x     = element_blank())

# RANGE
under_spde_range <- custom_plot(data = g %>% filter(type_var == "Range",
                                                    type_mod == "Under")) +
  scale_y_continuous(breaks = 1, labels = "Range") +
  theme(strip.background = element_blank(),
        strip.text.x     = element_blank(),
        legend.position  = "bottom",
        legend.direction = "horizontal",
        legend.title     = element_blank())

# LEGEND (extract from Range and update range to no legend)
leg              <- get_legend(under_spde_range)
under_spde_range <- under_spde_range + theme(legend.position = "none")
# ----------------------------------- #


# ----------------------------------- #
# FULL PLOT
# ----------------------------------- #
under_final_plot <- plot_grid(under_structural,
                              under_spde_kappa,
                              under_spde_sigma,
                              under_spde_range,leg,
                              rel_heights = c(3, 1, 1, 1, .3),
                              nrow  = 5,
                              align = "v",
                              axis  = "l")
# ----------------------------------- #
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# SAVE                                                                    ----
#-----------------------------------------------------------------------------#
ggsave(filename = "Results/Plots/coef-plot-event.png",
       plot     = event_final_plot,
       width    = 6.5,
       height   = 6.5,
       units    = "in",
       dpi      = 380)

ggsave(filename = "Results/Plots/coef-plot-under.png",
       plot     = under_final_plot,
       width    = 6.5,
       height   = 6.5,
       units    = "in",
       dpi      = 380)

save(event_final_plot,
     under_final_plot,
     file = "Results/Plots/coef-plot.Rdata")

rm(list = ls())
#-----------------------------------------------------------------------------#

